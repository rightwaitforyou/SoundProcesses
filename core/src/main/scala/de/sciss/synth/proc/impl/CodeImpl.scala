/*
 *  CodeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}

import de.sciss.processor.ProcessorLike
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise, blocking}
import scala.util.{Failure, Success}

object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    import java.util.jar._

    import scala.annotation.tailrec

    val in = new JarInputStream(new ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    @tailrec def loop(): Unit = {
      val entry = in.getNextJarEntry
      if (entry != null) {
        if (!entry.isDirectory) {
          val name  = entry.getName

          // cf. http://stackoverflow.com/questions/8909743/jarentry-getsize-is-returning-1-when-the-jar-files-is-opened-as-inputstream-f
          val bs  = new ByteArrayOutputStream
          var i   = 0
          while (i >= 0) {
            i = in.read()
            if (i >= 0) bs.write(i)
          }
          val bytes = bs.toByteArray
          b += mkClassName(name) -> bytes
        }
        loop()
      }
    }
    loop()
    in.close()
    b.result()
  }

  /* Converts a jar entry name with slashes to a class name with dots
   * and dropping the `class` extension
   */
  private def mkClassName(path: String): String = {
    require(path.endsWith(".class"))
    path.substring(0, path.length - 6).replace("/", ".")
  }

  implicit object serializer extends ImmutableSerializer[Code] {
    def write(v: Code, out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeInt(v.id)
      out.writeUTF(v.source)
    }

    def read(in: DataInput): Code = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val id      = in.readInt()
      val source  = in.readUTF()
      Code.apply(id, source)
    }
  }

  //  // note: the Scala compiler is _not_ reentrant!!
  //  private implicit val executionContext: ExecutionContextExecutor =
  //    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  def future[A](fun: => A)(implicit compiler: Code.Compiler): Future[A] =
    concurrent.Future(fun)(compiler.executionContext)

  // ---- elem ----

//  object ElemImpl extends proc.impl.ElemCompanionImpl[Code.Elem] {
//    def typeID = Code.typeID // 0x20001
//
//    // Elem.registerExtension(this)
//
//    def apply[S <: Sys[S]](peer: Expr[S, Code])(implicit tx: S#Tx): Code.Elem[S] = {
//      val targets = evt.Targets[S]
//      new Impl[S](targets, peer)
//    }
//
//    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Code.Elem[S] =
//      serializer[S].read(in, access)
//
//    // ---- Elem.Extension ----
//
//    /** Read identified active element */
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): Code.Elem[S] with evt.Node[S] = {
//      val peer = Code.Expr.read(in, access)
//      new Impl[S](targets, peer)
//    }
//
//    /** Read identified constant element */
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Code.Elem[S] =
//      sys.error("Constant Code not supported")
//
//    // ---- implementation ----
//
//    private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
//                                          val peer: Expr[S, Code])
//      extends Code.Elem[S]
//      with proc.impl.ActiveElemImpl[S] {
//
//      def typeID = ElemImpl.typeID
//      def prefix = "Code"
//
//      override def toString: String = s"$prefix$id"
//
//      def mkCopy()(implicit tx: S#Tx): Code.Elem[S] = {
//        val cpy = peer match {
//          case Expr.Var(vr) => Code.Expr.newVar(vr())
//          case other => other
//        }
//        Code.Elem(cpy)
//      }
//    }
//  }

  // ---- imports ----

  private val sync = new AnyRef

  private var importsMap = Map[Int, Vec[String]](
    Code.FileTransform.id -> Vec(
      "de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat, Frames}",
      "de.sciss.synth._",
      "de.sciss.file._",
      "de.sciss.fscape.{FScapeJobs => fscape}",
      "de.sciss.mellite.TransformUtil._"  // XXX TODO - should not refer to Mellite packages
    ),
    Code.SynthGraph.id -> Vec(
      "de.sciss.synth.{Buffer => _, _}",
      "de.sciss.synth.ugen.{DiskIn => _, VDiskIn => _, BufChannels => _, BufRateScale => _, BufSampleRate => _, _}",
      "de.sciss.synth.proc.graph._"
    ),
    Code.Action.id -> Vec(     // what should go inside?
      "de.sciss.synth.proc._",
      "de.sciss.synth.proc.Implicits._",
      "de.sciss.lucre.expr.{Boolean => BooleanEx, Int => IntEx, Double => DoubleEx, String => StringEx}",
      "de.sciss.lucre.expr.Expr"
    )
  )

  def registerImports(id: Int, imports: Seq[String]): Unit = sync.synchronized {
    importsMap += id -> (importsMap(id) ++ imports)
  }

  def getImports(id: Int): Vec[String] = importsMap(id)

  // ---- internals ----

  object Wrapper {
    implicit object FileTransform
      extends Wrapper[(File, File, ProcessorLike[Any, Any] => Unit), Future[Unit], Code.FileTransform] {

      def id = Code.FileTransform.id

      def binding: Option[String] = Some("FileTransformContext")

      def wrap(args: (File, File, ProcessorLike[Any, Any] => Unit))(fun: => Any): Future[Unit] = {
        val (in, out, procHandler) = args
        val proc = new FileTransformContext(in, out, () => fun)
        procHandler(proc)
        import ExecutionContext.Implicits.global  // XXX TODO - good?
        proc.start()
        proc
      }

      def blockTag = "Unit" // = typeTag[Unit]
      //      def inTag     = typeTag[File]
      //      def outTag    = typeTag[File]
    }

    implicit object SynthGraph
      extends Wrapper[Unit, synth.SynthGraph, Code.SynthGraph] {

      def id = Code.SynthGraph.id

      def binding = None

      def wrap(in: Unit)(fun: => Any): synth.SynthGraph = synth.SynthGraph(fun)

      def blockTag = "Unit" // typeTag[Unit]
      //      def inTag     = typeTag[Unit]
      //      def outTag    = typeTag[synth.SynthGraph]
    }

    //    implicit object Action
    //      extends Wrapper[Unit, Unit, Code.Action] {
    //
    //      def id = Code.Action.id
    //
    //      def binding = None
    //
    //      def wrap(in: Unit)(fun: => Any): Unit = fun
    //
    //      def blockTag  = typeTag[Unit]
    ////      def inTag     = typeTag[Unit]
    ////      def outTag    = typeTag[Unit]
    //    }
  }
  trait Wrapper[In, Out, Repr] {
    protected def id: Int

    final def imports: ISeq[String] = importsMap(id)
    def binding: Option[String]

    /** When `execute` is called, the result of executing the compiled code
      * is passed into this function.
      *
      * @param in   the code type's input
      * @param fun  the thunk that executes the code
      * @return     the result of `fun` wrapped into type `Out`
      */
    def wrap(in: In)(fun: => Any): Out

    /** TypeTag of */
    def blockTag: String // TypeTag[_]
    //    def inTag   : TypeTag[In]
    //    def outTag  : TypeTag[Out]
  }

  final def execute[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr, in: I)
                                                                     (implicit w: Wrapper[I, O, Repr],
                                                                      compiler: Code.Compiler): O = {
    w.wrap(in) {
      compileThunk(code.source, w, execute = true)
    }
  }

  def compileBody[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr)
                                                                   (implicit w: Wrapper[I, O, Repr],
                                                                    compiler: Code.Compiler): Future[Unit] =
    future {
      blocking {
        compileThunk(code.source, w, execute = false)
      }
    }

  /** Compiles a source code consisting of a body which is wrapped in a `Function0` apply method,
    * and returns the function's class name (without package) and the raw jar file produced in the compilation.
    */
  def compileToFunction(name: String, code: Code.Action)(implicit compiler: Code.Compiler): Array[Byte] = {

    val imports = getImports(Code.Action.id)
    val impS  = /* w. */imports.map(i => s"  import $i\n").mkString
    //        val bindS = w.binding.fold("")(i =>
    //          s"""  val __context__ = $pkg.$i.__context__
    //             |  import __context__._
    //             |""".stripMargin)
    // val aTpe  = w.blockTag.tpe.toString
    val source =
      s"""package ${Code.UserPackage}
         |
         |final class $name extends $pkgAction.Body {
         |  def apply[S <: $pkgSys.Sys[S]](universe: $pkgAction.Universe[S])(implicit tx: S#Tx): Unit = {
         |    import universe._
         |$impS
         |${code.source}
         |  }
         |}
         |""".stripMargin

    // println(source)

    compiler.compile(source)
  }

  object Run {
    def apply[A](execute: Boolean)(thunk: => A): A = if (execute) thunk else null.asInstanceOf[A]
  }

  sealed trait Context[A] {
    def __context__(): A
  }

  object FileTransformContext extends Context[FileTransformContext#Bindings] {
    private[CodeImpl] val contextVar = new ThreadLocal[FileTransformContext#Bindings]
    def __context__(): FileTransformContext#Bindings = contextVar.get()
  }

  final class FileTransformContext(in: File, out: File, fun: () => Any)
    extends ProcessorImpl[Unit, FileTransformContext] {
    process =>

    type Bindings = Bindings.type
    object Bindings {
      def in : File = process.in
      def out: File = process.out
      def checkAborted(): Unit = process.checkAborted()
      def progress(f: Double): Unit = {
        process.progress = f
        process.checkAborted()
      }
    }

    protected def body(): Unit = {
      // blocking {
      val prom  = Promise[Unit]()
      val t = new Thread {
        override def run(): Unit = {
          FileTransformContext.contextVar.set(Bindings)
          try {
            fun()
            prom.complete(Success {})
          } catch {
            case e: Exception =>
              e.printStackTrace()
              prom.complete(Failure(e))
          }
        }
      }
      t.start()
      Await.result(prom.future, Duration.Inf)
      // }
    }
  }

  private val pkgAction = "de.sciss.synth.proc.Action"
  private val pkgCode   = "de.sciss.synth.proc.impl.CodeImpl"
  private val pkgSys    = "de.sciss.lucre.event"

  // note: synchronous
  private def compileThunk(code: String, w: Wrapper[_, _, _], execute: Boolean)(implicit compiler: Code.Compiler): Any = {
    val impS  = w.imports.map(i => s"  import $i\n").mkString
    val bindS = w.binding.fold("")(i =>
      s"""  val __context__ = $pkgCode.$i.__context__
        |  import __context__._
        |""".stripMargin)
    val aTpe  = w.blockTag // .tpe.toString
    val synth =
      s"""$pkgCode.Run[$aTpe]($execute) {
        |$impS
        |$bindS
        |
        |""".stripMargin + code + "\n}"

    compiler.interpret(synth, execute = execute && w.blockTag != "Unit")
  }
}