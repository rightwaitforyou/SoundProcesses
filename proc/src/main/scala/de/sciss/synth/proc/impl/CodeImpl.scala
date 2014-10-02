/*
 *  CodeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import java.io.File
import de.sciss.processor.Processor
import de.sciss.synth
import de.sciss.synth.proc
import scala.concurrent.{ExecutionContext, Await, Promise, Future, blocking}
import de.sciss.processor.impl.ProcessorImpl
import collection.immutable.{Seq => ISeq}
import scala.util.{Success, Failure}
import scala.concurrent.duration.Duration
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.lucre.{event => evt}
import evt.Sys
import de.sciss.lucre.expr.Expr
import scala.collection.immutable.{IndexedSeq => Vec}

object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"

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

  object ElemImpl extends proc.impl.ElemImpl.Companion[Code.Elem] {
    def typeID = Code.typeID // 0x20001

    Elem.registerExtension(this)

    def apply[S <: Sys[S]](peer: Expr[S, Code])(implicit tx: S#Tx): Code.Elem[S] = {
      val targets = evt.Targets[S]
      new Impl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Code.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Code.Elem[S] with evt.Node[S] = {
      val peer = Code.Expr.read(in, access)
      new Impl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Code.Elem[S] =
      sys.error("Constant Code not supported")

    // ---- implementation ----

    private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                          val peer: Expr[S, Code])
      extends Code.Elem[S]
      with proc.impl.ElemImpl.Active[S] {

      def typeID = ElemImpl.typeID
      def prefix = "Code"

      override def toString() = s"$prefix$id"

      def mkCopy()(implicit tx: S#Tx): Code.Elem[S] = Code.Elem(peer)
    }
  }

  // ---- imports ----

  private val sync = new AnyRef

  private var importsMap = Map[Int, Vec[String]](
    Code.FileTransform.id -> Vec(
      "de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat, Frames}",
      "de.sciss.synth._",
      "de.sciss.file._",
      "de.sciss.fscape.{FScapeJobs => fscape}",
      "de.sciss.mellite.TransformUtil._"
    ),
    Code.SynthGraph.id -> Vec(
      "de.sciss.synth.{Buffer => _, _}",
      "de.sciss.synth.ugen.{DiskIn => _, VDiskIn => _, BufChannels => _, BufRateScale => _, BufSampleRate => _, _}",
      "de.sciss.synth.proc.graph._"
    ),
    Code.Action.id -> Vec() // what should go inside?
  )

  def registerImports(id: Int, imports: Seq[String]): Unit = sync.synchronized {
    importsMap += id -> (importsMap(id) ++ imports)
  }

  def getImports(id: Int): Vec[String] = importsMap(id)

  // ---- internals ----

  object Wrapper {
    implicit object FileTransform
      extends Wrapper[(File, File, Processor[Any, _] => Unit), Future[Unit], Code.FileTransform] {

      def id = Code.FileTransform.id

      def binding: Option[String] = Some("FileTransformContext")

      def wrap(args: (File, File, Processor[Any, _] => Unit))(fun: => Any): Future[Unit] = {
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
    // val compiler        = intp.global  // we re-use the intp compiler -- no problem, right?
    // intp.reset()
    // compiler.reporter.reset()

    // val f               = File.createTempFile("temp", ".scala")
    // val out             = new BufferedOutputStream(new FileOutputStream(f))

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
         |class $name extends Function0[Unit] {
         |  def apply(): Unit = {
         |$impS
         |${code.source}
         |  }
         |}
         |""".stripMargin

    // println(synth)

    compiler.compile(source)

    //    out.write(synth.getBytes("UTF-8"))
    //    out.flush(); out.close()
    //    val run = new compiler.Run()
    //    run.compile(List(f.getPath))
    //    f.delete()
    //
    //    if (compiler.reporter.hasErrors) throw new Code.CompilationFailed()
    //
    //    val d0    = intp.virtualDirectory // method deprecated in Scala 2.11, but necessary for Scala 2.10
    //    // intp.replOutput.dir
    //
    //    val bytes = JarUtil.pack(d0)
    //    bytes
  }

  //  def deleteDir(base: File): Unit = {
  //    base.listFiles().foreach { f =>
  //      if (f.isFile) f.delete()
  //      else deleteDir(f)
  //    }
  //    base.delete()
  //  }

  //  private final class Intp(cSet: nsc.Settings)
  //    extends IMain(cSet, new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)) {
  //
  //    override protected def parentClassLoader = CodeImpl.getClass.getClassLoader
  //  }
  //
  //  private lazy val intp = {
  //    val cSet = new nsc.Settings()
  //    cSet.classpath.value += File.pathSeparator + sys.props("java.class.path")
  //    val res = new Intp(cSet)
  //    res.initializeSynchronous()
  //    res
  //  }

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

  private val pkg = "de.sciss.synth.proc.impl.CodeImpl"

  // val UserPackage = "de.sciss.mellite.user"

  // note: synchronous
  private def compileThunk(code: String, w: Wrapper[_, _, _], execute: Boolean)(implicit compiler: Code.Compiler): Any = {
    // val i = intp

    val impS  = w.imports.map(i => s"  import $i\n").mkString
    val bindS = w.binding.fold("")(i =>
      s"""  val __context__ = $pkg.$i.__context__
        |  import __context__._
        |""".stripMargin)
    val aTpe  = w.blockTag // .tpe.toString
    val synth =
      s"""$pkg.Run[$aTpe]($execute) {
        |$impS
        |$bindS
        |
        |""".stripMargin + code + "\n}"

    //    // work-around for SI-8521 (Scala 2.11.0)
    //    def interpret(line: String) = {
    //      val th = Thread.currentThread()
    //      val cl = th.getContextClassLoader
    //      try {
    //        i.interpret(line)
    //      } finally {
    //        th.setContextClassLoader(cl)
    //      }
    //    }

    // i.reset()
    // val res = interpret(synth)
    val res = compiler.interpret(synth)
    res

    //    // commented out to chase ClassNotFoundException
    //    // i.reset()
    //    res match {
    //      case Results.Success =>
    //        if (aTpe == "Unit" || !execute) () else {
    //          val n = i.mostRecentVar
    //          i.valueOfTerm(n).getOrElse(sys.error(s"No value for term $n"))
    //        }
    //
    //      case Results.Error      => throw Code.CompilationFailed()
    //      case Results.Incomplete => throw Code.CodeIncomplete()
    //    }
  }
}