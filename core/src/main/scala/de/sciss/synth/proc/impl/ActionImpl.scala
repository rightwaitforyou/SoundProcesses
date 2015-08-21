/*
 *  ActionImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{NoSys, Sys, TxnLike, IDPeek}
import de.sciss.serial.{Serializer, DataInput, DataOutput}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable
import scala.concurrent.{Promise, Future, blocking}
import scala.concurrent.stm.{InTxn, TMap, TSet}

object ActionImpl {
  // private val count = TxnLocal(0) // to distinguish different action class-names within the same transaction

  private final val COOKIE        = 0x61637400   // "act\0"
  private final val CONST_EMPTY   = 0
  private final val CONST_JAR     = 1
  private final val CONST_VAR     = 2
  private final val CONST_BODY    = 3

  private final val DEBUG         = false

  // ---- creation ----

  def compile[S <: Sys[S]](source: Code.Action)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S],
                           compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    val id      = tx.newID()
    // val cnt     = count.getAndTransform(_ + 1)(tx.peer)
    val name    = s"Action${IDPeek(id)}"  // _$cnt
    val p       = Promise[stm.Source[S#Tx, Action[S]]]()
    val system  = tx.system
    tx.afterCommit(performCompile(p, name, source, system))
    p.future
  }

  def empty[S <: Sys[S]](implicit tx: S#Tx): Action[S] = new ConstEmptyImpl[S]

  def newVar[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Action.Var[S] = {
    val targets = evt.Targets[S]
    val peer    = tx.newVar(targets.id, init)
    new VarImpl[S](targets, peer)
  }

  def newConst[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): Action[S] =
    new ConstFunImpl(name, jar)

  private val mapPredef = TMap.empty[String, Action.Body]

  def predef[S <: Sys[S]](id: String)(implicit tx: S#Tx): Action[S] = {
    if (!mapPredef.contains(id)(tx.peer))
      throw new IllegalArgumentException(s"Predefined action '$id' is not registered")

    new ConstBodyImpl[S](id)
  }

  def registerPredef(id: String, body: Action.Body)(implicit tx: TxnLike): Unit =
    if (mapPredef.put(id, body)(tx.peer).nonEmpty)
      throw new IllegalArgumentException(s"Predefined action '$id' was already registered")

  private def classLoader[S <: Sys[S]](implicit tx: S#Tx): MemoryClassLoader = sync.synchronized {
    clMap.getOrElseUpdate(tx.system, {
      if (DEBUG) println("ActionImpl: Create new class loader")
      new MemoryClassLoader
    })
  }

  //  def execute[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): Unit = {
  //    implicit val itx = tx.peer
  //    val cl = classLoader[S]
  //    cl.add(name, jar)
  //    val fullName  = s"${Code.UserPackage}.$name"
  //    val clazz     = Class.forName(fullName, true, cl)
  //    //  println("Instantiating...")
  //    val fun = clazz.newInstance().asInstanceOf[() => Unit]
  //    fun()
  //  }

  def execute[S <: Sys[S]](universe: Action.Universe[S], name: String, jar: Array[Byte])(implicit tx: S#Tx): Unit = {
    implicit val itx = tx.peer
    val cl = classLoader[S]
    cl.add(name, jar)
    val fullName  = s"${Code.UserPackage}.$name"
    val clazz     = Class.forName(fullName, true, cl)
    //  println("Instantiating...")
    val fun = clazz.newInstance().asInstanceOf[Action.Body]
    fun(universe)
  }

  // ----

  private def performCompile[S <: Sys[S]](p: Promise[stm.Source[S#Tx, Action[S]]], name: String,
                                          source: Code.Action, system: S)
                                         (implicit cursor: stm.Cursor[S], compiler: Code.Compiler): Unit = {
    // val jarFut = source.compileToFunction(name)
    val jarFut = Code.future(blocking(source.execute(name)))

    // somehow we get problems with BDB on the compiler context.
    // for simplicity use the main SP context!

    // import compiler.executionContext
    import SoundProcesses.executionContext
    val actFut = jarFut.map { jar =>
      if (DEBUG) println(s"ActionImpl: compileToFunction completed. jar-size = ${jar.length}")
      cursor.step { implicit tx =>
        val a = newConst(name, jar)
        // Is this affected by https://github.com/Sciss/LucreConfluent/issues/6 ?
        // No, as it doesn't contain any mutable state or S#ID instances
        tx.newHandle(a)
      }
    }
    p.completeWith(actFut)
  }

  // ---- universe ----

  final class UniverseImpl[S <: Sys[S]](val self: Action[S], workspace: WorkspaceHandle[S],
                                        val invoker: Option[Obj[S]], val values: Vec[Float])
                                       (implicit val cursor: stm.Cursor[S])
    extends Action.Universe[S] {

    def root(implicit tx: S#Tx): Folder[S] = workspace.root
  }

  // ---- serialization ----

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = anySer.asInstanceOf[Ser[S]]

  def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action.Var[S]] = anyVarSer.asInstanceOf[VarSer[S]]

  private val anySer    = new Ser   [NoSys]
  private val anyVarSer = new VarSer[NoSys]

  private final class Ser[S <: Sys[S]] extends stm.Obj.Serializer[S, Action[S]] {
    def typeID: Int = Action.typeID

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Action[S] =
      (readCookieAndTpe(in): @switch) match {
        case CONST_JAR    =>
          val name    = in.readUTF()
          val jarSize = in.readInt()
          val jar     = new Array[Byte](jarSize)
          in.readFully(jar)
          // val system  = tx.system
          new ConstFunImpl[S](name, jar)

        case CONST_BODY   =>
          val id  = in.readUTF()
          new ConstBodyImpl[S](id)

        case CONST_EMPTY  => new ConstEmptyImpl[S]

        case CONST_VAR =>
          readIdentifiedVar(in, access, targets)

        case other => sys.error(s"Unexpected action cookie $other")
      }
  }

  private final class VarSer[S <: Sys[S]] extends stm.Obj.Serializer[S, Action.Var[S]] {
    def typeID: Int = Action.typeID

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Action.Var[S] =
      ActionImpl.readVar(in, access, targets)
  }

  private def readCookieAndTpe(in: DataInput): Byte = {
    val cookie = in.readInt()
    if (cookie != COOKIE)
      sys.error(s"Unexpected cookie (found ${cookie.toHexString}, expected ${COOKIE.toHexString})")
    in.readByte()
  }

  private def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): Action.Var[S] =
    readCookieAndTpe(in) /* : @switch */ match {
      case CONST_VAR  => readIdentifiedVar(in, access, targets)
      case other      => sys.error(s"Unexpected action cookie $other")
    }

  private def readIdentifiedVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): Action.Var[S] = {
    val peer = tx.readVar[Action[S]](targets.id, in)
    new VarImpl[S](targets, peer)
  }

  // ---- constant implementation ----

  private val sync = new AnyRef

  // this is why workspace should have a general caching system
  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]

  private sealed trait ConstImpl[S <: Sys[S]] extends Action[S] with evt.impl.ConstImpl[S, Unit] {
    def typeID: Int = Action.typeID
  }

  private final case class ConstBodyImpl[S <: Sys[S]](id: String)
    extends ConstImpl[S] {

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      val fun = mapPredef.getOrElse(id, sys.error(s"Predefined action '$id' not registered"))
      fun(universe)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_BODY)
      out.writeUTF(id)
    }
  }

  // XXX TODO - should be called ConstJarImpl in next major version
  private final class ConstFunImpl[S <: Sys[S]](val name: String, jar: Array[Byte])
    extends ConstImpl[S] {

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = {
      ActionImpl.execute[S](universe, name, jar)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_JAR)
      out.writeUTF(name)
      out.writeInt(jar.length)
      out.write(jar)
    }

    override def hashCode(): Int = name.hashCode

    override def equals(that: Any): Boolean = that match {
      case cf: ConstFunImpl[_] => cf.name == name
      case _ => super.equals(that)
    }
  }

  private final class ConstEmptyImpl[S <: Sys[S]] extends ConstImpl[S] {
    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = ()

    override def equals(that: Any): Boolean = that match {
      case e: ConstEmptyImpl[_] => true
      case _ => super.equals(that)
    }

    override def hashCode(): Int = 0

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_EMPTY)
    }
  }

  private final class VarImpl[S <: Sys[S]](protected val targets: evt.Targets[S], peer: S#Var[Action[S]])
    extends Action.Var[S] with evt.impl.SingleGenerator[S, Unit, Action[S]] {

    def apply()(implicit tx: S#Tx): Action[S] = peer()

    def update(value: Action[S])(implicit tx: S#Tx): Unit = {
      val old = peer()
      peer()  = value
      if (old != value) fire(())
    }

    // stupidly defined on stm.Var
    def transform(fun: Action[S] => Action[S])(implicit tx: S#Tx): Unit = update(fun(apply()))

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = peer().execute(universe)

    protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_VAR)
      peer.write(out)
    }
  }

  // ---- class loader ----

  private final class MemoryClassLoader extends ClassLoader {
    // private var map: Map[String, Array[Byte]] = Map.empty
    private val setAdded    = TSet.empty[String]
    private val mapClasses  = TMap.empty[String, Array[Byte]]

    def add(name: String, jar: Array[Byte])(implicit tx: InTxn): Unit = {
      val isNew = setAdded.add(name)
      if (DEBUG) println(s"ActionImpl: Class loader add '$name' - isNew? $isNew")
      if (isNew) {
        val entries = Code.unpackJar(jar)
        if (DEBUG) {
          entries.foreach { case (n, _) =>
            println(s"...'$n'")
          }
        }
        mapClasses ++= entries
      }
    }

    override protected def findClass(name: String): Class[_] =
      mapClasses.single.get(name).map { bytes =>
        if (DEBUG) println(s"ActionImpl: Class loader: defineClass '$name'")
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse {
        if (DEBUG) println(s"ActionImpl: Class loader: not found '$name' - calling super")
        super.findClass(name) // throws exception
      }
  }

  // ---- elem ----
//  def mkCopy()(implicit tx: S#Tx): Action.Elem[S] = {
//    val cpy = peer match {
//      case Action.Var(vr) => Action.Var(vr())
//      case other => other
//    }
//    Action.Elem(cpy)
//  }
}