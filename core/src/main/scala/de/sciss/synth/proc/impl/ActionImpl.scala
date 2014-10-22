/*
 *  ActionImpl.scala
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

import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc
import de.sciss.lucre.event.{Reader, InMemory, EventLike, Sys}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.IDPeek
import de.sciss.serial.{Serializer, DataInput, DataOutput}

import scala.annotation.switch
import scala.collection.mutable
import scala.concurrent.{Promise, Future, blocking}
import scala.concurrent.stm.{InTxn, TMap, TSet}

object ActionImpl {
  // private val count = TxnLocal(0) // to distinguish different action class-names within the same transaction

  private final val COOKIE        = 0x61637400   // "act\0"
  private final val CONST_EMPTY   = 0
  private final val CONST_FUN     = 1
  private final val CONST_VAR     = 2

  private final val DEBUG = false

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

  final class UniverseImpl[S <: Sys[S]](val self: Action.Obj[S]) extends Action.Universe[S]

  // ---- serialization ----

  def serializer[S <: Sys[S]]: evt.EventLikeSerializer[S, Action[S]] = anySer.asInstanceOf[Ser[S]]

  def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action.Var[S]] = anyVarSer.asInstanceOf[VarSer[S]]

  private val anySer    = new Ser   [InMemory]
  private val anyVarSer = new VarSer[InMemory]

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Action[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Action[S] =
      (readCookieAndTpe(in): @switch) match {
        case CONST_FUN    =>
          val name    = in.readUTF()
          val jarSize = in.readInt()
          val jar     = new Array[Byte](jarSize)
          in.readFully(jar)
          // val system  = tx.system
          new ConstFunImpl[S](name, jar)

        case CONST_EMPTY  => new ConstEmptyImpl[S]

        case other => sys.error(s"Unexpected action cookie $other")
      }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Action[S] with evt.Node[S] =
      ActionImpl.readVar(in, access, targets)
  }

  private final class VarSer[S <: Sys[S]] extends evt.NodeSerializer[S, Action.Var[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): Action.Var[S] with evt.Node[S] =
      ActionImpl.readVar(in, access, targets)
  }

  private def readCookieAndTpe(in: DataInput): Byte = {
    val cookie = in.readInt()
    if (cookie != COOKIE)
      sys.error(s"Unexpected cookie (found ${cookie.toHexString}, expected ${COOKIE.toHexString})")
    in.readByte()
  }

  private def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): Action.Var[S] with evt.Node[S] =
    (readCookieAndTpe(in): @switch) match {
      case CONST_VAR =>
        val peer = tx.readVar[Action[S]](targets.id, in)
        new VarImpl[S](targets, peer)

      case other => sys.error(s"Unexpected action cookie $other")
    }

  // ---- constant implementation ----

  private val sync = new AnyRef

  // this is why workspace should have a general caching system
  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]

  private sealed trait ConstImpl[S <: Sys[S]] extends Action[S] with evt.impl.Constant {
    def dispose()(implicit tx: S#Tx): Unit = ()

    def changed: EventLike[S, Unit] = evt.Dummy[S, Unit]
  }

  private final class ConstFunImpl[S <: Sys[S]](val name: String, jar: Array[Byte])
    extends ConstImpl[S] {

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = {
      ActionImpl.execute[S](universe, name, jar)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_FUN)
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

    protected def reader: Reader[S, Action[S]] = ActionImpl.serializer
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

  object ElemImpl extends proc.impl.ElemCompanionImpl[Action.Elem] {
    def typeID = Action.typeID

    // Elem.registerExtension(this)

    def apply[S <: Sys[S]](peer: Action[S])(implicit tx: S#Tx): Action.Elem[S] = {
      val targets = evt.Targets[S]
      new ActiveImpl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Action.Elem[S] with evt.Node[S] = {
      val peer = ActionImpl.serializer.read(in, access)
      new ActiveImpl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Action.Elem[S] =
      sys.error("Constant Action not supported")

    // ---- implementation ----

    private sealed trait Impl {
      def typeID = Action.typeID
      def prefix = "Action"
    }

    private final class ActiveImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val peer: Action[S])
      extends Action.Elem[S]
      with proc.impl.ActiveElemImpl[S] with Impl {

      override def toString() = s"$prefix.Elem$id"

      def mkCopy()(implicit tx: S#Tx): Action.Elem[S] = Action.Elem(peer)
    }

    private final class PassiveImpl[S <: Sys[S]](val peer: Action[S])
      extends Action.Elem[S]
      with proc.impl.PassiveElemImpl[S] with Impl {

      override def toString = s"$prefix.Elem($peer)"
    }
  }
}