/*
 *  AuralContextImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj}
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth.proc.AuralContext.{AuxAdded, AuxUpdate}

import scala.concurrent.stm.Ref
import scala.language.higherKinds

object AuralContextImpl {
  def apply[S <: Sys[S]](server: Server, sched: Scheduler[S])
                        (implicit tx: S#Tx, workspaceHandle: WorkspaceHandle[S]): AuralContext[S] = {
    val objMap  = tx.newInMemoryIDMap[Entry[S]]
    val auxMap  = tx.newInMemoryIDMap[Any]
    val obsMap  = tx.newInMemoryIDMap[List[AuxObserver[S]]]
    val res     = new Impl[S](objMap, auxMap, obsMap, sched, server)
    logAural(s"create context ${res.hashCode().toHexString}")
    // (new Throwable).printStackTrace()
    res
  }

  private final class Entry[S <: Sys[S]](val data: Disposable[S#Tx]) {
    val count = Ref(0)
  }

  private trait AuxObserver[S <: Sys[S]]
    extends Disposable[S#Tx] {

    def fun: S#Tx => AuxUpdate[S, Any] => Unit
  }

  private final class Impl[S <: Sys[S]](objMap: IdentifierMap[S#ID, S#Tx, Entry[S]],
                                        auxMap: IdentifierMap[S#ID, S#Tx, Any],
                                        auxObservers: IdentifierMap[S#ID, S#Tx, List[AuxObserver[S]]],
                                        val scheduler: Scheduler[S],
                                        val server: Server)(implicit val workspaceHandle: WorkspaceHandle[S])
    extends AuralContext[S] /* with ObservableImpl[S, AuralContext.Update[S]] */ {

    // private val waiting = TxnLocal(Map.empty[S#ID, List[PartialFunction[Any, Unit]]])

    def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A = {
      val id = obj.id
      val e  = objMap.getOrElse(id, {
        val e0 = new Entry[S](init)
        objMap.put(id, e0)
        e0
      })
      e.count.transform(_ + 1)(tx.peer)
      e.data.asInstanceOf[A]
    }

    def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A] =
      objMap.get(obj.id).map(_.data.asInstanceOf[A])

    def release(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val id  = obj.id
      val e   = objMap.getOrElse(id, sys.error(s"No data cached for $obj"))
      val c   = e.count.transformAndGet(_ - 1)(tx.peer)
      if (c == 0) {
        objMap.remove(id)
        e.data.dispose()
      }
    }

    private final class AuxObserverImpl(idH: stm.Source[S#Tx, S#ID],
                                        val fun: S#Tx => AuxUpdate[S, Any] => Unit)
      extends AuxObserver[S] {

      def dispose()(implicit tx: S#Tx): Unit = {
        val id    = idH()
        val list0 = auxObservers.getOrElse(id, Nil)
        val list1 = list0.filterNot(_ == this)
        if (list1.isEmpty) auxObservers.remove(id) else auxObservers.put(id, list1)
      }
    }

    def observeAux[A](id: S#ID)(fun: S#Tx => AuxUpdate[S, A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
      val list0 = auxObservers.getOrElse(id, Nil)
      val obs   = new AuxObserverImpl(tx.newHandle(id), fun.asInstanceOf[S#Tx => AuxUpdate[S, Any] => Unit])
      val list1 = obs :: list0
      auxObservers.put(id, list1)
      obs
    }

    def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit = {
      auxMap.put(id, value)
      implicit val itx = tx.peer
      val list = auxObservers.getOrElse(id, Nil)
        if (list.nonEmpty) {
          val upd = AuxAdded(id, value)
          list.foreach { obs =>
          obs.fun(tx)(upd)
        }
      }
    }

    def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A] = auxMap.get(id).asInstanceOf[Option[A]]

    def removeAux(id: S#ID)(implicit tx: S#Tx): Unit = {
      auxMap.remove(id)
    }
  }
}
