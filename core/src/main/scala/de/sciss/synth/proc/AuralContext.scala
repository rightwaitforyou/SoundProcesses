/*
 *  AuralContext.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.synth.{Server, Sys => SSys}
import de.sciss.synth.proc.impl.{AuralContextImpl => Impl}

import scala.language.higherKinds

object AuralContext {
  def apply[S <: SSys[S]](server: Server, scheduler: Scheduler[S])
                        (implicit tx: S#Tx, workspaceHandle: WorkspaceHandle[S]): AuralContext[S] =
    Impl(server, scheduler)

  def apply[S <: SSys[S]](server: Server)(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                         workspaceHandle: WorkspaceHandle[S]): AuralContext[S] = {
    val sched = Scheduler[S]
    apply(server, sched)
  }

  sealed trait AuxUpdate[S <: Sys[S], +A]
  final case class AuxAdded  [S <: Sys[S], A](id: S#ID, value: A) extends AuxUpdate[S, A]
  // final case class AuxRemoved[S <: Sys[S], A](id: S#ID, value: A) extends AuxUpdate[S, A]
}
trait AuralContext[S <: Sys[S]] /* extends Observable[S#Tx, AuralContext.Update[S]] */ {
  import AuralContext.AuxUpdate

  def server: Server

  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit

  def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A]

  /** Waits for the auxiliary object to appear. If the object
    * appears the function is applied, otherwise nothing happens.
    */
  def observeAux[A](id: S#ID)(fun: S#Tx => AuxUpdate[S, A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def removeAux(id: S#ID)(implicit tx: S#Tx): Unit

  val scheduler: Scheduler[S]

  implicit def workspaceHandle: WorkspaceHandle[S]
}