/*
 *  Elem.scala
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

package de.sciss
package synth
package proc

import de.sciss.lucre.{event => evt}
import evt.{EventLike, Sys}
import de.sciss.lucre.stm.Disposable
import proc.impl.{ElemImpl => Impl}
import scala.language.{higherKinds, implicitConversions}
import de.sciss.serial.{DataInput, Writable}

object Elem {
  final case class Update[S <: Sys[S], +Upd](element: Elem[S], change: Upd)

  //  type Expr[S <: Sys[S], A] = Elem[S] {
  //    type Peer[~ <: Sys[~]] = de.sciss.lucre.expr.Expr[~, A]
  //  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Elem[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = Impl.read(in, access)

  trait Companion[E[~ <: Sys[~]]] {
    def typeID: scala.Int
  }

  trait Extension {
    /** Unique type identifier */
    def typeID: scala.Int

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                      (implicit tx: S#Tx): Elem[S] with evt.Node[S]

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Elem[S]
  }

  def registerExtension(ext: Extension): Unit = {
    // Impl
    // println(s"YO: ${Impl.debug()}")
    Impl.registerExtension(ext)
  }
}
trait Elem[S <: Sys[S]]
  extends Writable with Disposable[S#Tx] /* Mutable[S#ID, S#Tx] */ with evt.Publisher[S, Elem.Update[S, Any]] { me =>

  def typeID: Int

  type Peer
  type PeerUpdate

  /** The actual object wrapped by the element. */
  val peer: Peer

  override def changed: EventLike[S, Elem.Update[S, PeerUpdate]]

  import me.{Peer => Peer0, PeerUpdate => PeerUpdate0}

  def mkCopy()(implicit tx: S#Tx): Elem[S] {
    type Peer       = Peer0
    type PeerUpdate = PeerUpdate0
  }
}