/*
 *  State.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import scala.concurrent.stm.Ref

object State {
  def apply(owner: Any, name: String, init: Boolean): State = new Impl(owner, name, init)

  //   def and( that: State )( owner: Any, name: String, init: Boolean ) : State = new And( that, owner, name, init )

  private final class Impl(val owner: Any, val name: String, init: Boolean) extends State {
    val value = Ref(init)

    def swap(newValue: Boolean)(implicit tx: Txn): Boolean = value.swap(newValue)(tx.peer)

    def get(implicit tx: Txn): Boolean = value.get(tx.peer)
  }

  //   private final class And( that: State, val owner: Any, val name: String, init: Boolean ) extends State {
//      val value = Ref( init )
//      def swap( newValue: Boolean )( implicit tx: Txn ) : Boolean = {
//         value.swap( newValue )( tx.peer ) && that.get
//      }
//      def get( implicit tx: Txn ) : Boolean = value.get( tx.peer ) && that.get
//   }
//
//   object IfEqual {
//      /**
//       * If a state change does not imply a change in the state's value, drop any associated OSC message.
//       */
//      case object Drop  extends IfEqual
//      /**
//       * If a state change does not imply a change in the state's value, send any associated OSC message anyways.
//       */
//      case object Send  extends IfEqual
//      /**
//       * If a state change does not imply a change in the state's value, throw an error (rollback transaction).
//       */
//      case object Error extends IfEqual
//   }
//   sealed trait IfEqual
//
//   /**
//    * Description of a state change.
//    *
//    * @param state   the state which changes according to an OSC message
//    * @param value   the state's target value
//    * @param ifEqual the behavior requested in the case that the state's previous and target value are the same:
//    *                `Drop` means that the OSC message should be dropped (not sent); `Send` means it should nevertheless
//    *                be sent; `Error` means this is an unexpected situation and the transaction should be rolled back.
//    */
//   final case class Change( state: State, value: Boolean, ifEqual: IfEqual )
}
sealed trait State {
  protected def value: Ref[Boolean]

  def swap(newValue: Boolean)(implicit tx: Txn): Boolean

  def get(implicit tx: Txn): Boolean

  final def set(newValue: Boolean)(implicit tx: Txn): Unit =
    value.set(newValue)(tx.peer)

  protected def owner: Any

  def name: String

  override def toString = s"<$owner $name>"
}