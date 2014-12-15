/*
 *  Txn.scala
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

package de.sciss.lucre
package synth

import collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.InTxn
import de.sciss.osc
import de.sciss.lucre.synth.impl.TxnPlainImpl
import de.sciss.synth.message
import de.sciss.lucre.stm.TxnLike

object Txn {
  def wrap(implicit itx: InTxn): Txn = new TxnPlainImpl(itx)

  /** A data type encapsulating all the outgoing OSC bundles for this transaction.
    *
    * @param firstCnt   the counter value of the first bundle in the payload
    * @param payload    the succession of bundles, represented as a sequence of a sequence of messages
    */
  final case class Bundles(firstCnt: Int, payload: Vec[Vec[osc.Message with message.Send]])
}

/** The `Txn` trait is declared without representation type parameter in order to keep the real-time sound
  * synthesis API clutter free. The sound synthesis is always ephemeral, so does not need to know anything
  * about the underlying system. What the process transaction provides is a package private
  * `addMessage` method for staging OSC messages which are flushed at the end of a successful transaction.
  */
trait Txn extends TxnLike {
  def addMessage(resource: Resource, m: osc.Message with message.Send,
                 dependencies: Seq[Resource] = Nil, noErrors: Boolean = false): Unit
}