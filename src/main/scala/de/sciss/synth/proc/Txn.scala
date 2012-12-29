/*
 *  Txn.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.proc

import de.sciss.osc
import de.sciss.synth.{osc => sosc}
import concurrent.stm.InTxn
import collection.immutable.{IndexedSeq => IIdxSeq}
import impl.{ProcTxnPlainImpl => PlainImpl}

object Txn {
   private[proc] def applyPlain( implicit itx: InTxn ) : Txn = new PlainImpl( itx )

   /**
    * A data type encapsulating all the outgoing OSC bundles for this transaction.
    *
    * @param firstCnt   the counter value of the first bundle in the payload
    * @param payload    the succession of bundles, represented as a sequence of a sequence of messages
    */
   final case class Bundles( firstCnt: Int, payload: IIdxSeq[ IIdxSeq[ osc.Message with sosc.Send ]])
}

/**
 * The `Txn` trait is declared without representation type parameter in order to keep the real-time sound
 * synthesis API clutter free. The sound synthesis is always ephemeral, so does not need to know anything
 * about the underlying system. What the process transaction provides is a package private
 * `addMessage` method for staging OSC messages which are flushed at the end of a successful transaction.
 */
trait Txn {
//   protected type S <: evt.

   def peer: InTxn
//   def beforeCommit( fun: Txn => Unit ) : Unit
   private[proc] def addMessage( resource: Resource, message: osc.Message with sosc.Send,
                                 audible: Boolean, dependencies: Seq[ Resource ] = Nil, noErrors: Boolean = false ) : Unit

//   def addMessage( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, State, Boolean) ], audible: Boolean,
//                   dependencies: Map[ State, Boolean ], noError: Boolean = false ) {
}