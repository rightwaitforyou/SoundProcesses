/*
 *  Sys.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.{event => evt, synth}

object Sys {
  trait Txn[S <: Sys[S]] extends evt.Txn[S] with synth.Txn
}

trait Sys[S <: Sys[S]] extends evt.Sys[S] {
  type Tx <: Sys.Txn[S]
}
