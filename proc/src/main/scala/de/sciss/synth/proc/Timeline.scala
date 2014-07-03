/*
 *  Timeline.scala
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

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.event.Sys
import de.sciss.synth.proc

object Timeline {
  final val SampleRate = 14112000.0 // lcm(88.2k, 96k)

  type Update[S <: Sys[S]]  = BiGroup.Update[S,Obj[S], Obj.Update[S]]
  val  Update               = BiGroup.Update

  trait Modifiable[S <: Sys[S]] extends BiGroup.Modifiable[S, Obj[S], Obj.Update[S]]

  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Timeline[S]
    type PeerUpdate = Timeline.Update[S]
  }
}
trait Timeline[S <: Sys[S]] extends BiGroup[S, Obj[S], Obj.Update[S]] {
  override def modifiableOption: Option[Timeline.Modifiable[S]]

  // def sampleRate: Double
}