/*
 *  Time.scala
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

package de.sciss.synth
package proc
package graph

object Time {
  private[proc] final val key = "$time"

  def ir: GE = Time()
}
/** Absolute time on the canvas, in seconds. */
final case class Time() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Time.key.ir
}

object Offset {
  private[proc] final val key = "$off"

  def ir: GE = Offset()
}
/** Start time offset within the proc, in seconds. Will be zero if proc is started from the beginning. */
final case class Offset() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Offset.key.ir
}

object Duration {
  private[proc] final val key = "$dur"

  def ir: GE = Duration()
}

/** Total duration of proc in seconds. If proc was started midway through, this is still its total
  * length. To gather for how long it's going to play, use `Duration() - Offset()`.
  */
final case class Duration() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Duration.key.ir
}
