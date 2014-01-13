/*
 *  ProcKeys.scala
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

package de.sciss.synth.proc

object ProcKeys {
  /** Name attribute. Type `String` */
  final val attrName    = "name"
  /** Track attribute. Type `Int` */
  final val attrTrack   = "track"
  /** Audio input file (tape) grapheme. */
  final val graphAudio  = "sig"
  /** Source code of the graph function. */
  final val attrGraphSource = "graph-source"
  /** Bus attribute. Type `Int` */
  final val attrBus     = "bus"
  /** Gain factor. Type `Double` */
  final val attrGain    = "gain"
  /** Gain factor. Type `Boolean` */
  final val attrMute    = "mute"
  /** Fade in. Type `FadeSpec` */
  final val attrFadeIn  = "fade-in"
  /** Fade out. Type `FadeSpec` */
  final val attrFadeOut = "fade-out"

  final val scanMainIn  = "in"
  final val scanMainOut = "out"
}