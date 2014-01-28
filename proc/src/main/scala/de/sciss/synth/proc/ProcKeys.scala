/*
 *  ProcKeys.scala
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