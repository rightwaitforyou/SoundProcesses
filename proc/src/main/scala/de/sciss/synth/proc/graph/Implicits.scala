/*
 *  Implicits.scala
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

package de.sciss.synth
package proc
package graph

/** Type enrichments for graph generation. */
object Implicits {
  /** Enables the use of a `stream` argument in place of the (numChannels, buf) tuple in regular `DiskIn` use. */
  implicit class diskInStream(peer: ugen.DiskIn.type) {
    def ar(buf: stream, loop: GE = 0): GE =
      stream.DiskIn(audio, key = buf.key, loop = loop)
  }

  /** Enables the use of a `stream` argument in place of the (numChannels, buf) tuple in regular `VDiskIn` use. */
  implicit class vDiskInStream(peer: ugen.VDiskIn.type) {
    def ar(buf: stream, speed: GE = 1f, loop: GE = 0): GE =
      stream.VDiskIn(audio, key = buf.key, speed = speed, loop = loop)
  }
}