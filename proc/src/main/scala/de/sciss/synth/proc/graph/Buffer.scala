/*
 *  Buffer.scala
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

package de.sciss.synth.proc.graph

import de.sciss.synth.{UGenInLike, GE, control, scalar, Rate}

/** An element referring to a random access buffer provided through an attribute.
  * The attribute will typically be an audio grapheme.
  */
object Buffer {
  def ir(key: String): Buffer = new Buffer(scalar , key)
  def kr(key: String): Buffer = new Buffer(control, key)

  private[proc] def controlName(key: String): String = "$buf_"  + key
}

/** An element referring to a random access buffer provided through an attribute.
  * The attribute will typically be an audio grapheme.
  *
  * @param key  the attribute key.
  */
final case class Buffer(rate: Rate, key: String) extends GE.Lazy {
  protected def makeUGens: UGenInLike = ???
}