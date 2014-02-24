/*
 *  Curves.scala
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

package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{event => evt}
import evt.Targets
import de.sciss.synth.Curve
import de.sciss.serial.{DataOutput, DataInput}

object Curves extends BiTypeImpl[Curve] {
  final val typeID = 15

  def readValue (              in : DataInput ): Curve  = Curve.serializer.read (       in )
  def writeValue(value: Curve, out: DataOutput): Unit   = Curve.serializer.write(value, out)

  lazy val install: Unit = ()
}