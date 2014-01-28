/*
 *  SynthGraphSerializer.scala
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
package impl

import scala.annotation.switch
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.{Lazy, MaybeRate, SynthGraph}
import de.sciss.synth.ugen.{ControlProxyLike, Constant}
