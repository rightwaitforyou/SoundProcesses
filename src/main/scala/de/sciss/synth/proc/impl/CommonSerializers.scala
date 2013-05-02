/*
 *  CommonSerializers.scala
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

package de.sciss.synth
package proc
package impl

import annotation.switch
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object CommonSerializers {
  implicit object EnvConstShape extends ImmutableSerializer[Env.ConstShape] {
    def write(shape: Env.ConstShape, out: DataOutput) {
      val sid = shape.id
      out.writeInt(sid)
      if (sid == curveShape.id) out.writeFloat(shape.curvature)
    }

    def read(in: DataInput): Env.ConstShape = {
      (in.readInt(): @switch) match {
        case stepShape  .id => stepShape
        case linShape   .id => linShape
        case expShape   .id => expShape
        case sinShape   .id => sinShape
        case welchShape .id => welchShape
        case curveShape .id => curveShape(in.readFloat())
        case sqrShape   .id => sqrShape
        case cubShape   .id => cubShape
        case other          => sys.error("Unexpected envelope shape ID " + other)
      }
    }
  }

}
