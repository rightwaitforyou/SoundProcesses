package de.sciss
package synth
package expr

import de.sciss.lucre.{event => evt}
import evt.Targets
import serial.{DataInput, DataOutput}
import proc.impl.CommonSerializers.{EnvConstShape => ser}

object EnvShapes extends BiTypeImpl[Env.ConstShape] {
  final val typeID = 15

  def readValue(in: DataInput): Env.ConstShape = ser.read(in)

  def writeValue(value: Env.ConstShape, out: DataOutput) {
    ser.write(value, out)
  }

  // ---- protected ----

  def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    (cookie /* : @switch */) match {
      case _ => sys.error("Invalid cookie " + cookie)
    }
  }
}
