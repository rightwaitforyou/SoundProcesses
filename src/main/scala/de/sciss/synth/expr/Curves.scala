package de.sciss
package synth
package expr

import de.sciss.lucre.{event => evt}
import evt.Targets
import serial.{DataInput, DataOutput}
import proc.impl.CommonSerializers.{Curve => ser}

object Curves extends BiTypeImpl[Curve] {
  final val typeID = 15

  def readValue(in: DataInput): Curve = ser.read(in)

  def writeValue(value: Curve, out: DataOutput) {
    ser.write(value, out)
  }

  // ---- protected ----

  def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    cookie match {
      case _ => sys.error("Invalid cookie " + cookie)
    }
  }
}
