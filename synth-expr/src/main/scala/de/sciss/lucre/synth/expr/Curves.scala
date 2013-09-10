package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{event => evt}
import evt.Targets
import de.sciss.synth.Curve
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.synth
import de.sciss.synth.Curve._
import scala.annotation.switch

object Curves extends BiTypeImpl[Curve] {
  final val typeID = 15

  implicit object ValueSerializer extends ImmutableSerializer[synth.Curve] {
    def write(shape: synth.Curve, out: DataOutput): Unit = {
      out.writeInt(shape.id)
      shape match {
        case parametric(c)  => out.writeFloat(c)
        case _              =>
      }
    }

    def read(in: DataInput): synth.Curve = {
      (in.readInt(): @switch) match {
        case step       .id => step
        case linear     .id => linear
        case exponential.id => exponential
        case sine       .id => sine
        case welch      .id => welch
        case parametric .id => parametric(in.readFloat())
        case squared    .id => squared
        case cubed      .id => cubed
        case other          => sys.error("Unexpected envelope shape ID " + other)
      }
    }
  }

  def readValue(in: DataInput): Curve = ValueSerializer.read(in)

  def writeValue(value: Curve, out: DataOutput): Unit = ValueSerializer.write(value, out)

  // ---- protected ----

  def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    cookie match {
      case _ => sys.error("Invalid cookie " + cookie)
    }
  }
}
