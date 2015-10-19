package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{Serializer, DataInput}
import impl.{OutputImpl => Impl}

object Output extends Obj.Type {
  final val typeID = 0x10009

  def apply[S <: Sys[S]](proc: Proc[S], key: String)(implicit tx: S#Tx): Output[S] =  Impl(proc, key)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Output[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Output[S]] = Impl.serializer

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Output[S <: Sys[S]] extends Obj[S] {
  def proc: Proc[S]
  def key : String  // or `StringObj`?
}