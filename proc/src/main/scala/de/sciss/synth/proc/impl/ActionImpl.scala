//package de.sciss.synth.proc
//package impl
//
//import de.sciss.lucre.{event => evt}
//import de.sciss.lucre.event.Sys
//import de.sciss.lucre.expr.impl.ExprTypeImpl
//import de.sciss.serial.{DataInput, DataOutput}
//
//object ActionImpl extends ExprTypeImpl[() => Unit] {
//  def typeID: Int = Action.typeID
//
//  protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                     (implicit tx: S#Tx): Ex[S] with evt.Node[S] = {
//    ???
//  }
//
//  def readValue(in: DataInput): () => Unit = {
//    ???
//  }
//
//  def writeValue(value: () => Unit, out: DataOutput): Unit = {
//    ???
//  }
//}
