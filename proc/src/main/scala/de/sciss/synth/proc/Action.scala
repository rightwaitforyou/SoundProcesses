//package de.sciss.synth.proc
//
//import de.sciss.lucre.event.{Serializer, Sys}
//import de.sciss.lucre.expr.Expr.{Var, Const}
//import de.sciss.lucre.expr.{Expr, ExprType}
//import de.sciss.serial.{DataInput, DataOutput}
//
//object Action {
//  final val typeID = 19
//
//  def newConst[S <: Sys[S]](value       : () => Unit )                   : Const[S, () => Unit] = ???
//  def newVar  [S <: Sys[S]](init: Expr[S, () => Unit])(implicit tx: S#Tx): Var  [S, () => Unit] = ???
//
//  def readConst[S <: Sys[S]](in: DataInput                                  ): Const[S, () => Unit] = ???
//  def readVar  [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Var  [S, () => Unit] = ???
//
//  def readValue(in: DataInput): () => Unit = ???
//  def writeValue(value: () => Unit, out: DataOutput): Unit = ???
//
//  implicit def serializer   [S <: Sys[S]]: Serializer[S, Expr[S, () => Unit]] = ???
//  implicit def varSerializer[S <: Sys[S]]: Serializer[S, Var [S, () => Unit]] = ???
//}