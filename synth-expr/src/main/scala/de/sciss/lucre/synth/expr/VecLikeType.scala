package de.sciss.lucre.synth.expr

import scala.collection.immutable.{Seq => ISeq, IndexedSeq => Vec}
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.lucre.bitemp.BiType

trait VecLikeType[A] extends BiTypeImpl[Vec[A]] {
  def element: BiType[A]

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): ReprNode[S] = sys.error("Invalid cookie " + cookie)

  private val valueSer = ImmutableSerializer.indexedSeq[A](element.ValueSer)

  def writeValue(value: Vec[A], out: DataOutput): Unit = valueSer.write(value, out)
  def readValue(in: DataInput): Vec[A] = valueSer.read(in)

  // def typeID: Int = ...

  // def newConst[S <: Sys[S]](vec: Vec[A])         : Expr.Const[S, Vec[A]]

  // def wrap[S <: Sys[S]](vec: Vec[Expr[S, A]]): Expr[S, Vec[A]]

  // def newVar  [S <: Sys[S]](vec: Vec[Expr[S, A]]): Expr.Var  [S, Vec[A]]
}
//trait VecExpr[S <: Sys[S], +A] extends Expr[S, Vec[A]] {
//  def size: Expr[S, Int]
//  def get(index: Expr[S, Int]): Expr[S, Option[A]] // OptionExpr[S, A]
//}