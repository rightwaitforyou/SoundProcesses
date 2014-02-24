/*
 *  Strings.scala
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
import evt.{Targets, Sys}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.bitemp.BiType
import scala.annotation.switch

object Strings extends BiTypeImpl[String] {
  final val typeID = 8

  def readValue (               in : DataInput ): String  = in .readUTF()
  def writeValue(value: String, out: DataOutput): Unit    = out.writeUTF(value)

  lazy val install: Unit = {
    registerOp(StringTuple2s)
  }

  private[this] object StringTuple2s extends BiType.TupleReader[String] {
    final val arity = 2
    final val opLo  = BinaryOp.Append.id
    final val opHi  = BinaryOp.Append.id

    val name = "Long-Long Ops"

    def readTuple[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                              (implicit tx: S#Tx): Expr.Node[S, String] = {
      import BinaryOp._
      val op: Op = (opID: @switch) match {
        case Append.id => Append
      }
      val _1 = readExpr(in, access)
      val _2 = readExpr(in, access)
      new Tuple2(typeID, op, targets, _1, _2)
    }
  }

  // ----- operators -----

  final class Ops[S <: Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    private type E = Ex[S]

    import BinaryOp._

    def ++(b: E): E = Append(ex, b)
  }

  private object BinaryOp {
    sealed abstract class Op extends Tuple2Op[String, String] {
      def id: Int
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new Tuple2(typeID, this, Targets.partial[S], a, b)
      }

      def value(a: String, b: String): String

      def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.indexOf('$') + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    case object Append extends Op {
      final val id = 0
      def value(a: String, b: String): String = a + b
    }
  }
}