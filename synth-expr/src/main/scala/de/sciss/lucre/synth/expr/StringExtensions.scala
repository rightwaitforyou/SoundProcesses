/*
 *  StringExtensions.scala
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
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.serial.DataInput
import de.sciss.lucre.expr.{Type, Expr}
import scala.annotation.switch
import de.sciss.lucre

object StringExtensions  {
  import de.sciss.lucre.expr.String.{newConst, read, typeID, registerExtension}

  private[this] type Ex[S <: Sys[S]] = Expr[S, String]

  registerExtension(2, StringTuple2s)

  private[this] object StringTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, String]})#Repr] {
    final val arity = 2
    final val opLo  = BinaryOp.Append.id
    final val opHi  = BinaryOp.Append.id

    val name = "String-2 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, String] = {
      import BinaryOp._
      val op: Op = (opID: @switch) match {
        case Append.id => Append
      }
      val _1 = read(in, access)
      val _2 = read(in, access)
      new impl.Tuple2(lucre.expr.String, typeID, op, targets, _1, _2)
    }
  }

  // ----- operators -----

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    import BinaryOp._

    def ++(b: E)(implicit tx: S#Tx): E = Append(ex, b)
  }

  private object BinaryOp {
    sealed abstract class Op extends impl.Tuple2Op[String, String, String] {
      def id: Int
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new impl.Tuple2(lucre.expr.String, typeID, this, Targets.partial[S], a, b)
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