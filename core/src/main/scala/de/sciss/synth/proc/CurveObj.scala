/*
 *  CurveObj.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.expr.impl.ExprTypeImpl
import de.sciss.lucre.stm.Sys
import de.sciss.synth.{proc, Curve}

object CurveObj extends ExprTypeImpl[Curve, CurveObj] {
  import proc.{CurveObj => Repr}

  final val typeID = 15
  final val valueSerializer = Curve.serializer

  protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
    extends VarImpl[S] with Repr[S]
}
trait CurveObj[S <: Sys[S]] extends Expr[S, Curve]