/*
 *  Implicits.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{String => StringEx, Boolean => BooleanEx}

object Implicits {
  implicit final class RichAttr[S <: Sys[S]](val `this`: AttrMap.Modifiable[S]) extends AnyVal { me =>
    import me.{`this` => attr}

    def name(implicit tx: S#Tx): String =
      attr.expr[String](ObjKeys.attrName).fold("<unnamed>")(_.value)

    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      val valueC = StringEx.newConst[S](value)
      attr.expr[String](ObjKeys.attrName) match {
        case Some(Expr.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = StringEx.newVar(valueC)
          attr.put(ObjKeys.attrName, Obj(StringElem[S](valueVr)))
      }
    }

    def muted(implicit tx: S#Tx): Boolean =
      attr.expr[Boolean](ObjKeys.attrMute).fold(false)(_.value)

    def muted_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      val valueC = BooleanEx.newConst[S](value)
      attr.expr[Boolean](ObjKeys.attrMute) match {
        case Some(Expr.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = BooleanEx.newVar(valueC)
          attr.put(ObjKeys.attrMute, Obj(BooleanElem[S](valueVr)))
      }
    }
  }
}