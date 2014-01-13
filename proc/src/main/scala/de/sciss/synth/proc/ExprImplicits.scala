/*
 *  ExprImplicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.{event => evt}
import de.sciss.synth.SynthGraph
import de.sciss.lucre.expr.Expr
import language.implicitConversions

object ExprImplicits {
  def apply[S <: evt.Sys[S]]: ExprImplicits[S] = Imp.asInstanceOf[ExprImplicits[S]]
  private object Imp extends ExprImplicits[evt.InMemory]
}

/** This class helps overcome the limitation of scala's implicit resolution -- by resolving an expression
  * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
  * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
  * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
  * outside the implicit scope (e.g. addition on longs).
  */
class ExprImplicits[S <: evt.Sys[S]] protected extends de.sciss.lucre.synth.expr.ExprImplicits[S] {
  implicit def synthGraphConst(s: SynthGraph    ): Expr[S, SynthGraph    ] = SynthGraphs  .newConst(s)
  implicit def graphemeConst  (v: Grapheme.Value): Expr[S, Grapheme.Value] = Grapheme.Elem.newConst(v)
}