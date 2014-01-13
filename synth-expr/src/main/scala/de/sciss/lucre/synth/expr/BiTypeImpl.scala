/*
 *  BiTypeImpl.scala
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

package de.sciss.lucre.synth
package expr

import de.sciss.lucre.bitemp.BiType
import de.sciss.span.SpanLike

/**
  * typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
  *           8 = string, 9 = spanlike, 10 = span, 11 = Grapheme.Value
  */
trait BiTypeImpl[A] extends BiType[A] {
  //   final protected def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ] =
  //      Longs.readExpr( in, access )

  final def longType    : BiType[Long    ] = Longs
  final def spanLikeType: BiType[SpanLike] = SpanLikes
}
