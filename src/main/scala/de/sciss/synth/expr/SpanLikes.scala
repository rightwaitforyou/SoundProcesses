/*
 *  Booleans.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.expr

import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.{Span, SpanLike, Type}

// typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
//           8 = string, 9 = spanlike, 10 = span
object SpanLikes extends BiTypeImpl[ SpanLike ] {
   private val typeID = 9

   /* protected */ def readValue( in: DataInput ) : SpanLike = SpanLike.read( in )
   /* protected */ def writeValue( value: SpanLike, out: DataOutput ) { value.write( out )}

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ]) {
   }

   // ---- protected ----

   def readTuple[ S <: Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Ex[ S ] =
//   case 3 =>
//      readCursor[ S ]( in, access, targets )
      sys.error( "Invalid cookie " + cookie )
}