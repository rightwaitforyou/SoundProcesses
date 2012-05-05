/*
 *  Spans.scala
 *  (LucreExpr)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucre
package expr

object Span {
   def from( start: Long ) : Span = Span( start, 0x4000000000000000L )  // XXX TODO should have special version of Span)
}
final case class Span( start: Long, stop: Long ) {
   def length: Long = stop - start

   // where overlapping results in negative spacing
   def spacing( b: Span ) : Long = {
      val bStart = b.start
      if( start < bStart ) {
         bStart - stop
      } else {
         start - b.stop
      }
   }

   /**
    *  Checks if a position lies within the span.
    *
    *  @return		<code>true</code>, if <code>start <= pos < stop</code>
    */
   def contains( pos: Long ) : Boolean = pos >= start && pos < stop

   /**
    *  Checks if another span lies within the span.
    *
    *	@param	aSpan	second span, may be <code>null</code> (in this case returns <code>false</code>)
    *  @return		<code>true</code>, if <code>aSpan.start >= this.span &&
    *				aSpan.stop <= this.stop</code>
    */
    def contains( aSpan: Span ) : Boolean =
         (aSpan.start >= this.start) && (aSpan.stop <= this.stop)

   /**
    *  Checks if a two spans overlap each other.
    *
    *	@param	aSpan	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
    def overlaps( aSpan: Span ) : Boolean =
      ((aSpan.start < this.stop) && (aSpan.stop > this.start))

   /**
    *  Checks if a two spans overlap or touch each other.
    *
    *	@param	aSpan	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
    def touches( aSpan: Span ) : Boolean =
      if( start <= aSpan.start ) {
         stop >= aSpan.start
      } else {
         aSpan.stop >= start
      }

   /**
    *  Checks if the span is empty.
    *
    *  @return		<code>true</code>, if <code>start == stop</code>
    */
   def isEmpty : Boolean = start == stop

   def nonEmpty : Boolean = start != stop

   def unite( aSpan: Span )      = Span( math.min( start, aSpan.start ), math.max( stop, aSpan.stop ))
   def intersect( aSpan: Span )  = Span( math.max( start, aSpan.start ), math.min( stop, aSpan.stop ))

   def clip( pos: Long ) : Long = math.max( start, math.min( stop, pos ))

   def shift( delta: Long ) = Span( start + delta, stop + delta )
}
