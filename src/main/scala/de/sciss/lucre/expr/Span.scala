/*
 *  Span.scala
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

package de.sciss.lucre
package expr

object Span {
   def from( start: Long ) = SpanFrom( start )
   def until( stop: Long ) = SpanUntil( stop )
}
sealed trait SpanLike {
   def clip( pos: Long ) : Long
   def shift( delta: Long ) : SpanLike

   /**
    *  Checks if the span is empty.
    *
    *  @return		<code>true</code>, if <code>start == stop</code>
    */
   def isEmpty : Boolean

   def nonEmpty : Boolean

   /**
    *  Checks if a position lies within the span.
    *
    *  @return		<code>true</code>, if <code>start <= pos < stop</code>
    */
   def contains( pos: Long ) : Boolean

   /**
    *  Checks if another span lies within the span.
    *
    *	@param	that	second span, may be <code>null</code> (in this case returns <code>false</code>)
    *  @return		`true`, if `that.start >= this.span && that.stop <= this.stop`
    */
   def contains( that: SpanLike ) : Boolean

   /**
    *  Checks if a two spans overlap each other.
    *
    *	@param	that	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
   def overlaps( that: SpanLike ) : Boolean

   /**
    *  Checks if a two spans overlap or touch each other.
    *
    *	@param	that	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
   def touches( that: SpanLike ) : Boolean

   def union( that: SpanLike ) : SpanLike
   def intersect( that: SpanLike ) : SpanLike
}
sealed trait OpenSpan extends SpanLike {
   final def isEmpty    = false
   final def nonEmpty   = true

   def shift( delta: Long ) : OpenSpan
   def union( that: SpanLike ) : OpenSpan
}
case object AllSpan extends OpenSpan {
   def shift( delta: Long ) : OpenSpan = this
   def union( that: SpanLike ) : OpenSpan = this
   def intersect( that: SpanLike ) : SpanLike = that
   def clip( pos: Long ) : Long = pos

   def contains( pos: Long )        = true
   def contains( that: SpanLike )   = true
   def overlaps( that: SpanLike )   = true
   def touches( that: SpanLike )    = true
}
final case class SpanFrom( start: Long ) extends OpenSpan {
   def clip( pos: Long ) : Long = math.max( start, pos )
   def shift( delta: Long ) : OpenSpan = SpanFrom( start + delta )

   def contains( pos: Long ) : Boolean = pos >= start

   def contains( that: SpanLike ) : Boolean = that match {
      case SpanFrom( thatStart ) => thatStart >= start
      case Span( thatStart, _ )  => thatStart >= start
      case _                     => false
   }

   def overlaps( that: SpanLike ) : Boolean = that match {
      case SpanFrom( _ )         => true
      case SpanUntil( thatStop ) => start < thatStop
      case Span( _, thatStop )   => start < thatStop
      case VoidSpan              => false
      case AllSpan               => true
   }

   def touches( that: SpanLike ) : Boolean = that match {
      case SpanFrom( _ )         => true
      case SpanUntil( thatStop ) => start <= thatStop
      case Span( _, thatStop )   => start <= thatStop
      case VoidSpan              => false
      case AllSpan               => true
   }

   def union( that: SpanLike ) : OpenSpan = that match {
      case SpanFrom( thatStart ) => SpanFrom( math.min( start, thatStart ))
      case Span( thatStart, _ )  => SpanFrom( math.min( start, thatStart ))
      case VoidSpan              => this
      case _                     => AllSpan
   }

   def intersect( that: SpanLike ) : SpanLike = that match {
      case SpanFrom( thatStart ) => SpanFrom( math.max( start, thatStart ))
      case SpanUntil( thatStop ) => if( start <= thatStop ) Span( start, thatStop ) else VoidSpan
      case Span( thatStart, thatStop ) =>
         val maxStart = math.max( start, thatStart )
         if( maxStart <= thatStop ) Span( maxStart, thatStop ) else VoidSpan
      case VoidSpan  => VoidSpan
      case AllSpan   => this
   }
}
final case class SpanUntil( stop: Long ) extends OpenSpan {
   def clip( pos: Long ) : Long = math.min( stop, pos )
   def shift( delta: Long ) : OpenSpan = SpanUntil( stop + delta )

   def contains( pos: Long ) : Boolean = pos < stop

   def contains( that: SpanLike ) : Boolean = that match {
      case SpanUntil( thatStop ) => thatStop <= stop
      case Span( _, thatStop )   => thatStop <= stop
      case _                     => false
   }

   def overlaps( that: SpanLike ) : Boolean = that match {
      case SpanUntil( _ )        => true
      case SpanFrom( thatStart ) => thatStart < stop
      case Span( thatStart, _ )  => thatStart < stop
      case VoidSpan              => false
      case AllSpan               => true
   }

   def touches( that: SpanLike ) : Boolean = that match {
      case SpanUntil( _ )        => true
      case SpanFrom( thatStart ) => thatStart <= stop
      case Span( thatStart, _ )  => thatStart <= stop
      case VoidSpan              => false
      case AllSpan               => true
   }

   def union( that: SpanLike ) : OpenSpan = that match {
      case SpanUntil( thatStop ) => SpanUntil( math.max( stop, thatStop ))
      case Span( _, thatStop )   => SpanUntil( math.max( stop, thatStop ))
      case VoidSpan              => this
      case _                     => AllSpan
   }

   def intersect( that: SpanLike ) : SpanLike = that match {
      case SpanFrom( thatStart ) => if( thatStart <= stop ) Span( thatStart, stop ) else VoidSpan
      case SpanUntil( thatStop ) => SpanUntil( math.min( stop, thatStop ))
      case Span( thatStart, thatStop ) =>
         val minStop = math.min( stop, thatStop )
         if( thatStart <= minStop ) Span( thatStart, minStop ) else VoidSpan
      case VoidSpan  => VoidSpan
      case AllSpan   => this
   }
}
sealed trait ClosedSpan extends SpanLike {
   def length: Long

   def shift( delta: Long ) : ClosedSpan
   def intersect( that: SpanLike ) : ClosedSpan
}
case object VoidSpan extends ClosedSpan {
   val length = 0L

   def shift( delta: Long ) : ClosedSpan = this
   def union( that: SpanLike ) : SpanLike = that
   def intersect( that: SpanLike ) : ClosedSpan = this
   def clip( pos: Long ) : Long = pos

   def contains( pos: Long )        = false
   def contains( that: SpanLike )   = false
   def overlaps( that: SpanLike )   = false
   def touches( that: SpanLike )    = false

   val isEmpty    = true
   val nonEmpty   = false
}
final case class Span( start: Long, stop: Long ) extends ClosedSpan {
   require( start <= stop, "A span's start (" + start + ") must be <= its stop (" + stop + ")" )

   def length: Long = stop - start

   def contains( pos: Long ) : Boolean = pos >= start && pos < stop

   def shift( delta: Long ) : ClosedSpan = Span( start + delta, stop + delta )

   def clip( pos: Long ) : Long = math.max( start, math.min( stop, pos ))

   def isEmpty : Boolean = start == stop

   def nonEmpty : Boolean = start != stop

   def contains( that: SpanLike ) : Boolean = that match {
      case Span( thatStart, thatStop ) => (thatStart >= start) && (thatStop <= stop)
      case _ => false
   }

   def union( that: SpanLike ) : SpanLike = that match {
      case SpanFrom(  thatStart )      => SpanFrom( math.min( start, thatStart ))
      case SpanUntil( thatStop )       => SpanUntil( math.max( stop, thatStop ))
      case Span( thatStart, thatStop ) => Span( math.min( start, thatStart ), math.max( stop, thatStop ))
      case VoidSpan                    => this
      case AllSpan                     => AllSpan
   }

   def intersect( that: SpanLike ) : ClosedSpan = that match {
      case SpanFrom( thatStart ) =>
         val maxStart   = math.max( start, thatStart )
         if( maxStart <= stop ) Span( maxStart, stop ) else VoidSpan
      case SpanUntil( thatStop ) =>
         val minStop    = math.min( stop, thatStop )
         if( start <= minStop ) Span( start, minStop ) else VoidSpan
      case Span( thatStart, thatStop ) =>
         val maxStart   = math.max( start, thatStart )
         val minStop    = math.min( stop, thatStop )
         if( maxStart <= minStop ) Span( maxStart, minStop ) else VoidSpan
      case VoidSpan  => VoidSpan
      case AllSpan   => this
   }

   def overlaps( that: SpanLike ) : Boolean = that match {
      case SpanFrom( thatStart ) =>
         val maxStart = math.max( start, thatStart )
         maxStart < stop
      case SpanUntil( thatStop ) =>
         val minStop = math.min( stop, thatStop )
         start < minStop
      case Span( thatStart, thatStop ) =>
         val maxStart   = math.max( start, thatStart )
         val minStop    = math.min( stop, thatStop )
         maxStart < minStop
      case VoidSpan  => false
      case AllSpan   => true
   }

   def touches( that: SpanLike ) : Boolean = that match {
      case SpanFrom( thatStart ) =>
         val maxStart = math.max( start, thatStart )
         maxStart <= stop
      case SpanUntil( thatStop ) =>
         val minStop = math.min( stop, thatStop )
         start <= minStop
      case Span( thatStart, thatStop ) =>
         val maxStart   = math.max( start, thatStart )
         val minStop    = math.min( stop, thatStop )
         maxStart <= minStop
      case VoidSpan  => false
      case AllSpan   => true
   }

//   // where overlapping results in negative spacing
//   def spacing( b: Span ) : Long = {
//      val bStart = b.start
//      if( start < bStart ) {
//         bStart - stop
//      } else {
//         start - b.stop
//      }
//   }
}
