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
   def from( start: Long ) : From                     = From( start )
   def until( stop: Long ) : Until                    = Until( stop )
   def apply( start: Long, stop: Long ) : Span        = Apply( start ,stop )
   def unapply( span: Span ) : Option[ (Long, Long) ] = Some( (span.start, span.stop) )

   sealed trait Open extends SpanLike {
      final def isEmpty    = false
      final def nonEmpty   = true
   
      def shift( delta: Long ) : Open
      def union( that: SpanLike ) : Open
   }
   case object All extends Open {
      def shift( delta: Long ) : Open = this
      def union( that: SpanLike ) : Open = this
      def intersect( that: SpanLike ) : SpanLike = that
      def clip( pos: Long ) : Long = pos
   
      def contains( pos: Long )        = true
      def contains( that: SpanLike )   = true
      def overlaps( that: SpanLike )   = true
      def touches( that: SpanLike )    = true
   }
   final case class From( start: Long ) extends Open {
      def clip( pos: Long ) : Long = math.max( start, pos )
      def shift( delta: Long ) : Open = From( start + delta )
   
      def contains( pos: Long ) : Boolean = pos >= start
   
      def contains( that: SpanLike ) : Boolean = that match {
         case From( thatStart )     => thatStart >= start
         case Span( thatStart, _ )  => thatStart >= start
         case _                     => false
      }
   
      def overlaps( that: SpanLike ) : Boolean = that match {
         case From( _ )             => true
         case Until( thatStop )     => start < thatStop
         case Span( _, thatStop )   => start < thatStop
         case Void                  => false
         case All                   => true
      }
   
      def touches( that: SpanLike ) : Boolean = that match {
         case From( _ )             => true
         case Until( thatStop )     => start <= thatStop
         case Span( _, thatStop )   => start <= thatStop
         case Void                  => false
         case All                   => true
      }
   
      def union( that: SpanLike ) : Open = that match {
         case From( thatStart )     => From( math.min( start, thatStart ))
         case Span( thatStart, _ )  => From( math.min( start, thatStart ))
         case Void                  => this
         case _                     => All
      }
   
      def intersect( that: SpanLike ) : SpanLike = that match {
         case From( thatStart ) => From( math.max( start, thatStart ))
         case Until( thatStop ) => if( start <= thatStop ) Span( start, thatStop ) else Void
         case Span( thatStart, thatStop ) =>
            val maxStart = math.max( start, thatStart )
            if( maxStart <= thatStop ) Span( maxStart, thatStop ) else Void
         case Void  => Void
         case All   => this
      }
   }
   final case class Until( stop: Long ) extends Open {
      def clip( pos: Long ) : Long = math.min( stop, pos )
      def shift( delta: Long ) : Open = Until( stop + delta )
   
      def contains( pos: Long ) : Boolean = pos < stop
   
      def contains( that: SpanLike ) : Boolean = that match {
         case Until( thatStop )     => thatStop <= stop
         case Span( _, thatStop )   => thatStop <= stop
         case _                     => false
      }
   
      def overlaps( that: SpanLike ) : Boolean = that match {
         case Until( _ )            => true
         case From( thatStart )     => thatStart < stop
         case Span( thatStart, _ )  => thatStart < stop
         case Void                  => false
         case All                   => true
      }
   
      def touches( that: SpanLike ) : Boolean = that match {
         case Until( _ )            => true
         case From( thatStart )     => thatStart <= stop
         case Span( thatStart, _ )  => thatStart <= stop
         case Void                  => false
         case All                   => true
      }
   
      def union( that: SpanLike ) : Open = that match {
         case Until( thatStop )     => Until( math.max( stop, thatStop ))
         case Span( _, thatStop )   => Until( math.max( stop, thatStop ))
         case Void                  => this
         case _                     => All
      }
   
      def intersect( that: SpanLike ) : SpanLike = that match {
         case From( thatStart ) => if( thatStart <= stop ) Span( thatStart, stop ) else Void
         case Until( thatStop ) => Until( math.min( stop, thatStop ))
         case Span( thatStart, thatStop ) =>
            val minStop = math.min( stop, thatStop )
            if( thatStart <= minStop ) Span( thatStart, minStop ) else Void
         case Void  => Void
         case All   => this
      }
   }
   sealed trait Closed extends SpanLike {
      def length: Long
   
      def shift( delta: Long ) : Closed
      def intersect( that: SpanLike ) : Closed
   }
   case object Void extends Closed {
      val length = 0L
   
      def shift( delta: Long ) : Closed = this
      def union( that: SpanLike ) : SpanLike = that
      def intersect( that: SpanLike ) : Closed = this
      def clip( pos: Long ) : Long = pos
   
      def contains( pos: Long )        = false
      def contains( that: SpanLike )   = false
      def overlaps( that: SpanLike )   = false
      def touches( that: SpanLike )    = false
   
      val isEmpty    = true
      val nonEmpty   = false
   }

   private final case class Apply( start: Long, stop: Long ) extends Span {
      require( start <= stop, "A span's start (" + start + ") must be <= its stop (" + stop + ")" )

      override def toString = "Span(" + start + "," + stop + ")"

      def length: Long = stop - start

      def contains( pos: Long ) : Boolean = pos >= start && pos < stop

      def shift( delta: Long ) : Span.Closed = Span( start + delta, stop + delta )

      def clip( pos: Long ) : Long = math.max( start, math.min( stop, pos ))

      def isEmpty : Boolean = start == stop

      def nonEmpty : Boolean = start != stop

      def contains( that: SpanLike ) : Boolean = that match {
         case Span( thatStart, thatStop ) => (thatStart >= start) && (thatStop <= stop)
         case _ => false
      }

      def union( that: SpanLike ) : SpanLike = that match {
         case Span.From(  thatStart )     => Span.From( math.min( start, thatStart ))
         case Span.Until( thatStop )      => Span.Until( math.max( stop, thatStop ))
         case Span( thatStart, thatStop ) => Span( math.min( start, thatStart ), math.max( stop, thatStop ))
         case Span.Void                   => this
         case Span.All                    => Span.All
      }

      def intersect( that: SpanLike ) : Span.Closed = that match {
         case Span.From( thatStart ) =>
            val maxStart   = math.max( start, thatStart )
            if( maxStart <= stop ) Span( maxStart, stop ) else Span.Void
         case Span.Until( thatStop ) =>
            val minStop    = math.min( stop, thatStop )
            if( start <= minStop ) Span( start, minStop ) else Span.Void
         case Span( thatStart, thatStop ) =>
            val maxStart   = math.max( start, thatStart )
            val minStop    = math.min( stop, thatStop )
            if( maxStart <= minStop ) Span( maxStart, minStop ) else Span.Void
         case Span.Void  => Span.Void
         case Span.All   => this
      }

      def overlaps( that: SpanLike ) : Boolean = that match {
         case Span.From( thatStart ) =>
            val maxStart = math.max( start, thatStart )
            maxStart < stop
         case Span.Until( thatStop ) =>
            val minStop = math.min( stop, thatStop )
            start < minStop
         case Span( thatStart, thatStop ) =>
            val maxStart   = math.max( start, thatStart )
            val minStop    = math.min( stop, thatStop )
            maxStart < minStop
         case Span.Void  => false
         case Span.All   => true
      }

      def touches( that: SpanLike ) : Boolean = that match {
         case Span.From( thatStart ) =>
            val maxStart = math.max( start, thatStart )
            maxStart <= stop
         case Span.Until( thatStop ) =>
            val minStop = math.min( stop, thatStop )
            start <= minStop
         case Span( thatStart, thatStop ) =>
            val maxStart   = math.max( start, thatStart )
            val minStop    = math.min( stop, thatStop )
            maxStart <= minStop
         case Span.Void  => false
         case Span.All   => true
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
sealed trait Span extends Span.Closed {
   def start: Long
   def stop: Long
}