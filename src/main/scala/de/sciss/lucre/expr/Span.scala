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

import stm.{Writer, Serializer}
import annotation.switch
import collection.immutable.{IndexedSeq => IIdxSeq}

object Span {
   def from( start: Long ) : From                     = From( start )
   def until( stop: Long ) : Until                    = Until( stop )
   def apply( start: Long, stop: Long ) : Span        = Apply( start ,stop )
   def unapply( span: Span ) : Option[ (Long, Long) ] = Some( (span.start, span.stop) )
   def all : All.type                                 = All
   def void : Void.type                               = Void

   implicit object serializer extends Serializer[ Span ] {
      def write( v: Span, out: DataOutput ) { v.write( out )}
      def read( in: DataInput ) : Span = Span.read( in )
   }

   def read( in: DataInput ) : Span = {
      val cookie = in.readUnsignedByte()
      require( cookie == 0, "Unexpected cookie " + cookie )
      Span( in.readLong(), in.readLong() )
   }

   sealed trait Open extends SpanLike {
      final def isEmpty    = false
      final def nonEmpty   = true
   
      def shift( delta: Long ) : Open
      def union( that: SpanLike ) : Open
      def invert : SpanLike
   }
   case object All extends Open {
      def shift( delta: Long ) : All.type = this
      def union( that: SpanLike ) : All.type = this
      def intersect( that: SpanLike ) : SpanLike = that
      def clip( pos: Long ) : Long = pos
      def invert : Void.type = Void
   
      def contains( pos: Long )        = true
      def contains( that: SpanLike )   = that != Void
      def overlaps( that: SpanLike )   = that match {
         case sp: Span  => sp.nonEmpty
         case Void      => false
         case _         => true
      }
      def touches( that: SpanLike )    = that != Void

      def subtract( that: Span.Open ) : SpanLike = that.invert
      def subtract( that: SpanLike ) : IIdxSeq[ SpanLike ] = IIdxSeq.empty

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
      }
   }
   final case class From( start: Long ) extends Open {
      def clip( pos: Long ) : Long = math.max( start, pos )
      def shift( delta: Long ) : From = From( start + delta )
      def invert : Until = Until( start )
   
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

      def subtract( that: Span.Open ) : SpanLike = that match {
         case Span.From( thatStart ) =>
            if( thatStart > start ) Span( start, thatStart ) else Span.Void
         case Span.Until( thatStop ) if thatStop > start => From( thatStop )
         case Span.All   => Span.Void
         case _          => this
      }

      def subtract( that: SpanLike ) : IIdxSeq[ SpanLike ] = that match {
         case Span.From( thatStart ) =>
            if( thatStart > start ) IIdxSeq( Span( start, thatStart )) else IIdxSeq.empty
         case Span.Until( thatStop ) if thatStop > start => IIdxSeq( From( thatStop ))
         case Span( thatStart, thatStop ) if thatStop > start =>
            if( thatStart <= start ) {
               IIdxSeq( From( thatStop ))
            } else {
               IIdxSeq( Span( start, thatStart ), From( thatStop ))
            }
         case Span.All   => IIdxSeq.empty
         case _          => IIdxSeq( this )
      }

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
         out.writeLong( start )
      }
   }
   final case class Until( stop: Long ) extends Open {
      def clip( pos: Long ) : Long = math.min( stop, pos )
      def shift( delta: Long ) : Until = Until( stop + delta )
      def invert : From = From( stop )
   
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

      def subtract( that: Span.Open ) : SpanLike = that match {
         case Span.From( thatStart ) if thatStart < stop => Until( thatStart )
         case Span.Until( thatStop ) =>
            if( thatStop < stop ) Span( thatStop, stop ) else Span.Void
         case Span.All   => Span.Void
         case _          => this
      }

      def subtract( that: SpanLike ) : IIdxSeq[ SpanLike ] = that match {
         case Span.From( thatStart ) if thatStart < stop => IIdxSeq( Until( thatStart ))
         case Span.Until( thatStop ) =>
            if( thatStop < stop ) IIdxSeq( Span( thatStop, stop )) else IIdxSeq.empty
         case Span( thatStart, thatStop ) if thatStart < stop =>
            if( thatStop >= stop ) {
               IIdxSeq( Until( thatStart ))
            } else {
               IIdxSeq( Until( thatStart ), Span( thatStop, stop ))
            }
         case Span.All   => IIdxSeq.empty
         case _          => IIdxSeq( this )
      }

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 2 )
         out.writeLong( stop )
      }
   }
   sealed trait Closed extends SpanLike {
      def length: Long
   
      def shift( delta: Long ) : Closed
      def intersect( that: SpanLike ) : Closed
      def subtract( that: Span.Open ) : Closed
      def subtract( that: SpanLike ) : IIdxSeq[ Closed ]
   }
   case object Void extends Closed {
      val length = 0L
   
      def shift( delta: Long ) : Void.type = this
      def union( that: SpanLike ) : SpanLike = that
      def invert : All.type = All

      def intersect( that: SpanLike ) : Void.type = this
      def subtract( that: Span.Open ) : Void.type = this
      def subtract( that: SpanLike ) : IIdxSeq[ Closed ] = IIdxSeq.empty
      def clip( pos: Long ) : Long = pos
   
      def contains( pos: Long )        = false
      def contains( that: SpanLike )   = false
      def overlaps( that: SpanLike )   = false
      def touches( that: SpanLike )    = false
   
      val isEmpty    = true
      val nonEmpty   = false

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 4 )
      }
   }

   private final case class Apply( start: Long, stop: Long ) extends Span {
      if( start > stop ) throw new IllegalArgumentException( "A span's start (" + start + ") must be <= its stop (" + stop + ")" )

      override def toString = "Span(" + start + "," + stop + ")"

      def length: Long = stop - start

      def contains( pos: Long ) : Boolean = pos >= start && pos < stop

      def shift( delta: Long ) : Span = Span( start + delta, stop + delta )

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
         case Span.All   => nonEmpty
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

      def subtract( that: Span.Open ) : Span.Closed = that match {
         case Span.From( thatStart ) if thatStart < stop =>
            if( thatStart > start ) Span( start, thatStart ) else Span.Void
         case Span.Until( thatStop ) if( thatStop > start ) =>
            if( thatStop < stop ) Span( thatStop, stop ) else Span.Void
         case Span.All   => Span.Void
         case _          => this
      }

      def subtract( that: SpanLike ) : IIdxSeq[ Span.Closed ] = that match {
         case Span.From( thatStart ) if thatStart < stop =>
            if( thatStart > start ) IIdxSeq( Span( start, thatStart )) else IIdxSeq.empty
         case Span.Until( thatStop ) if( thatStop > start ) =>
            if( thatStop < stop ) IIdxSeq( Span( thatStop, stop )) else IIdxSeq.empty
         case Span( thatStart, thatStop ) if thatStart < stop && thatStop > start =>
            if( thatStart <= start ) {
               if( thatStop < stop ) IIdxSeq( Span( thatStop, stop )) else IIdxSeq.empty
            } else if( thatStop >= stop ) {
               IIdxSeq( Span( start, thatStart ))
            } else {
               IIdxSeq( Span( start, thatStart ), Span( thatStop, stop ))
            }
         case Span.All   => IIdxSeq.empty
         case _          => IIdxSeq( this )
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

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 0 )
         out.writeLong( start )
         out.writeLong( stop )
      }
   }
}
object SpanLike {
   implicit object serializer extends Serializer[ SpanLike ] {
      def write( v: SpanLike, out: DataOutput ) { v.write( out )}
      def read( in: DataInput ) : SpanLike = SpanLike.read( in )
   }

   def read( in: DataInput ) : SpanLike = (in.readUnsignedByte(): @switch) match {
      case 0 => Span( in.readLong(), in.readLong() )
      case 1 => Span.from( in.readLong() )
      case 2 => Span.until( in.readLong() )
      case 3 => Span.All
      case 4 => Span.Void
      case cookie => sys.error( "Unrecognized cookie " + cookie )
   }
}
sealed trait SpanLike extends Writer {
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

   def subtract( that: SpanLike ) : IIdxSeq[ SpanLike ]
   def subtract( that: Span.Open ) : SpanLike
}
sealed trait Span extends Span.Closed {
   def start: Long
   def stop: Long
   def shift( delta: Long ) : Span
}