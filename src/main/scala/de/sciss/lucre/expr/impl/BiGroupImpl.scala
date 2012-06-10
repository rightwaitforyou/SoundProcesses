package de.sciss.lucre.expr
package impl

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import de.sciss.collection.geom.{Point2D, Point2DLike, Square, Space}
import Space.TwoDim
import txn.{SpaceSerializers, SkipOctree}

/**
 * TODO need a Long based 2D space
 */
object BiGroupImpl {
   import BiGroup.Var

   private val MAX_SQUARE  = Square( 0, 0, 0x40000000 )
   private val MIN_COORD   = MAX_SQUARE.left
   private val MAX_COORD   = MAX_SQUARE.right

//   private final case class Entry[ Elem ]( )

   def newVar[ S <: Sys[ S ], Elem, U <: EventLike[ S, _, Elem ]]( eventView: Elem => U )(
      implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ]) : Var[ S, Elem, U ] = {

      implicit val pointView: ((SpanLike, Elem), S#Tx) => Point2DLike = (tup, tx) => tup._1 match {
         case Span( start, stop )=> Point2D( start.toInt, (stop + 1).toInt )
         case Span.From( start ) => Point2D( start.toInt, MAX_COORD )
         case Span.Until( stop ) => Point2D( MIN_COORD, (stop + 1).toInt )
         case Span.All           => Point2D( MIN_COORD, MAX_COORD )
         case Span.Void          => Point2D( MAX_COORD, MIN_COORD )  // ??? what to do with this case ??? forbid?
      }
      implicit val hyperSer = SpaceSerializers.SquareSerializer
      val tree = SkipOctree.empty[ S, TwoDim, (SpanLike, Elem) ]( MAX_SQUARE )
      new ImplNew( tree, eventView )
   }

   private sealed trait Impl[ S <: Sys[ S ], Elem, U <: EventLike[ S, _, Elem ]]
   extends Var[ S, Elem, U ] {
      protected def tree: SkipOctree[ S, TwoDim, (SpanLike, Elem) ]

//      final def add( time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) {
//         sys.error( "TODO" )
//      }
//      final def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Option[ Elem ] = {
//         sys.error( "TODO" )
//      }

      final def iterator( implicit tx: S#Tx, time: Chronos[ S ]) : txn.Iterator[ S#Tx, (SpanLike, Elem) ] =
         sys.error( "TODO" )

      final def iteratorAt( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Elem) ] =
         sys.error( "TODO" )

      final def iteratorWithin( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Elem) ] =
         sys.error( "TODO" )

      final def changed : Event[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]] = sys.error( "TODO" )
   }

   private final class ImplNew[ S <: Sys[ S ], Elem, U <: EventLike[ S, _, Elem ]](
      protected val tree: SkipOctree[ S, TwoDim, (SpanLike, Elem) ], eventView: Elem => U )
   extends Impl[ S, Elem, U ] {

   }
}
