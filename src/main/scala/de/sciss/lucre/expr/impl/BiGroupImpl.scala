package de.sciss.lucre.expr
package impl

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import txn.{SpaceSerializers, SkipOctree}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.collection.geom.{Rectangle, Point2D, Point2DLike, Square, Space}
import Space.TwoDim

/**
 * TODO need a Long based 2D space
 */
object BiGroupImpl {
   import BiGroup.Var

   private val MAX_SQUARE  = Square( 0, 0, 0x40000000 )
   private val MIN_COORD   = MAX_SQUARE.left
   private val MAX_COORD   = MAX_SQUARE.right

//   private final case class Entry[ Elem ]( )

   private type Leaf[ S <: Sys[ S ], Elem ] = (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Elem) ])
   private type Tree[ S <: Sys[ S ], Elem ] = SkipOctree[ S, TwoDim, Leaf[ S, Elem ]]

   private def spanToPoint( span: SpanLike ) : Point2D = span match {
      case Span( start, stop )=> Point2D( start.toInt, (stop + 1).toInt )
      case Span.From( start ) => Point2D( start.toInt, MAX_COORD )
      case Span.Until( stop ) => Point2D( MIN_COORD, (stop + 1).toInt )
      case Span.All           => Point2D( MIN_COORD, MAX_COORD )
      case Span.Void          => Point2D( MAX_COORD, MIN_COORD )  // ??? what to do with this case ??? forbid?
   }

   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ], spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = {

      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => Point2DLike = (tup, tx) => spanToPoint( tup._1 )
      implicit val hyperSer   = SpaceSerializers.SquareSerializer
      implicit val exprSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, SpanLike ]] = spanType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipOctree.empty[ S, TwoDim, Leaf[ S, Elem ]]( MAX_SQUARE )
      new ImplNew( tree, eventView )
   }

   private sealed trait Impl[ S <: Sys[ S ], Elem, U ]
   extends Var[ S, Elem, U ] {
      protected def tree: Tree[ S, Elem ]

      override def toString = "BiGroup" + tree.id

      final def add( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) {
         val spanVal = span.value
         val point   = spanToPoint( spanVal )
         val entry   = (span, elem)
         tree.transformAt( point ) {
            case None               => Some( spanVal -> IIdxSeq( entry ))
            case Some( (_, seq) )   => Some( spanVal -> (seq :+ entry) )
         }
      }
      final def remove( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val spanVal = span.value
         val point   = spanToPoint( spanVal )
         val entry   = (span, elem)
         tree.get( point ) match {
            case Some( (_, IIdxSeq( single )) ) =>
               if( single == entry ) {
                  tree.removeAt( point )
                  true
               } else {
                  false
               }
            case Some( (_, seq) ) =>
               val seqNew = seq.filterNot( _ == entry )
               if( seqNew.size != seq.size ) {
                  tree.add( (spanVal, seqNew) )
                  true
               } else {
                  false
               }
            case None => false
         }
      }

      final def debugList()( implicit tx: S#Tx ) : List[ (SpanLike, Elem) ] =
         tree.toList.flatMap { case (span, seq) => seq.map { case (_, elem) => span -> elem }}

      final def iterator( implicit tx: S#Tx, time: Chronos[ S ]) : txn.Iterator[ S#Tx, (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Elem) ])]  =
         iteratorAt( time.time.value )

      final def iteratorAt( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Elem) ])] = {
         val ti = time.toInt
         val shape = Rectangle( ti, MIN_COORD, MAX_COORD - ti + 1, ti - MIN_COORD + 1 )
         tree.rangeQuery( shape ) // .flatMap ....
      }

      final def iteratorWithin( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Elem) ])] =
         sys.error( "TODO" )

      final def changed : Event[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]] = sys.error( "TODO" )
   }

   private final class ImplNew[ S <: Sys[ S ], Elem, U ](
      protected val tree: Tree[ S, Elem ], eventView: Elem => EventLike[ S, U, Elem ])
   extends Impl[ S, Elem, U ] {

   }
}
