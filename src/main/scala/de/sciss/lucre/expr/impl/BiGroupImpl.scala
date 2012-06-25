/*
 *  BiGroupImpl.scala
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

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.{event => evt, DataInput, DataOutput}
import evt.{Change, Event, EventLike}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import txn.{SpaceSerializers, SkipOctree}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import annotation.switch
import de.sciss.collection.geom.{LongDistanceMeasure2D, LongRectangle, LongPoint2DLike, LongPoint2D, LongSquare}
import de.sciss.collection.geom.LongSpace.TwoDim

object BiGroupImpl {
   import BiGroup.{Leaf, TimedElem, Var}

//   var VERBOSE = true

   private val MAX_SQUARE  = LongSquare( 0, 0, 0x2000000000000000L )
   private val MIN_COORD   = MAX_SQUARE.left
   private val MAX_COORD   = MAX_SQUARE.right
   private val MAX_SIDE    = MAX_SQUARE.side

//   private final case class Entry[ Elem ]( )

   private type Tree[ S <: Sys[ S ], Elem ] = SkipOctree[ S, TwoDim, Leaf[ S, Elem ]]

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   private def spanToPoint( span: SpanLike ) : LongPoint2D = span match {
      case Span( start, stop )=> LongPoint2D( start, stop )
      case Span.From( start ) => LongPoint2D( start, MAX_COORD )
      case Span.Until( stop ) => LongPoint2D( MIN_COORD, stop )
      case Span.All           => LongPoint2D( MIN_COORD, MAX_COORD )
      case Span.Void          => LongPoint2D( MAX_COORD, MIN_COORD )  // ??? what to do with this case ??? forbid?
   }

   private def searchSpanToPoint( span: SpanLike ) : LongPoint2D = span match {
      case Span( start, stop )=> LongPoint2D( start, stop )
      case Span.From( start ) => LongPoint2D( start, MAX_COORD + 1 )
      case Span.Until( stop ) => LongPoint2D( MIN_COORD, stop )
      case Span.All           => LongPoint2D( MIN_COORD, MAX_COORD + 1 )
      case Span.Void          => LongPoint2D( MAX_COORD, MIN_COORD )  // ??? what to do with this case ??? forbid?
   }

   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = {

      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint( tup._1 )
      implicit val hyperSer   = SpaceSerializers.LongSquareSerializer
      implicit val exprSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, SpanLike ]] = spanType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipOctree.empty[ S, TwoDim, Leaf[ S, Elem ]]( MAX_SQUARE )
      new ImplNew( evt.Targets[ S ], tree, eventView )
   }

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      spanType: Type[ SpanLike ]) : evt.NodeSerializer[ S , BiGroup[ S, Elem, U ]] = new Ser( eventView )

   def varSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      spanType: Type[ SpanLike ]) : evt.NodeSerializer[ S , BiGroup.Var[ S, Elem, U ]] = new VarSer( eventView )

   def readGenericVar[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
         ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
           spanType: Type[ SpanLike ]) : BiGroup.Var[ S, Elem, U ] = {

      val targets = evt.Targets.read[ S ]( in, access )
      read( in, access, targets, eventView )
   }

   private def read[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ], eventView: Elem => EventLike[ S, U, Elem ])
                                             ( implicit tx: S#Tx,
                                               elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                               spanType: Type[ SpanLike ]) : Impl[ S, Elem, U ] = {
      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint( tup._1 )
      implicit val hyperSer   = SpaceSerializers.LongSquareSerializer
      implicit val exprSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, SpanLike ]] = spanType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipOctree.read[ S, TwoDim, Leaf[ S, Elem ]]( in, access )
      new ImplNew( targets, tree, eventView )
   }

   private class Ser[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                spanType: Type[ SpanLike ])
   extends evt.NodeSerializer[ S, BiGroup[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiGroup[ S, Elem, U ] = {
         BiGroupImpl.read( in, access, targets, eventView )
      }
   }

   private class VarSer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                                 ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                   spanType: Type[ SpanLike ])
   extends evt.NodeSerializer[ S, BiGroup.Var[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiGroup.Var[ S, Elem, U ] = {
         BiGroupImpl.read( in, access, targets, eventView )
      }
   }

      // ... accepted are points with x > LRP || y > LRP ...
      private val advanceNNMetric   = LongDistanceMeasure2D.nextSpanEvent( MAX_SQUARE )
//   private val advanceNNMetric   = LongDistanceMeasure2D.vehsybehc.exceptOrthant( 1 )

   private val regressNNMetric   = LongDistanceMeasure2D.prevSpanEvent( MAX_SQUARE )

   private sealed trait Impl[ S <: Sys[ S ], Elem, U ]
   extends Var[ S, Elem, U ]
//   with evt.Compound[ S, Impl[ S, Elem, U ], Impl.type ]
//   with evt.Trigger.Impl[ S, BiGroup.Update[ S, Elem, U ], BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]
//   with evt.StandaloneLike[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]
   with evt.Node[ S ]
   {
      group =>

      protected def tree: Tree[ S, Elem ]
      protected def eventView: Elem => EventLike[ S, U, Elem ]
      implicit protected def elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]
      implicit protected def spanType: Type[ SpanLike ]

      override def toString() = "BiGroup" + tree.id

      // ---- event behaviour ----

      private object CollChanged
      extends evt.Trigger.Impl[ S, BiGroup.Collection[ S, Elem, U ], BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
      with evt.EventImpl[ S, BiGroup.Collection[ S, Elem, U ], BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
//      with evt.Root[ S, BiGroup.Collection[ S, Elem, U ]]
      {
         protected def reader : evt.Reader[ S, BiGroup[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 1
         def node: evt.Node[ S ] = group

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Expr[ S, SpanLike ])( implicit tx: S#Tx ) {
            elem.changed ---> this
         }

         def -=( elem: Expr[ S, SpanLike ])( implicit tx: S#Tx ) {
            elem.changed -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiGroup.Collection[ S, Elem, U ]] = {
            val par = pull.parents( this )
            if( par.isEmpty ) {  // add or remove
               pull.resolve[ BiGroup.Collection[ S, Elem, U ]]
            } else {             // span key changed
               val changes: IIdxSeq[ (Change[ SpanLike ], Elem) ] = par.flatMap( sel => {
                  val span = sel.devirtualize( spanType.serializer[ S ].asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]])
                     .node.asInstanceOf[ Expr[ S, SpanLike ]]
                  val changeOption = span.changed.pullUpdate( pull )
                  // somehow the flatMap is shadowed in Option, so the implicit conversion
                  // to Iterable doesn't kick in...
                  (changeOption: Iterable[ Change[ SpanLike ]]).flatMap({ case change @ Change( spanValOld, spanValNew ) =>
                     val pointOld = spanToPoint( spanValOld )
                     (tree.get( pointOld ): Iterable[ Leaf[ S, Elem ]]).flatMap({ case (_, seq) =>
                        val moved: IIdxSeq[ Elem ] = seq.collect { case (span2, elem) if span2 == span => elem }
                        // update in spatial structure
                        moved.foreach { elem =>
                           removeNoFire( spanValOld, span, elem )
                           addNoFire(    spanValNew, span, elem )
                        }
                        moved.map( elem => (change, elem) )
                     })
                  })
               })( breakOut )
               if( changes.isEmpty ) None else Some( BiGroup.Moved( group, changes ))
            }
         }
      }

      private object ElemChanged
      extends evt.EventImpl[ S, BiGroup.Element[ S, Elem, U ], BiGroup.Element[ S, Elem, U ], BiGroup[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiGroup.Element[ S, Elem, U ], BiGroup[ S, Elem, U ]] {
         protected def reader : evt.Reader[ S, BiGroup[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 2
         def node: evt.Node[ S ] = group

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) ---> this
         }

         def -=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiGroup.Element[ S, Elem, U ]] = {
            val changes: IIdxSeq[ (Elem, U) ] = pull.parents( this ).flatMap( sel => {
//               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
               val elem = sel.devirtualize( elemSerializer.asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]]).node.
                  asInstanceOf[ Elem ]
               eventView( elem ).pullUpdate( pull ).map( u => (elem, u) )
            })( breakOut )

            if( changes.isEmpty ) None else Some( BiGroup.Element( group, changes ))
         }
      }

      private object Changed
      extends evt.Event[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, BiGroup[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = opNotSupported
         def node: evt.Node[ S ] = group

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         private[lucre] def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged ---> r
            ElemChanged ---> r
         }
         private[lucre] def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged -/-> r
            ElemChanged -/-> r
         }

         private[lucre] def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiGroup.Update[ S, Elem, U ]] = {
            if( CollChanged.isSource( pull )) CollChanged.pullUpdate( pull )
            else if( ElemChanged.isSource( pull )) ElemChanged.pullUpdate( pull )
            else None
         }

         def react( fun: BiGroup.Update[ S, Elem, U ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]] =
            reactTx( _ => fun )

         def reactTx( fun: S#Tx => BiGroup.Update[ S, Elem, U ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]] = {
            val obs = evt.Observer( serializer( eventView ), fun )
            obs.add( CollChanged )
            obs.add( ElemChanged )
            obs
         }

         private[lucre] def isSource( pull: evt.Pull[ S ]) : Boolean = opNotSupported
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         tree.dispose()
      }

      final protected def writeData( out: DataOutput ) {
         tree.write( out )
      }

      private def foreach( fun: TimedElem[ S, Elem ] => Unit )( implicit tx: S#Tx ) {
         tree.iterator.foreach { case (_, seq) => seq.foreach( fun )}
      }

      final def connect()( implicit tx: S#Tx ) {
         foreach { case (span, elem) =>
            CollChanged += span
            ElemChanged += elem
         }
      }

      final def disconnect()( implicit tx: S#Tx ) {
         foreach { case (span, elem) =>
            CollChanged -= span
            ElemChanged -= elem
         }
      }

      final def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = (slot: @switch) match {
         case 1 => CollChanged
         case 2 => ElemChanged
      }

      // ---- collection behaviour ----

      @inline private def isConnected( implicit tx: S#Tx ) : Boolean = targets.nonEmpty

      final def clear()( implicit tx: S#Tx ) {
         if( isConnected ) {
            val changes = tree.iterator.toIndexedSeq.flatMap { case (spanVal, seq) =>
               seq.map { case (_, elem) => BiGroup.Removed( this, spanVal, elem )}
            }
            tree.clear()
            changes.foreach( CollChanged.apply )

         } else {
            tree.clear()
         }
      }

      final def add( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) {
         val spanVal = span.value
         addNoFire( spanVal, span, elem )
         if( isConnected ) {
            CollChanged += span
            ElemChanged += elem
            CollChanged( BiGroup.Added( this, spanVal, elem ))
         }
      }

      private def addNoFire( spanVal: SpanLike, span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) {
         val point   = spanToPoint( spanVal )
//if( VERBOSE ) println( "add at point " + point )
         val entry   = (span, elem)
         tree.transformAt( point ) {
            case None               => Some( spanVal -> IIdxSeq( entry ))
            case Some( (_, seq) )   => Some( spanVal -> (seq :+ entry) )
         }
      }

      final def remove( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val spanVal = span.value
         val res     = removeNoFire( spanVal, span, elem )
         if( res && isConnected ) {
            CollChanged -= span
            ElemChanged -= elem
            CollChanged( BiGroup.Removed( this, spanVal, elem ))
         }
         res
      }

      private def removeNoFire( spanVal: SpanLike, span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val point   = spanToPoint( spanVal )
         val entry   = (span, elem)
         tree.get( point ) match {
            case Some( (_, IIdxSeq( single ))) =>
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

//      final def iterator( implicit tx: S#Tx, chr: Chronos[ S ]) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]  =
//         intersect( chr.time.value )

      final def intersect( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
         val start   = time
         val stop    = time + 1
//         val shape = Rectangle( ti, MIN_COORD, MAX_COORD - ti + 1, ti - MIN_COORD + 1 )
         // horizontally: until query_stop; vertically: from query_start
         // start < query.stop && stop > query.start
         val shape = LongRectangle( MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start )
         rangeSearch( shape )
      }

      final def intersect( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
         // horizontally: until query_stop; vertically: from query_start
         span match {
            case Span( start, stop ) =>
               val shape = LongRectangle( MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start )
               rangeSearch( shape )

            case Span.From( start ) =>
               val shape = LongRectangle( MIN_COORD, start + 1, MAX_SIDE, MAX_COORD - start )
               rangeSearch( shape )

            case Span.Until( stop ) =>
               val shape = LongRectangle( MIN_COORD, MIN_COORD, stop - MIN_COORD, MAX_SIDE )
               rangeSearch( shape )

            case Span.All  => tree.iterator
            case Span.Void => txn.Iterator.empty
         }
      }

      final def rangeSearch( start: SpanLike, stop: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
         if( start == Span.Void || stop == Span.Void ) return txn.Iterator.empty

         val startP  = searchSpanToPoint( start )
         val stopP   = searchSpanToPoint( stop  )
         val shape   = LongRectangle( startP.x, stopP.x, startP.y - startP.x /* + 1 */, stopP.y - stopP.x /* + 1 */)
//println( "RANGE " + shape )
         rangeSearch( shape )
      }

      // this can be easily implemented with two rectangular range searches
      final def eventsAt( time: Long )( implicit tx: S#Tx ) : (txn.Iterator[ S#Tx, Leaf[ S, Elem ]], txn.Iterator[ S#Tx, Leaf[ S, Elem ]]) = {
         val startShape = LongRectangle( time, MIN_COORD, 1, MAX_SIDE )
         val stopShape  = LongRectangle( MIN_COORD, time, MAX_SIDE, 1 )
         (rangeSearch( startShape ), rangeSearch( stopShape ))
      }

      final def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = {
         val point   = LongPoint2D( time, time ) // + 1
         val span    = tree.nearestNeighborOption( point, advanceNNMetric ).map( _._1 ).getOrElse( Span.Void )
         span match {
            case sp @ Span.From( start ) => assert( start >= time, sp ); Some( start ) // else None
            case sp @ Span.Until( stop ) => assert( stop  >= time, sp ); Some( stop  ) // else None
            case sp @ Span( start, stop ) =>
               if( start >= time ) {
                  Some( start )
               } else {
                  assert( stop >= time, sp ); Some( stop )
               }
            case _ => None // All or Void
         }
      }

      final def nearestEventBefore( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = {
         val point   = LongPoint2D( time, time )
         val span    = tree.nearestNeighborOption( point, regressNNMetric ).map( _._1 ).getOrElse( Span.Void )
         span match {
            case sp @ Span.From( start ) => assert( start <= time, sp ); Some( start ) // else None
            case sp @ Span.Until( stop ) => assert( stop  <= time, sp ); Some( stop  ) // else None
            case sp @ Span( start, stop ) =>
               if( stop <= time ) {
                  Some( stop )
               } else {
                  assert( start <= time, sp ); Some( start )
               }
            case _ => None // All or Void
         }
      }

      private def rangeSearch( shape: LongRectangle )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
         val res = tree.rangeQuery( shape ) // .flatMap ....
//if( VERBOSE ) println( "Range in " + shape + " --> right = " + shape.right + "; bottom = " + shape.bottom + " --> found some? " + !res.isEmpty )
         res
      }

      final def collectionChanged : Event[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]] = CollChanged
      final def elementChanged    : Event[ S, BiGroup.Element[    S, Elem, U ], BiGroup[ S, Elem, U ]] = ElemChanged
      final def changed           : Event[ S, BiGroup.Update[     S, Elem, U ], BiGroup[ S, Elem, U ]] = Changed
   }

   private final class ImplNew[ S <: Sys[ S ], Elem, U ]( protected val targets: evt.Targets[ S ],
                                                          protected val tree: Tree[ S, Elem ],
                                                          protected val eventView: Elem => EventLike[ S, U, Elem ])
                                                        ( implicit protected val elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                          protected val spanType: Type[ SpanLike ])
   extends Impl[ S, Elem, U ]
}
