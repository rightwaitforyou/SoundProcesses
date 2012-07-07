/*
 *  BiPinImpl.scala
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
import txn.SkipList
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import annotation.switch

object BiPinImpl {
   import BiPin.{Leaf, TimedElem, Var}

   private type Tree[ S <: Sys[ S ], Elem ] = SkipList.Map[ S, Long, Leaf[ S, Elem ]]

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : Var[ S, Elem, U ] = {

//      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint( tup._1 )
//      implicit val hyperSer   = SpaceSerializers.LongSquareSerializer
      implicit val exprSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, Long ]] = timeType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipList.Map.empty[ S, Long, Leaf[ S, Elem ]]()
      new ImplNew( evt.Targets[ S ], tree, eventView )
   }

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      sys.error( "TODO" )

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : evt.NodeSerializer[ S , BiPin[ S, Elem, U ]] = new Ser( eventView )

   def varSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : evt.NodeSerializer[ S , BiPin.Var[ S, Elem, U ]] = new VarSer( eventView )

   def readGenericVar[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
         ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
           timeType: Type[ Long ]) : BiPin.Var[ S, Elem, U ] = {

      val targets = evt.Targets.read[ S ]( in, access )
      read( in, access, targets, eventView )
   }

   private def read[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ], eventView: Elem => EventLike[ S, U, Elem ])
                                             ( implicit tx: S#Tx,
                                               elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                               timeType: Type[ Long ]) : Impl[ S, Elem, U ] = {
//      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint( tup._1 )
//      implicit val hyperSer   = SpaceSerializers.LongSquareSerializer
      implicit val exprSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, Long ]] = timeType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipList.Map.read[ S, Long, Leaf[ S, Elem ]]( in, access )
      new ImplNew( targets, tree, eventView )
   }

   private class Ser[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                timeType: Type[ Long ])
   extends evt.NodeSerializer[ S, BiPin[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin[ S, Elem, U ] = {
         BiPinImpl.read( in, access, targets, eventView )
      }
   }

   private class VarSer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                                 ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                   timeType: Type[ Long ])
   extends evt.NodeSerializer[ S, BiPin.Var[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin.Var[ S, Elem, U ] = {
         BiPinImpl.read( in, access, targets, eventView )
      }
   }

   private sealed trait Impl[ S <: Sys[ S ], Elem, U ]
   extends Var[ S, Elem, U ]
//   with evt.Compound[ S, Impl[ S, Elem, U ], Impl.type ]
//   with evt.Trigger.Impl[ S, BiPin.Update[ S, Elem, U ], BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
//   with evt.StandaloneLike[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
//   with evt.Node[ S ]
   {
      pin =>

      protected def tree: Tree[ S, Elem ]
      protected def eventView: Elem => EventLike[ S, U, Elem ]
      implicit protected def elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]
      implicit protected def timeType: Type[ Long ]

      override def toString() = "BiPin" + tree.id

      // ---- event behaviour ----

      private object CollChanged
      extends evt.Trigger.Impl[ S, BiPin.Collection[ S, Elem, U ], BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.EventImpl[ S, BiPin.Collection[ S, Elem, U ], BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
//      with evt.Root[ S, BiPin.Collection[ S, Elem, U ]]
      {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 1
         def node: evt.Node[ S ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Expr[ S, Long ])( implicit tx: S#Tx ) {
            elem.changed ---> this
         }

         def -=( elem: Expr[ S, Long ])( implicit tx: S#Tx ) {
            elem.changed -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Collection[ S, Elem, U ]] = {
            val par = pull.parents( this )
            if( par.isEmpty ) {  // add or remove
               pull.resolve[ BiPin.Collection[ S, Elem, U ]]
            } else {             // span key changed
               val changes: IIdxSeq[ (Change[ Long ], Elem) ] = par.flatMap( sel => {
                  val time = sel.devirtualize( timeType.serializer[ S ].asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]])
                     .node.asInstanceOf[ Expr[ S, Long ]]
                  val changeOption = time.changed.pullUpdate( pull )
                  // somehow the flatMap is shadowed in Option, so the implicit conversion
                  // to Iterable doesn't kick in...
                  (changeOption: Iterable[ Change[ Long ]]).flatMap({ case change @ Change( timeValOld, timeValNew ) =>
                     val pointOld = timeValOld // spanToPoint( spanValOld )
                     (tree.get( pointOld ): Iterable[ Leaf[ S, Elem ]]).flatMap { seq =>
                        val moved: IIdxSeq[ Elem ] = seq.collect { case (time2, elem) if time2 == time => elem }
                        // update in spatial structure
                        moved.foreach { elem =>
                           removeNoFire( timeValOld, time, elem )
                           addNoFire(    timeValNew, time, elem )
                        }
                        moved.map( elem => (change, elem) )
                     }
                  })
               })( breakOut )
               if( changes.isEmpty ) None else Some( BiPin.Collection( pin, changes ))
            }
         }
      }

      private object ElemChanged
      extends evt.EventImpl[ S, BiPin.Element[ S, Elem, U ], BiPin.Element[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiPin.Element[ S, Elem, U ], BiPin[ S, Elem, U ]] {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 2
         def node: evt.Node[ S ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) ---> this
         }

         def -=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Element[ S, Elem, U ]] = {
            val changes: IIdxSeq[ (Elem, U) ] = pull.parents( this ).flatMap( sel => {
//               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
               val elem = sel.devirtualize( elemSerializer.asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]]).node.
                  asInstanceOf[ Elem ]
               eventView( elem ).pullUpdate( pull ).map( u => (elem, u) )
            })( breakOut )

            if( changes.isEmpty ) None else Some( BiPin.Element( pin, changes ))
         }
      }

      private object Changed
      extends evt.Event[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = opNotSupported
         def node: evt.Node[ S ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         private[lucre] def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged ---> r
// TODO XXX
//            ElemChanged ---> r
         }
         private[lucre] def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged -/-> r
// TODO XXX
//            ElemChanged -/-> r
         }

         private[lucre] def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Update[ S, Elem, U ]] = {
            if( CollChanged.isSource( pull )) CollChanged.pullUpdate( pull )
            else if( ElemChanged.isSource( pull )) ElemChanged.pullUpdate( pull )
            else None
         }

         def react( fun: BiPin.Update[ S, Elem, U ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]] =
            reactTx( _ => fun )

         def reactTx( fun: S#Tx => BiPin.Update[ S, Elem, U ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]] = {
            val obs = evt.Observer( serializer( eventView ), fun )
            obs.add( CollChanged )
// TODO XXX
//            obs.add( ElemChanged )
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
         foreach { case (time, elem) =>
            CollChanged += time
// TODO XXX
//            ElemChanged += elem
         }
      }

      final def disconnect()( implicit tx: S#Tx ) {
         foreach { case (time, elem) =>
            CollChanged -= time
// TODO XXX
//            ElemChanged -= elem
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
//            val changes = tree.iterator.toIndexedSeq.flatMap { case (spanVal, seq) =>
//               seq.map { case (_, elem) => BiPin.Removed( this, spanVal, elem )}
//            }
//            tree.clear()
//            changes.foreach( CollChanged.apply )
            sys.error( "TODO" )

         } else {
            tree.clear()
         }
      }

      final def add( time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) {
         val timeVal = time.value
         val span    = addNoFire( timeVal, time, elem )
         if( isConnected ) {
            CollChanged += time
// TODO XXX
//            ElemChanged += elem
            CollChanged( BiPin.Collection( pin, IIdxSeq( span -> elem )))
         }
      }

      private def addNoFire( timeVal: Long, time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : SpanLike = {
         (tree.floor( timeVal ), tree.ceil( timeVal + 1 )) match {
            case (Some( (start, startLeaf) ), Some( (stop, stopLeaf) )) =>
               if( start == timeVal ) {
                  tree += timeVal -> ((time -> elem) +: startLeaf)
               } else {
                  tree += timeVal -> IIdxSeq( time -> elem )
               }
               Span( timeVal, stop )

            case (Some( (start, startLeaf) ), None) =>
               if( start == timeVal ) {
                  tree += timeVal -> ((time -> elem) +: startLeaf)
                  Span.from( timeVal )
               } else {
                  tree += timeVal -> IIdxSeq( time -> elem )
                  Span( start, timeVal )
               }

            case (None, Some( (stop, stopLeaf) )) =>
               tree += timeVal -> IIdxSeq( time -> elem )
               Span( timeVal, stop )

            case (None, None) =>
               tree += timeVal -> IIdxSeq( time -> elem )
               Span.from( timeVal )
         }
      }

      final def remove( time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val timeVal = time.value
         val res     = removeNoFire( timeVal, time, elem )
         if( res && isConnected ) {
            CollChanged -= time
// TODO XXX
//            ElemChanged -= elem
            CollChanged( BiPin.Removed( this, timeVal, elem ))
         }
         res
      }

      private def removeNoFire( timeVal: Long, time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val point   = timeVal
         val entry   = (time, elem)
         tree.get( point ) match {
            case Some( IIdxSeq( single )) =>
               if( single == entry ) {
                  tree -= point // .removeAt( point )
                  true
               } else {
                  false
               }
            case Some( seq ) =>
               val seqNew = seq.filterNot( _ == entry )
               if( seqNew.size != seq.size ) {
                  tree.add( (timeVal, seqNew) )
                  true
               } else {
                  false
               }
            case None => false
         }
      }

      final def debugList()( implicit tx: S#Tx ) : List[ (Long, Elem) ] =
         tree.toList.flatMap { case (time, seq) => seq.map { case (_, elem) => time -> elem }}

//      final def iterator( implicit tx: S#Tx, chr: Chronos[ S ]) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]  =
//         intersect( chr.time.value )

//      final def intersect( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
//         val start   = time
//         val stop    = time + 1
////         val shape = Rectangle( ti, MIN_COORD, MAX_COORD - ti + 1, ti - MIN_COORD + 1 )
//         // horizontally: until query_stop; vertically: from query_start
//         // start < query.stop && stop > query.start
//         val shape = LongRectangle( MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start )
//         rangeSearch( shape )
//      }

//      final def intersect( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
//         // horizontally: until query_stop; vertically: from query_start
//         span match {
//            case Span( start, stop ) =>
//               val shape = LongRectangle( MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start )
//               rangeSearch( shape )
//
//            case Span.From( start ) =>
//               val shape = LongRectangle( MIN_COORD, start + 1, MAX_SIDE, MAX_COORD - start )
//               rangeSearch( shape )
//
//            case Span.Until( stop ) =>
//               val shape = LongRectangle( MIN_COORD, MIN_COORD, stop - MIN_COORD, MAX_SIDE )
//               rangeSearch( shape )
//
//            case Span.All  => tree.iterator
//            case Span.Void => txn.Iterator.empty
//         }
//      }

//      final def rangeSearch( start: SpanLike, stop: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
//         if( start == Span.Void || stop == Span.Void ) return txn.Iterator.empty
//
//         val startP  = searchSpanToPoint( start )
//         val stopP   = searchSpanToPoint( stop  )
//         val shape   = LongRectangle( startP.x, stopP.x, startP.y - startP.x /* + 1 */, stopP.y - stopP.x /* + 1 */)
////println( "RANGE " + shape )
//         rangeSearch( shape )
//      }

//      // this can be easily implemented with two rectangular range searches
//      final def eventsAt( time: Long )( implicit tx: S#Tx ) : (txn.Iterator[ S#Tx, Leaf[ S, Elem ]], txn.Iterator[ S#Tx, Leaf[ S, Elem ]]) = {
//         val startShape = LongRectangle( time, MIN_COORD, 1, MAX_SIDE )
//         val stopShape  = LongRectangle( MIN_COORD, time, MAX_SIDE, 1 )
//         (rangeSearch( startShape ), rangeSearch( stopShape ))
//      }

//      final def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = {
//         val point   = LongPoint2D( time, time ) // + 1
//         val span    = tree.nearestNeighborOption( point, advanceNNMetric ).map( _._1 ).getOrElse( Span.Void )
//         span match {
//            case sp @ Span.From( start ) => assert( start >= time, sp ); Some( start ) // else None
//            case sp @ Span.Until( stop ) => assert( stop  >= time, sp ); Some( stop  ) // else None
//            case sp @ Span( start, stop ) =>
//               if( start >= time ) {
//                  Some( start )
//               } else {
//                  assert( stop >= time, sp ); Some( stop )
//               }
//            case _ => None // All or Void
//         }
//      }

//      final def nearestEventBefore( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = {
//         val point   = LongPoint2D( time, time )
//         val span    = tree.nearestNeighborOption( point, regressNNMetric ).map( _._1 ).getOrElse( Span.Void )
//         span match {
//            case sp @ Span.From( start ) => assert( start <= time, sp ); Some( start ) // else None
//            case sp @ Span.Until( stop ) => assert( stop  <= time, sp ); Some( stop  ) // else None
//            case sp @ Span( start, stop ) =>
//               if( stop <= time ) {
//                  Some( stop )
//               } else {
//                  assert( start <= time, sp ); Some( start )
//               }
//            case _ => None // All or Void
//         }
//      }

//      private def rangeSearch( shape: LongRectangle )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]] = {
//         val res = tree.rangeQuery( shape ) // .flatMap ....
////if( VERBOSE ) println( "Range in " + shape + " --> right = " + shape.right + "; bottom = " + shape.bottom + " --> found some? " + !res.isEmpty )
//         res
//      }

      final def collectionChanged : Event[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]] = CollChanged
      final def elementChanged    : Event[ S, BiPin.Element[    S, Elem, U ], BiPin[ S, Elem, U ]] = ElemChanged
      final def changed           : Event[ S, BiPin.Update[     S, Elem, U ], BiPin[ S, Elem, U ]] = Changed
   }

   private final class ImplNew[ S <: Sys[ S ], Elem, U ]( protected val targets: evt.Targets[ S ],
                                                          protected val tree: Tree[ S, Elem ],
                                                          protected val eventView: Elem => EventLike[ S, U, Elem ])
                                                        ( implicit protected val elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                          protected val timeType: Type[ Long ])
   extends Impl[ S, Elem, U ]
}
