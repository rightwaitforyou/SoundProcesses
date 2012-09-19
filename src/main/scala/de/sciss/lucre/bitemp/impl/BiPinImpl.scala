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

package de.sciss.lucre
package bitemp
package impl

import de.sciss.lucre.{event => evt}
import evt.{Change, Event, EventLike}
import stm.{Serializer, Sys}
import data.SkipList
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import annotation.switch
import expr.{Expr, Type}

object BiPinImpl {
   import BiPin.{Leaf, TimedElem, Modifiable, Region}

//   private val MIN_TIME = Long.MinValue

   private type Tree[ S <: Sys[ S ], Elem ] = SkipList.Map[ S, Long, Leaf[ S, Elem ]]

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   def newModifiable[ S <: Sys[ S ], Elem, U ]( /* default: Elem, */ eventView: Elem => EventLike[ S, U, Elem ])(
      implicit tx: S#Tx, elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : Modifiable[ S, Elem, U ] = {

      implicit val exprSer: Serializer[ S#Tx, S#Acc, Expr[ S, Long ]] = timeType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipList.Map.empty[ S, Long, Leaf[ S, Elem ]]()
//      tree += MIN_TIME -> IIdxSeq( timeType.newConst( MIN_TIME ) -> default )
      new ImplNew( evt.Targets[ S ], tree, eventView )
   }

   def newPartialModifiable[ S <: Sys[ S ], Elem, U ]( /* default: Elem, */ eventView: Elem => EventLike[ S, U, Elem ])(
      implicit tx: S#Tx, elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : Modifiable[ S, Elem, U ] = {

      implicit val exprSer: Serializer[ S#Tx, S#Acc, Expr[ S, Long ]] = timeType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipList.Map.empty[ S, Long, Leaf[ S, Elem ]]()
//      tree += MIN_TIME -> IIdxSeq( timeType.newConst( MIN_TIME ) -> default )
      new ImplNew( evt.Targets.partial[ S ], tree, eventView )
   }

//   def readExprVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
//                                      ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
//      sys.error( "TODO" )
//
//   def readExpr[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
//                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : BiPin[ S, Expr[ S, A ], evt.Change[ A ]] =
//      read[ S, Expr[ S, A ], evt.Change[ A ]]( in, access, _.changed )

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : evt.NodeSerializer[ S , BiPin[ S, Elem, U ]] = new Ser( eventView )

   def modifiableSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
      timeType: Type[ Long ]) : evt.NodeSerializer[ S , BiPin.Modifiable[ S, Elem, U ]] = new ModSer( eventView )

   def readModifiable[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
         ( implicit tx: S#Tx, elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
           timeType: Type[ Long ]) : BiPin.Modifiable[ S, Elem, U ] = {

      val targets = evt.Targets.read[ S ]( in, access )
      readImpl( in, access, targets, eventView )
   }

   def read[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
                                     ( implicit tx: S#Tx,
                                       elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                       timeType: Type[ Long ]) : BiPin[ S, Elem, U ] = readModifiable( in, access, eventView )

   private def readImpl[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ], eventView: Elem => EventLike[ S, U, Elem ])
                                             ( implicit tx: S#Tx,
                                               elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                               timeType: Type[ Long ]) : Impl[ S, Elem, U ] = {
//      implicit val pointView: (Leaf[ S, Elem ], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint( tup._1 )
//      implicit val hyperSer   = SpaceSerializers.LongSquareSerializer
      implicit val exprSer: Serializer[ S#Tx, S#Acc, Expr[ S, Long ]] = timeType.serializer[ S ]
      val tree: Tree[ S, Elem ] = SkipList.Map.read[ S, Long, Leaf[ S, Elem ]]( in, access )
      new ImplNew( targets, tree, eventView )
   }

   private class Ser[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                timeType: Type[ Long ])
   extends evt.NodeSerializer[ S, BiPin[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin[ S, Elem, U ] = {
         BiPinImpl.readImpl( in, access, targets, eventView )
      }
   }

   private class ModSer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                                 ( implicit elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                   timeType: Type[ Long ])
   extends evt.NodeSerializer[ S, BiPin.Modifiable[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin.Modifiable[ S, Elem, U ] = {
         BiPinImpl.readImpl( in, access, targets, eventView )
      }
   }

   private sealed trait Impl[ S <: Sys[ S ], Elem, U ]
   extends Modifiable[ S, Elem, U ]
//   with evt.Compound[ S, Impl[ S, Elem, U ], Impl.type ]
//   with evt.Trigger.Impl[ S, BiPin.Update[ S, Elem, U ], BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
//   with evt.StandaloneLike[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
//   with evt.Node[ S ]
   {
      pin =>

      protected def tree: Tree[ S, Elem ]
      protected def eventView: Elem => EventLike[ S, U, Elem ]
      implicit protected def elemSerializer: evt.Serializer[ S, Elem ]
      implicit protected def timeType: Type[ Long ]

      override def toString() = "BiPin" + tree.id

      final def modifiableOption : Option[ BiPin.Modifiable[ S, Elem, U ]] = Some( this )

      // ---- event behaviour ----

      private object CollChanged
      extends evt.Trigger.Impl[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.EventImpl[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
//      with evt.Root[ S, BiPin.Collection[ S, Elem, U ]]
      {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 1
         def node: BiPin[ S, Elem, U ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Expr[ S, Long ])( implicit tx: S#Tx ) {
            elem.changed ---> this
         }

         def -=( elem: Expr[ S, Long ])( implicit tx: S#Tx ) {
            elem.changed -/-> this
         }

         private def incorporate( change: IIdxSeq[ Region[ Elem ]], span: Span.HasStart, elem: Elem ) : IIdxSeq[ Region[ Elem ]] = {
            val entry = (span, elem)
            change.flatMap({
               case (span2, elem2) => span2.subtract( span ).map( _ -> elem2 )
            }) :+ entry
//            span match {
//               case span1: Span =>
//                  change.flatMap({
//                     case (span2, elem2) => span2.subtract( span1 ).map( s => s -> elem2 )
//                  }) :+ entry
//               case span1: Span.From =>
//                  change.flatMap({
//                     case (span2, elem2) => span2.subtract( span1 ).nonEmptyOption.map( _ -> elem2 )
//                  }) :+ entry
//            }
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Collection[ S, Elem, U ]] = {
            val par = pull.parents( this )
            if( par.isEmpty ) {  // add or remove
               pull.resolve[ BiPin.Collection[ S, Elem, U ]]
            } else {             // span key changed
               val changes = par.foldLeft( IIdxSeq.empty[ Region[ Elem ]]) { case (ch, sel) =>
                  val time = sel.devirtualize( timeType.serializer[ S ].asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]])
                     .node.asInstanceOf[ Expr[ S, Long ]]
                  time.changed.pullUpdate( pull ) match {
                     case Some( Change( timeValOld, timeValNew )) =>
                        tree.get( timeValOld ) match {
                           case Some( leaf ) =>
                              leaf.foldLeft( ch ) { case (chi, (time2, elem)) =>
                                 if( time2 == time ) {
                                    // update in spatial structure
                                    val chi1 = removeNoFire( timeValOld, time, elem ) match {
                                       case Some( (time3, elem3) ) => incorporate( chi, time3, elem3 )
                                       case None => chi
                                    }
                                    incorporate( chi1, addNoFire( timeValNew, time, elem ), elem )
                                 } else {
                                    chi
                                 }
                              }
                           case None => ch
                        }
                     case None => ch
                  }
               }
               if( changes.isEmpty ) None else Some( BiPin.Collection( pin, changes ))
            }
         }
      }

      private object ElemChanged
      extends evt.EventImpl[ S, BiPin.Element[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantEvent[ S, BiPin.Element[ S, Elem, U ], BiPin[ S, Elem, U ]] {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 2
         def node: BiPin[ S, Elem, U ] = pin

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
               val evt  = sel.devirtualize( elemSerializer )
               val elem = evt.node
               // wow... how does this get the event update type right I'm wondering... ?
               evt.pullUpdate( pull ).map( elem -> _ )   // eventView( elem )
            })( breakOut )

            if( changes.isEmpty ) None else Some( BiPin.Element( pin, changes ))
         }
      }

      private object Changed
      extends evt.Event[ S, BiPin.Update[ S, Elem, U ], BiPin[ S, Elem, U ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, BiPin[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = opNotSupported
         def node: BiPin[ S, Elem, U ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged ---> r
            ElemChanged ---> r
         }
         def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollChanged -/-> r
            ElemChanged -/-> r
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Update[ S, Elem, U ]] = {
            if(      CollChanged.isSource( pull )) CollChanged.pullUpdate( pull )
            else if( ElemChanged.isSource( pull )) ElemChanged.pullUpdate( pull )
            else None
         }

         def react[ A1 >: BiPin.Update[ S, Elem, U ]]( fun: A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, BiPin[ S, Elem, U ]] =
            reactTx[ A1 ]( _ => fun )

         def reactTx[ A1 >: BiPin.Update[ S, Elem, U ]]( fun: S#Tx => A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, BiPin[ S, Elem, U ]] = {
            val obs = evt.Observer( serializer( eventView ), fun )
            obs.add( CollChanged )
            obs.add( ElemChanged )
            obs
         }

         def isSource( pull: evt.Pull[ S ]) : Boolean = CollChanged.isSource( pull ) || ElemChanged.isSource( pull )
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
            ElemChanged += elem
         }
      }

      final def disconnect()( implicit tx: S#Tx ) {
         foreach { case (time, elem) =>
            CollChanged -= time
            ElemChanged -= elem
         }
      }

      final def select( slot: Int, invariant: Boolean ) : Event[ S, Any, Any ] = (slot: @switch) match {
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
            ElemChanged += elem
            CollChanged( BiPin.Collection( pin, IIdxSeq( span -> elem )))
         }
      }

      final def intersect( time: Long )( implicit tx: S#Tx ) : Leaf[ S, Elem ] =
         tree.floor( time ) match {
            case Some( (_, leaf) ) => leaf
            case None => IIdxSeq.empty
         }

      final def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = tree.ceil( time ).map( _._1 )

      final def at( time: Long )( implicit tx: S#Tx ) : Option[ Elem ] = intersect( time ).headOption.map( _._2 )
//         tree.floor( time ).getOrElse( throw new NoSuchElementException( time.toString ))._2.head._2
//      }

      final def floor( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ] = tree.floor( time ).flatMap {
         case (time2, leaf) => leaf.headOption.map { case (_, elem) => time2 -> elem }
      }

      final def ceil( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ] = tree.ceil( time ).flatMap {
         case (time2, leaf) => leaf.headOption.map { case (_, elem) => time2 -> elem }
      }

      private def addNoFire( timeVal: Long, time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Span.HasStart = {
         val entry = (time, elem)
         (tree.floor( timeVal ), tree.ceil( timeVal + 1 )) match {
            case (Some( (start, startLeaf) ), Some( (stop, _) )) =>
               if( start == timeVal ) {
                  tree += timeVal -> (entry +: startLeaf)
               } else {
                  tree += timeVal -> IIdxSeq( entry )
               }
               Span( timeVal, stop )

            case (Some( (start, startLeaf) ), None) =>
               if( start == timeVal ) {
                  tree += timeVal -> (entry +: startLeaf)
                  Span.from( timeVal )
               } else {
                  tree += timeVal -> IIdxSeq( entry )
                  Span( start, timeVal )
               }

            case (None, Some( (stop, _) )) =>
               tree += timeVal -> IIdxSeq( entry )
               Span( timeVal, stop )

            case (None, None) =>
               tree += timeVal -> IIdxSeq( entry )
               Span.from( timeVal )
         }
      }

      final def remove( time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val timeVal = time.value
         val res     = removeNoFire( timeVal, time, elem )
         res.foreach { region =>
            if( isConnected ) {
               CollChanged -= time
               ElemChanged -= elem
               CollChanged( BiPin.Collection( pin, IIdxSeq( region )))
            }
         }
         res.isDefined
      }

      private def removeNoFire( timeVal: Long, time: Expr[ S, Long ], elem: Elem )
                              ( implicit tx: S#Tx ) : Option[ Region[ Elem ]] = {
         val entry = (time, elem)
         tree.get( timeVal ) match {
            case Some( IIdxSeq( single )) =>
               if( single == entry ) {
                  tree -= timeVal
                  (tree.floor( timeVal ), tree.ceil( timeVal + 1 )) match {
                     case (Some( (start, IIdxSeq( (_, startElem), _* ))), Some( (stop, _) )) =>
                        Some( Span( start, stop ) -> startElem )

                     case (Some( (start, IIdxSeq( (_, startElem), _* ))), None) =>
                        Some( Span.from( start ) -> startElem )

                     case _ => None
                  }
               } else {
                  None
               }
            case Some( seq ) =>
               val i = seq.indexOf( entry )
               if( i >= 0 ) {
                  val seqNew = seq.patch( i, IIdxSeq.empty[ TimedElem[ S, Elem ]], 1 )
                  tree += timeVal -> seqNew
//                  val IIdxSeq( (_, startElem), _* ) = seqNew.head
                  val startElem = seqNew.head._2
                  tree.ceil( timeVal + 1 ) match {
                     case Some( (stop, _) ) =>
                        Some( Span( timeVal, stop ) -> startElem )

                     case None =>
                        Some( Span.from( timeVal ) -> startElem )
                  }

               } else {
                  None
               }
            case None => None
         }
      }

      final def debugList()( implicit tx: S#Tx ) : List[ (Long, Elem) ] =
         tree.toList.flatMap { case (time, seq) => seq.map { case (_, elem) => time -> elem }}

      final def collectionChanged : Event[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]] = CollChanged
      final def elementChanged    : Event[ S, BiPin.Element[    S, Elem, U ], BiPin[ S, Elem, U ]] = ElemChanged
      final def changed           : Event[ S, BiPin.Update[     S, Elem, U ], BiPin[ S, Elem, U ]] = Changed
   }

   private final class ImplNew[ S <: Sys[ S ], Elem, U ]( protected val targets: evt.Targets[ S ],
                                                          protected val tree: Tree[ S, Elem ],
                                                          protected val eventView: Elem => EventLike[ S, U, Elem ])
                                                        ( implicit protected val elemSerializer: evt.Serializer[ S, Elem ],
                                                          protected val timeType: Type[ Long ])
   extends Impl[ S, Elem, U ]
}
