/*
 *  BiPinImpl2.scala
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
import evt.{Change, Event, EventLike, impl => evti, Sys}
import stm.Serializer
import data.SkipList
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import annotation.switch
import expr.{Expr, Type}

object BiPinImpl2 {
   import BiPin2.{Leaf, Modifiable}

   private type Tree[ S <: Sys[ S ], A ] = SkipList.Map[ S, Long, Leaf[ S, A ]]

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   private implicit def leafSerializer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) : stm.Serializer[ S#Tx, S#Acc, Leaf[ S, A ]] =
      new LeafSer

   private final class LeafSer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) extends stm.Serializer[ S#Tx, S#Acc, Leaf[ S, A ]] {
      def write( leaf: BiPin2.Leaf[ S, A ], out: DataOutput ) {
         val sz = leaf.size
         out.writeInt( sz )
         if( sz == 0 ) return
         leaf.foreach( _.write( out ))
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : BiPin2.Leaf[ S, A ] = {
         val sz = in.readInt()
         if( sz == 0 ) return IIdxSeq.empty

         val elemSer = BiExpr.serializer[ S, A ]
         IIdxSeq.fill( sz )( elemSer.read( in, access ))
      }
   }

   def newModifiable[ S <: Sys[ S ], A ]( implicit tx: S#Tx, biType: BiType[ A ]) : Modifiable[ S, A ] = {
      val tree: Tree[ S, A ] = SkipList.Map.empty[ S, Long, Leaf[ S, A ]]()
      new Impl( evt.Targets.partial[ S ], tree ) // XXX TODO partial?
   }

   def serializer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) : evt.NodeSerializer[ S, BiPin2[ S, A ]] =
      new Ser

   def modifiableSerializer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) : evt.NodeSerializer[ S, BiPin2.Modifiable[ S, A ]] =
      new ModSer

   def readModifiable[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )( implicit tx: S#Tx, biType: BiType[ A ]) : BiPin2.Modifiable[ S, A ] = {
      val targets = evt.Targets.read[ S ]( in, access )
      readImpl( in, access, targets )
   }

   def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )( implicit tx: S#Tx, biType: BiType[ A ]) : BiPin2[ S, A ] =
      readModifiable( in, access )

   private def readImpl[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])
                                           ( implicit tx: S#Tx, biType: BiType[ A ]) : Impl[ S, A ] = {
      val tree: Tree[ S, A ] = SkipList.Map.read[ S, Long, Leaf[ S, A ]]( in, access )
      new Impl( targets, tree )
   }

   private class Ser[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ])
   extends evt.NodeSerializer[ S, BiPin2[ S, A ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin2[ S, A ] = {
         BiPinImpl2.readImpl( in, access, targets )
      }
   }

   private class ModSer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ])
   extends evt.NodeSerializer[ S, BiPin2.Modifiable[ S, A ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin2.Modifiable[ S, A ] = {
         BiPinImpl2.readImpl( in, access, targets)
      }
   }

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: evt.Targets[ S ], tree: Tree[ S, A ])
                                               ( implicit biType: BiType[ A ])
   extends Modifiable[ S, A ]
//   with evt.Compound[ S, Impl[ S, A ], Impl.type ]
//   with evt.Trigger.Impl[ S, BiPin2.Update[ S, A ], BiPin2.Update[ S, A ], BiPin2[ S, A ]]
//   with evt.StandaloneLike[ S, BiPin2.Update[ S, A ], BiPin2[ S, A ]]
//   with evt.Node[ S ]
   {
      pin =>

      private type ElemChange = evt.Change[ (Long, A) ]

//      protected def tree: Tree[ S, A ]
//      implicit protected def biType: BiType[ A ]

      override def toString = "BiPin" + tree.id

      def modifiableOption : Option[ BiPin2.Modifiable[ S, A ]] = Some( this )

      // ---- event behaviour ----

      private object CollChanged
      extends evti.TriggerImpl[ S, BiPin2.Collection[ S, A ], BiPin2[ S, A ]]
      with evti.EventImpl[ S, BiPin2.Collection[ S, A ], BiPin2[ S, A ]]
      with evt.InvariantEvent[ S, BiPin2.Collection[ S, A ], BiPin2[ S, A ]]
      with evti.Root[ S, BiPin2.Collection[ S, A ]]
      {
         protected def reader : evt.Reader[ S, BiPin2[ S, A ]] = serializer
         def slot: Int = 1
         def node: BiPin2[ S, A ] = pin

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin2.Collection[ S, A ]] = {
            pull.resolve[ BiPin2.Collection[ S, A ]]
         }
      }

      private object ElemChanged
      extends evti.EventImpl[ S, BiPin2.Element[ S, A ], BiPin2[ S, A ]]
      with evt.InvariantEvent[ S, BiPin2.Element[ S, A ], BiPin2[ S, A ]] {
         protected def reader : evt.Reader[ S, BiPin2[ S, A ]] = serializer
         def slot: Int = 2
         def node: BiPin2[ S, A ] = pin

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Elem )( implicit tx: S#Tx ) {
            elem.changed ---> this
         }

         def -=( elem: Elem )( implicit tx: S#Tx ) {
            elem.changed -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin2.Element[ S, A ]] = {
            val changes: IIdxSeq[ (Elem, ElemChange) ] = pull.parents( this ).flatMap( sel => {
               // wow... how does this get the event update type right I'm wondering... ?
               // UPDATE: ha! it doesn't. hell, this produces a runtime exception re Nothing???
               // --> fix: evt needs type ascription!!!
               val e    = sel.devirtualize[ ElemChange, Elem ]( BiExpr.serializer[ S, A ])
               val elem = e.node
               e.pullUpdate( pull ).map( elem -> _ )
            })( breakOut )

            if( changes.isEmpty ) None else Some( BiPin2.Element( pin, changes ))
         }
      }

      private object Changed
      extends evt.Event[ S, BiPin2.Update[ S, A ], BiPin2[ S, A ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, BiPin2[ S, A ]] = serializer
         def slot: Int = opNotSupported
         def node: BiPin2[ S, A ] = pin

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

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin2.Update[ S, A ]] = {
            if(      CollChanged.isSource( pull )) CollChanged.pullUpdate( pull )
            else if( ElemChanged.isSource( pull )) ElemChanged.pullUpdate( pull )
            else None
         }

         def react[ A1 >: BiPin2.Update[ S, A ]]( fun: A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, BiPin2[ S, A ]] =
            reactTx[ A1 ]( _ => fun )

         def reactTx[ A1 >: BiPin2.Update[ S, A ]]( fun: S#Tx => A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, BiPin2[ S, A ]] = {
            val obs = evt.Observer( serializer[ S, A ], fun )
            obs.add( CollChanged )
            obs.add( ElemChanged )
            obs
         }

         def isSource( pull: evt.Pull[ S ]) : Boolean = CollChanged.isSource( pull ) || ElemChanged.isSource( pull )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         tree.dispose()
      }

      protected def writeData( out: DataOutput ) {
         tree.write( out )
      }

      private def foreach( fun: Elem => Unit )( implicit tx: S#Tx ) {
         tree.iterator.foreach { case (_, seq) => seq.foreach( fun )}
      }

      def connect()( implicit tx: S#Tx ) {
         foreach( ElemChanged += _ )
      }

      def disconnect()( implicit tx: S#Tx ) {
         foreach( ElemChanged -= _ )
      }

      def select( slot: Int, invariant: Boolean ) : Event[ S, Any, Any ] = (slot: @switch) match {
         case 1 => CollChanged
         case 2 => ElemChanged
      }

      // ---- collection behaviour ----

      @inline private def isConnected( implicit tx: S#Tx ) : Boolean = targets.nonEmpty

      def clear()( implicit tx: S#Tx ) {
         if( isConnected ) {
//            val changes = tree.iterator.toIndexedSeq.flatMap { case (spanVal, seq) =>
//               seq.map { case (_, elem) => BiPin2.Removed( this, spanVal, elem )}
//            }
//            tree.clear()
//            changes.foreach( CollChanged.apply )
            sys.error( "TODO" )

         } else {
            tree.clear()
         }
      }

      def add( elem: Elem )( implicit tx: S#Tx ) {
         val timeVal = elem.timeValue
         addNoFire( timeVal, elem )
         if( isConnected ) {
//            CollChanged += time
            ElemChanged += elem
            CollChanged( BiPin2.Added( pin, timeVal -> elem.magValue, elem ))
         }
      }

      def intersect( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Leaf[ S, A ])] = {
         tree.floor( time )
      }

      def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = tree.ceil( time ).map( _._1 )

      def at( time: Long )( implicit tx: S#Tx ) : Option[ Elem ] = intersect( time ).flatMap( _._2.headOption )

      def floor( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ] = tree.floor( time ).flatMap {
         case (t, IIdxSeq( elem, rest @ _* )) => Some( t -> elem )
         case _ => None
      }

      def ceil( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ] = tree.ceil( time ).flatMap {
         case (t, IIdxSeq( elem, rest @ _* )) => Some( t -> elem )
         case _ => None
      }

      /**
       * Adds a new value, and returns the dirty which corresponds to the new region holding `elem`.
       *
       * @param timeVal the time value at which the new element is inserted
       * @param time    the time expression at which the new element is inserted
       * @param elem    the element which is inserted
       * @return
       */
      private def addNoFire( timeVal: Long, elem: Elem )( implicit tx: S#Tx ) {
         tree.get( timeVal ) match {
            case Some( oldLeaf ) =>
               tree += timeVal -> (elem +: oldLeaf)
            case _ =>
               tree += timeVal -> IIdxSeq( elem )
         }
      }

      def remove( elem: Elem )( implicit tx: S#Tx ) : Boolean = {
         val timeVal = elem.timeValue
         val (found, visible) = removeNoFire( timeVal, elem )
         if( visible && isConnected ) {
//            CollChanged -= time
            ElemChanged -= elem
            CollChanged( BiPin2.Removed( pin, timeVal -> elem.magValue, elem ))
         }
         found
      }

      private def removeNoFire( timeVal: Long, elem: Elem )( implicit tx: S#Tx ) : (Boolean, Boolean) = {
         tree.get( timeVal ) match {
            case Some( IIdxSeq( single )) =>
               val found = single == elem
               if( found ) tree -= timeVal
               (found, found)

            case Some( seq ) =>
               val i       = seq.indexOf( elem )
               val found   = i >= 0
               val visible = i == 0
               if( found ) {
                  val seqNew = seq.patch( i, IIdxSeq.empty[ Elem ], 1 )
                  tree += timeVal -> seqNew
               }
               (found, visible)

            case None => (false, false)
         }
      }

      def debugList()( implicit tx: S#Tx ) : List[ (Long, A) ] =
         tree.toList.flatMap { case (time, seq) => seq.map( time -> _.magValue )}

      def changed : EventLike[ S, BiPin2.Update[ S, A ], BiPin2[ S, A ]] = Changed
   }
}
