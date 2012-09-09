/*
 *  ProcImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.{event => evt, stm, expr, data, bitemp, DataInput, DataOutput}
import de.sciss.synth.{stepShape, SynthGraph}
import de.sciss.synth.expr.{Booleans, Strings, Doubles, ExprImplicits}
import stm.{InMemory, Sys}
import bitemp.{BiType, Chronos, BiPin}
import expr.Expr
import data.SkipList
import ExprImplicits._
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import annotation.switch

object ProcImpl {
   private final val SER_VERSION = 2

   def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : Proc[ S ] = new New[ S ]( tx )

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] =
      serializer[ S ].read( in, access )

   def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Proc[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Proc[ S ]]]

   final val emptyGraph: ProcGraph = ProcGraph {}

   private val anySer = new Serializer[ InMemory ]

   private class Serializer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Proc[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Proc[ S ] =
         new Read( in, access, targets, tx )
   }

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   private object ScanNode {
      implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, ScanNode[ S ]] = anyScanNodeSer.asInstanceOf[ ScanNodeSer[ S ]]
   }
   private final class ScanNode[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], val key: String,
                                                  val value: Scan[ S ])
   extends evt.StandaloneLike[ S, (String, Scan_.Update[ S ]), ScanNode[ S ]] {
      protected def reader: evt.Reader[ S, ScanNode[ S ]] = ScanNode.serializer[ S ]

      /* private[lucre] */ def connect()( implicit tx: S#Tx ) {
         evt.Intruder.--->( value.changed, this )
      }

      /* private[lucre] */ def disconnect()( implicit tx: S#Tx ) {
         evt.Intruder.-/->( value.changed, this )
      }

      protected def writeData( out: DataOutput ) {
         out.writeString( key )
         value.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         value.dispose()
      }

      /* private[lucre] */ def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ (String, Scan_.Update[ S ])] =
         evt.Intruder.pullUpdate( value.changed, pull ).map( key -> _ )
   }

   implicit val paramType : BiType[ Param ] = Doubles

   private val anyScanNodeSer = new ScanNodeSer[ I ]

   private final class ScanNodeSer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, ScanNode[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ScanNode[ S ] = {
         val key     = in.readString()
         val value   = Scan_.read( in, access ) // BiPin.Expr.Modifiable.read( in, access )
         new ScanNode( targets, key, value )
      }
   }

   private sealed trait Impl[ S <: Sys[ S ]]
   extends Proc[ S ]
//   with evt.Compound[ S, Proc[ S ], Decl[ S ]]
//   with ParamMap[ S ]
   {
      proc =>

      import Proc._

//      private type ParamEx = BiPin.Expr[ S, Param ]

      protected def graphVar : S#Var[ Code[ ProcGraph ]]
//      protected def playing_# : Expr.Var[ S, Boolean ]
      protected def playing_# : BiPin.Expr.Modifiable[ S, Boolean ]
      protected def name_# : Expr.Var[ S, String ]

//      protected def parMap : txn.SkipList.Map[ S, String, BiPin.Expr[ S, Param ]]
//      protected def parMap : SkipList.Map[ S, String, ScanNode[ S ]]

      protected def scanMap : SkipList.Map[ S, String, ScanNode[ S ]]

      object scans extends Scans.Modifiable[ S ] {
         def get( key: String )( implicit tx: S#Tx ) : Option[ Scan[ S ]] = scanMap.get( key ).map( _.value )
         def keys( implicit tx: S#Tx ) : Set[ String ] = scanMap.keysIterator.toSet

         def add( key: String, scan: Scan[ S ])( implicit tx: S#Tx ) {
            val isConnected = targets.nonEmpty
            val tgt  = evt.Targets[ S ]   // XXX TODO : partial?
            val sn   = new ScanNode( tgt, key, scan )
            val setRemoved = scanMap.add( key -> sn ) match {
               case Some( oldScan ) =>
                  if( isConnected ) ScanEvent -= oldScan
                  Set( key )
               case _ => Set.empty[ String ]
            }
            if( isConnected ) {
               ScanEvent += sn
               StateEvent( Proc.ScansCollectionChange( proc, Set( key ), setRemoved ))
            }
         }

         def remove( key: String )( implicit tx: S#Tx ) : Boolean = {
            scanMap.remove( key ) match {
               case Some( oldScan ) =>
                  val isConnected = targets.nonEmpty
                  if( isConnected ) {
                     ScanEvent -= oldScan
                     Proc.ScansCollectionChange( proc, Set.empty, Set( key ))
                  }
                  true

               case _ => false
            }
         }

         def valueAt( key: String, time: Long )( implicit tx: S#Tx ) : Option[ Scan_.Value[ S ]] = {
            scanMap.get( key ).flatMap { entry =>
               val scan = entry.value
               scan.floor( time ) match {
                  case Some( (floorTime, floorElem) ) =>
                     floorElem match {
                        case Scan_.Synthesis() => Some( Scan_.Value.Synthesis( proc ))
                        case Scan_.Mono( floorValueEx, _ ) =>
                           val floorValue = floorValueEx.value.toFloat
                           scan.ceil( time ) match {
                              case Some( (ceilTime, Scan_.Mono( ceilValueEx, ceilShape )) ) =>
                                 val ceilValue = ceilValueEx.value.toFloat
                                 if( ceilShape == stepShape ) {
                                    Some( Scan_.Value.MonoConst( ceilValue ))
                                 } else {
                                    val start: Float = sys.error( "TODO" )
                                    val dur:   Float = sys.error( "TODO" )
                                    Some( Scan_.Value.MonoSegment( start, ceilValue, dur, ceilShape ))
                                 }
                              case _ => Some( Scan_.Value.MonoConst( floorValue ))
                           }
                     }

                  case None => None
               }
            }

            sys.error( "TODO" )
         }
      }

      final def name( implicit tx: S#Tx ) : Expr[ S, String ] = {
         name_#.get
      }
      final def name_=( s: Expr[ S, String ])( implicit tx: S#Tx ) {
         name_#.set( s )
      }
      final def playing( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, Boolean ] = {
         playing_#.at( chr.time ).getOrElse( true )   // true?
      }
      final def playing_=( b: Expr[ S, Boolean ])( implicit tx: S#Tx, chr: Chronos[ S ]) {
//         playing_#.set( b )
         playing_#.add( chr.time, b )
      }

      final def graph( implicit tx: S#Tx ) : Code[ ProcGraph ] = graphVar.get

      final def graph_=( g: Code[ ProcGraph ])( implicit tx: S#Tx ) {
         val old = graphVar.get
         if( old != g ) {
            graphVar.set( g )
            StateEvent( GraphChange( this, evt.Change( old.value, g.value )))
//            updatePars( old.value.synthGraph, g.value.synthGraph )
         }
      }
//      final def graph_=( block: => Any )( implicit tx: S#Tx ) { graph_=( ProcGraph.withoutSource( SynthGraph( block )))}
      final def play()( implicit tx: S#Tx, chr: Chronos[ S ]) {
         playing_=( true )
      }
      final def stop()( implicit tx: S#Tx, chr: Chronos[ S ]) {
         playing_=( false )
      }

      // ---- ParamMap ----

      private object StateEvent
      extends evt.Trigger.Impl[ S, Proc.StateChange[ S ], Proc.StateChange[ S ], Proc[ S ]]
      with evt.EventImpl[ S, Proc.StateChange[ S ], Proc.StateChange[ S ], Proc[ S ]]
      with evt.InvariantEvent[ S, Proc.StateChange[ S ], Proc[ S ]]
      with evt.Root[ S, Proc.StateChange[ S ]]
      {
         protected def reader : evt.Reader[ S, Proc[ S ]] = ProcImpl.serializer // ( eventView )
         def slot: Int = 1
         def node: evt.Node[ S ] = proc
      }

      private object ScanEvent
      extends evt.EventImpl[ S, Proc.ScansElementChange[ S ], Proc.ScansElementChange[ S ], Proc[ S ]]
      with evt.InvariantEvent[ S, Proc.ScansElementChange[ S ], Proc[ S ]] {
         protected def reader : evt.Reader[ S, Proc[ S ]] = ProcImpl.serializer // ( eventView )
         def slot: Int = 2
         def node: evt.Node[ S ] = proc

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( entry: ScanNode[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.--->( entry, this )
         }

         def -=( entry: ScanNode[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.-/->( entry, this )
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.ScansElementChange[ S ]] = {
            val changes = pull.parents( this ).foldLeft( Map.empty[ String, IIdxSeq[ Scan_.Update[ S ]]]) { case (map, sel) =>
//               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
               val node = evt.Intruder.devirtualizeNode( sel, ScanNode.serializer ) // .asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]])
                  .asInstanceOf[ ScanNode[ S ]]
               evt.Intruder.pullUpdate( node, pull ) match {
                  case Some( (name, upd) ) => map + (name -> (map.getOrElse( name, IIdxSeq.empty ) :+ upd))
                  case None => map
               }
            }

            if( changes.isEmpty ) None else Some( Proc.ScansElementChange( proc, changes ))
         }
      }

      private object ChangeEvent
      extends evt.Event[ S, Proc.Update[ S ], Proc[ S ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, Proc[ S ]] = ProcImpl.serializer // ( eventView )
         def slot: Int = opNotSupported
         def node: evt.Node[ S ] = proc

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         /* private[lucre] */ def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.--->( StateEvent, r )
            evt.Intruder.--->( ScanEvent, r )
         }
         /* private[lucre] */ def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.-/->( StateEvent, r )
            evt.Intruder.-/->( ScanEvent, r )
         }

         /* private[lucre] */ def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.Update[ S ]] = {
            if(      evt.Intruder.isSource( ScanEvent,  pull )) evt.Intruder.pullUpdate( ScanEvent,  pull )
            else if( evt.Intruder.isSource( StateEvent, pull )) evt.Intruder.pullUpdate( StateEvent, pull )
            else None
         }

         def react( fun: Proc.Update[ S ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, Proc.Update[ S ], Proc[ S ]] =
            reactTx( _ => fun )

         def reactTx( fun: S#Tx => Proc.Update[ S ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, Proc.Update[ S ], Proc[ S ]] = {
            val obs = evt.Observer( ProcImpl.serializer[ S ], fun )
            obs.add( StateEvent )
            obs.add( ScanEvent )
            obs
         }

         /* private[lucre] */ def isSource( pull: evt.Pull[ S ]) : Boolean = {
            // I don't know why this method is actually called? But it _is_, so we need to correctly handle the case
//            opNotSupported
            evt.Intruder.isSource( StateEvent, pull ) || evt.Intruder.isSource( ScanEvent, pull )
         }
      }

      final def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = (slot: @switch) match {
         case 1 => StateEvent
         case 2 => ScanEvent
      }

//      final def par: ParamMap[ S ] = this
//
//      final def get( key: String )( implicit tx: S#Tx ) : Option[ BiPin.Expr[ S, Param ]] =
//         parMap.get( key ).map( _.value )
//
//      final def apply( key: String )( implicit tx: S#Tx ) : BiPin.Expr[ S, Param ] = {
//         get( key ).getOrElse {
//            if( keys.contains( key )) {
//               implicit val doubles = Doubles
//               val expr = BiPin.Expr.Modifiable[ S, Param ] // ( 0d )   // XXX retrieve default value
//               val tgt  = evt.Targets[ S ]   // XXX partial?
//               val node = new ScanNode[ S ]( tgt, key, expr )
//               parMap += key -> node
//               expr
//            } else {
//               throw new NoSuchElementException( key )
//            }
//         }
//      }
//
//      final def keys( implicit tx: S#Tx ) : Set[ String ] = graphKeys( graph.synthGraph )
//
//      final def entriesAt( time: Long )( implicit tx: S#Tx ) : Map[ String, Param ] = {
//         keys.flatMap( key => {
//            parMap.get( key ).flatMap { bi =>
//               bi.value.at( time ).map( ex => key -> ex.value )
//            }
//         })( breakOut )
//      }

//      private def graphKeys( g: SynthGraph ) : Set[ String ] = g.controlProxies.flatMap( _.name )

//      private def updatePars( oldGraph: SynthGraph, newGraph: SynthGraph )( implicit tx: S#Tx ) {
//         val oldNames   = graphKeys( oldGraph )
//         val newNames   = graphKeys( newGraph )
//         val removed    = oldNames -- newNames
//         val added      = newNames -- oldNames
//         if( removed.nonEmpty || added.nonEmpty ) {
//
//         }
//      }

      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
//      final def paramChanged : evt.Event[ S, ParamChange[ S ], Proc[ S ]] = ParamEvent
      final def changed : evt.Event[ S, Update[ S ], Proc[ S ]] = ChangeEvent // = renamed | graphChanged | playingChanged | paramChanged

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         name_#.write( out )
         playing_#.write( out )
         graphVar.write( out )
//         parMap.write( out )
         scanMap.write( out )
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         name_#.dispose()
         playing_#.dispose()
         graphVar.dispose()
//         parMap.dispose()
         scanMap.dispose()
      }

      override def toString() = "Proc" + id

//      override def hashCode() : Int = id.##
//      override def equals( that: Any ) = that.isInstanceOf[ Proc[ _ ]] &&
//         (that.asInstanceOf[ Proc[ _ ]].id == id)
   }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
      protected val targets   = evt.Targets[ S ]( tx0 )

      protected val name_#    = Strings.newVar[ S ]( "unnamed" )( tx0 )
      protected val playing_# = BiPin.Expr.Modifiable.partial[ S, Boolean ]/*( true )*/( tx0, Booleans ) // Booleans.newVar[ S ]( true )( tx0 )
      protected val graphVar  = {
         implicit val ser = Code.serializer[ S, ProcGraph ]
         tx0.newVar[ Code[ ProcGraph ]]( id, emptyGraph )
      }

      protected val scanMap    = {
         implicit val tx      = tx0
//         implicit val parType = Doubles // .serializer[ S ]
//         implicit val exprSer = BiPin.exprSerializer[ S, Param ]
//         implicit val entrySer = entrySerializer[ S ]
//         implicit val scanSer = Scan_.serializer[ S ]
         SkipList.Map.empty[ S, String, ScanNode[ S ]]
      }
   }

   private final class Read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, protected val targets: evt.Targets[ S ],
                                             tx0: S#Tx )
   extends Impl[ S ] {

      {
         val serVer = in.readUnsignedByte()
         require( serVer == SER_VERSION, "Incompatible serialized  (found " + serVer + ", required " + SER_VERSION + ")" )
      }

      protected val name_#    = Strings.readVar[  S ]( in, access )( tx0 )
      protected val playing_# = BiPin.Expr.Modifiable.read[ S, Boolean ]( in, access )( tx0, Booleans )
      protected val graphVar  = tx0.readVar[ Code[ ProcGraph ]]( id, in ) // ( SynthGraphSerializer )

      protected val scanMap    = {
         implicit val tx      = tx0
         SkipList.Map.read[ S, String, ScanNode[ S ]]( in, access )
      }

//      protected val parMap    = {
//         implicit val tx      = tx0
////         implicit val parType = Doubles // .serializer[ S ]
////         implicit val exprSer = BiPin.exprSerializer[ S, Param ]
//         implicit val entrySer = entrySerializer[ S ]
//         SkipList.Map.read[ S, String, ScanNode[ S ]]( in, access )
//      }
   }
}
