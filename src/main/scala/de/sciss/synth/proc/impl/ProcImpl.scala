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
import de.sciss.synth.SynthGraph
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
   private val SER_VERSION = 1

   def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : Proc[ S ] = new New[ S ]( tx )

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] =
      serializer[ S ].read( in, access )

   def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Proc[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Proc[ S ]]]

   val emptyGraph = SynthGraph {}

   private val anySer = new Serializer[ InMemory ]

   private class Serializer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Proc[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Proc[ S ] =
         new Read( in, access, targets, tx )
   }

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

//   @volatile private var declMap = Map.empty[ Class[ _ ], Decl[ _ ]]
//
//   private def getDecl[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Decl[ S ] = {
//      val clz = tx.system.manifest.erasure
//      declMap.getOrElse( clz, {
//         val declNew = new Decl[ S ]
//         declMap += clz -> declNew
//         declNew
//      }).asInstanceOf[ Decl[ S ]]
//   }
//
//   private class Decl[ S <: Sys[ S ]] extends evt.Decl[ S, Proc[ S ]] {
//      val serializer: evt.Reader[ S, Impl[ S ]] = new evt.Reader[ S, Impl[ S ]] {
//         def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Impl[ S ] =
//            new Read( in, access, targets, tx )
//      }
//
//      type Update = Proc.Update[ S ]
//
//      import Proc._
//
//      declare[ Rename[        S ]]( _.renamed        )
//      declare[ GraphChange[   S ]]( _.graphChanged   )
//      declare[ PlayingChange[ S ]]( _.playingChanged )
////      declare[ FreqChange[    S ]]( _.freqChanged    )
//      declare[ ParamChange[    S ]]( _.paramChanged )
//   }

   private def entrySerializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, EntryNode[ S ]] = new EntrySer

   private final class EntryNode[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], val key: String,
                                                  val value: BiPin.Expr[ S, Param ])
   extends evt.StandaloneLike[ S, (String, BiPin.Expr.Update[ S, Param ]), EntryNode[ S ]] {
      protected def reader: evt.Reader[ S, EntryNode[ S ]] = entrySerializer[ S ]

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

      /* private[lucre] */ def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ (String, BiPin.Expr.Update[ S, Param ])] =
         evt.Intruder.pullUpdate( value.changed, pull ).map( key -> _ )
   }

   implicit val paramType : BiType[ Param ] = Doubles

   private final class EntrySer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, EntryNode[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : EntryNode[ S ] = {
         val key     = in.readString()
         val value   = BiPin.Expr.Modifiable.read( in, access )
         new EntryNode( targets, key, value )
      }
   }

   private sealed trait Impl[ S <: Sys[ S ]]
   extends Proc[ S ]
//   with evt.Compound[ S, Proc[ S ], Decl[ S ]]
   with ParamMap[ S ]
   {
      proc =>

      import Proc._

//      private type ParamEx = BiPin.Expr[ S, Param ]

      protected def graphVar : S#Var[ ProcGraph ]
//      protected def playing_# : Expr.Var[ S, Boolean ]
      protected def playing_# : BiPin.Expr.Modifiable[ S, Boolean ]
      protected def name_# : Expr.Var[ S, String ]

//      protected def parMap : txn.SkipList.Map[ S, String, BiPin.Expr[ S, Param ]]
      protected def parMap : SkipList.Map[ S, String, EntryNode[ S ]]

      final def name( implicit tx: S#Tx ) : Expr[ S, String ] = {
         name_#.get
      }
      final def name_=( s: Expr[ S, String ])( implicit tx: S#Tx ) {
         name_#.set( s )
      }
      final def playing( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, Boolean ] = {
         playing_#.at( chr.time )
      }
      final def playing_=( b: Expr[ S, Boolean ])( implicit tx: S#Tx, chr: Chronos[ S ]) {
//         sys.error( "TODO" )
//         playing_#.set( b )
         playing_#.add( chr.time, b )
      }

      final def graph( implicit tx: S#Tx ) : ProcGraph = graphVar.get

      final def graph_=( g: ProcGraph )( implicit tx: S#Tx ) {
         val old = graphVar.get
         if( old != g ) {
            graphVar.set( g )
            StateEvent( GraphChange( this, evt.Change( old, g )))
            updatePars( old.synthGraph, g.synthGraph )
         }
      }
      final def graph_=( block: => Any )( implicit tx: S#Tx ) { graph_=( ProcGraph.withoutSource( SynthGraph( block )))}
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

      private object ParamEvent
      extends evt.EventImpl[ S, Proc.ParamChange[ S ], Proc.ParamChange[ S ], Proc[ S ]]
      with evt.InvariantEvent[ S, Proc.ParamChange[ S ], Proc[ S ]] {
         protected def reader : evt.Reader[ S, Proc[ S ]] = ProcImpl.serializer // ( eventView )
         def slot: Int = 2
         def node: evt.Node[ S ] = proc

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( entry: EntryNode[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.--->( entry, this )
         }

         def -=( entry: EntryNode[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.-/->( entry, this )
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.ParamChange[ S ]] = {
            val changes = pull.parents( this ).foldLeft( Map.empty[ String, IIdxSeq[ BiPin.Expr.Update[ S, Param ]]]) { case (map, sel) =>
//               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
               val node = evt.Intruder.devirtualizeNode( sel, entrySerializer ) // .asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]])
                  .asInstanceOf[ EntryNode[ S ]]
               evt.Intruder.pullUpdate( node, pull ) match {
                  case Some( (name, upd) ) => map + (name -> (map.getOrElse( name, IIdxSeq.empty ) :+ upd))
                  case None => map
               }
            }

            if( changes.isEmpty ) None else Some( Proc.ParamChange( proc, changes ))
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
            evt.Intruder.--->( ParamEvent, r )
         }
         /* private[lucre] */ def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            evt.Intruder.-/->( StateEvent, r )
            evt.Intruder.-/->( ParamEvent, r )
         }

         /* private[lucre] */ def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.Update[ S ]] = {
            if( evt.Intruder.isSource( ParamEvent, pull )) evt.Intruder.pullUpdate( ParamEvent, pull )
            else if( evt.Intruder.isSource( StateEvent, pull )) evt.Intruder.pullUpdate( StateEvent, pull )
            else None
         }

         def react( fun: Proc.Update[ S ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, Proc.Update[ S ], Proc[ S ]] =
            reactTx( _ => fun )

         def reactTx( fun: S#Tx => Proc.Update[ S ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, Proc.Update[ S ], Proc[ S ]] = {
            val obs = evt.Observer( ProcImpl.serializer[ S ], fun )
            obs.add( StateEvent )
            obs.add( ParamEvent )
            obs
         }

         /* private[lucre] */ def isSource( pull: evt.Pull[ S ]) : Boolean = {
            // I don't know why this method is actually called? But it _is_, so we need to correctly handle the case
//            opNotSupported
            evt.Intruder.isSource( StateEvent, pull ) || evt.Intruder.isSource( ParamEvent, pull )
         }
      }

      final def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = (slot: @switch) match {
         case 1 => StateEvent
         case 2 => ParamEvent
      }

      final def par: ParamMap[ S ] = this

      final def get( key: String )( implicit tx: S#Tx ) : Option[ BiPin.Expr[ S, Param ]] =
         parMap.get( key ).map( _.value )

      final def apply( key: String )( implicit tx: S#Tx ) : BiPin.Expr[ S, Param ] = {
         get( key ).getOrElse {
            if( keys.contains( key )) {
               implicit val doubles = Doubles
               val expr = BiPin.Expr.Modifiable[ S, Param ]( 0d )   // XXX retrieve default value
               val tgt  = evt.Targets[ S ]   // XXX partial?
               val node = new EntryNode[ S ]( tgt, key, expr )
               parMap += key -> node
               expr
            } else {
               throw new NoSuchElementException( key )
            }
         }
      }

      final def keys( implicit tx: S#Tx ) : Set[ String ] = graphKeys( graph.synthGraph )

      final def entriesAt( time: Long )( implicit tx: S#Tx ) : Map[ String, Param ] = {
         keys.flatMap( key => {
            parMap.get( key ).map( bi => key -> bi.value.at( time ).value )
         })( breakOut )
      }

      private def graphKeys( g: SynthGraph ) : Set[ String ] = g.controlProxies.flatMap( _.name )

      private def updatePars( oldGraph: SynthGraph, newGraph: SynthGraph )( implicit tx: S#Tx ) {
         val oldNames   = graphKeys( oldGraph )
         val newNames   = graphKeys( newGraph )
         val removed    = oldNames -- newNames
         val added      = newNames -- oldNames
         if( removed.nonEmpty || added.nonEmpty ) {

         }
      }

//      protected def freqVar : S#Var[ Expr[ S, Double ]]

//      final def freq( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, Double ] = freq_#.at( chr.time )
//      final def freq_=( f: Expr[ S, Double ])( implicit tx: S#Tx, chr: Chronos[ S ]) {
////         val before = freq_#.get
////         if( before != f ) {
////            val con = targets.nonEmpty
//////            logEvent( this.toString + " set " + expr + " (con = " + con + ")" )
////            if( con ) evt.Intruder.-/->( before.changed, freqChanged )
////            freq_#.set( f )
////            if( con ) {
////               evt.Intruder.--->( f.changed, freqChanged )
////               val beforeV = before.value
////               val exprV   = f.value
////               freqChanged( FreqChanged( this, evt.Change( beforeV, exprV )))
////            }
////         }
//
////         sys.error( "TODO" )
////         freq_#.set( f )
//         freq_#.add( chr.time, f )
//      }

//      final def renamed             = name_#.changed.map( Rename( this, _ ))
//      final def graphChanged        = event[ GraphChange[ S ]]
//      final def playingChanged      = playing_#.changed.map( PlayingChange( this, _ ))
//      final def freqChanged         = freq_#.changed.map( FreqChange( this, _ ))
      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
      final def paramChanged : evt.Event[ S, ParamChange[ S ], Proc[ S ]] = ParamEvent
      final def changed : evt.Event[ S, Update[ S ], Proc[ S ]] = ChangeEvent // = renamed | graphChanged | playingChanged | paramChanged

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         name_#.write( out )
         playing_#.write( out )
//         freq_#.write( out )
         graphVar.write( out )
         parMap.write( out )
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         name_#.dispose()
         playing_#.dispose()
//         freq_#.dispose()
         graphVar.dispose()
         parMap.dispose()
      }

      override def toString() = "Proc" + id

      override def hashCode() : Int = id.##
      override def equals( that: Any ) = that.isInstanceOf[ Proc[ _ ]] &&
         (that.asInstanceOf[ Proc[ _ ]].id == id)
   }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
//      protected val decl      = getDecl[ S ]( tx0 )
      protected val targets   = evt.Targets[ S ]( tx0 )

      protected val name_#    = Strings.newVar[ S ]( "unnamed" )( tx0 )
      protected val playing_# = BiPin.Expr.Modifiable.partial[ S, Boolean ]( true )( tx0, Booleans ) // Booleans.newVar[ S ]( true )( tx0 )
//      protected val freqVar   = {
//         implicit val peerSer = Doubles.serializer[ S ]
//         tx0.newVar[ Expr[ S, Double ]]( id, 441 )
//      }
//      protected val freq_#    = BiPin.newConfluentExprVar[ S, Double ]( 441 )( tx0, Doubles ) // Doubles.newConfluentVar[ S ]( 441 )( tx0 )
      protected val graphVar  = tx0.newVar[ ProcGraph ]( id, emptyGraph ) // ( SynthGraphSerializer )

      protected val parMap    = {
         implicit val tx      = tx0
//         implicit val parType = Doubles // .serializer[ S ]
//         implicit val exprSer = BiPin.exprSerializer[ S, Param ]
         implicit val entrySer = entrySerializer[ S ]
         SkipList.Map.empty[ S, String, EntryNode[ S ]]
      }
   }

   private final class Read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, protected val targets: evt.Targets[ S ],
                                             tx0: S#Tx )
   extends Impl[ S ] {
//      protected val decl      = getDecl[ S ]( tx0 )

      {
         val serVer = in.readUnsignedByte()
         require( serVer == SER_VERSION, "Incompatible serialized  (found " + serVer + ", required " + SER_VERSION + ")" )
      }

      protected val name_#    = Strings.readVar[  S ]( in, access )( tx0 )
//      protected val playing_# = Booleans.readVar[ S ]( in, access )( tx0 )
//      protected val freq_#    = Doubles.readVar[ S ]( in, access )( tx0 )
      protected val playing_# = BiPin.Expr.Modifiable.read[ S, Boolean ]( in, access )( tx0, Booleans )
//      protected val freq_#    = BiPin.readExprVar[ S, Double  ]( in, access )( tx0, Doubles  )
      protected val graphVar  = tx0.readVar[ ProcGraph ]( id, in ) // ( SynthGraphSerializer )

      protected val parMap    = {
         implicit val tx      = tx0
//         implicit val parType = Doubles // .serializer[ S ]
//         implicit val exprSer = BiPin.exprSerializer[ S, Param ]
         implicit val entrySer = entrySerializer[ S ]
         SkipList.Map.read[ S, String, EntryNode[ S ]]( in, access )
      }
   }
}
