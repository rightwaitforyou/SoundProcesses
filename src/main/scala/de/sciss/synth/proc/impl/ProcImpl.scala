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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, stm, expr, data, bitemp, DataInput, DataOutput}
import de.sciss.synth.expr.{Strings, Doubles, ExprImplicits}
import evt.{Event, impl => evti, Sys}
import bitemp.BiType
import expr.Expr
import data.SkipList
import ExprImplicits._
import annotation.switch

object ProcImpl {
   private final val SER_VERSION = 0

   implicit val paramType : BiType[ Param ] = Doubles

   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Proc[ S ] = new New[ S ]( tx )

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] =
      serializer[ S ].read( in, access )

   def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Proc[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Proc[ S ]]]

   final val emptyGraph: SynthGraph = SynthGraph {}

   private val anySer = new Serializer[ evt.InMemory ]

   private class Serializer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Proc[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Proc[ S ] =
         new Read( in, access, targets, tx )
   }

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )
   
   private type GraphemeEntry[ S <: Sys[ S ]] = KeyMapImpl.Entry[ S, String, Grapheme[ S ], Grapheme.Update[ S ]]
   private type ScanEntry[     S <: Sys[ S ]] = KeyMapImpl.Entry[ S, String, Scan[     S ], Scan.Update[     S ]]

   implicit def graphemeEntryInfo[ S <: Sys[ S ]] : KeyMapImpl.ValueInfo[ S, String, Grapheme[ S ], Grapheme.Update[ S ]] =
      anyGraphemeEntryInfo.asInstanceOf[ KeyMapImpl.ValueInfo[ S, String, Grapheme[ S ], Grapheme.Update[ S ]]]

   private val anyGraphemeEntryInfo = new KeyMapImpl.ValueInfo[ I, String, Grapheme[ I ], Grapheme.Update[ I ]] {
      def valueEvent( value: Grapheme[ I ]) = value.changed
      val keySerializer    = stm.ImmutableSerializer.String
      val valueSerializer  = Grapheme.serializer[ I ]
   }

   implicit def scanEntryInfo[ S <: Sys[ S ]] : KeyMapImpl.ValueInfo[ S, String, Scan[ S ], Scan.Update[ S ]] =
      anyScanEntryInfo.asInstanceOf[ KeyMapImpl.ValueInfo[ S, String, Scan[ S ], Scan.Update[ S ]]]

   private val anyScanEntryInfo = new KeyMapImpl.ValueInfo[ I, String, Scan[ I ], Scan.Update[ I ]] {
      def valueEvent( value: Scan[ I ]) = value.changed
      val keySerializer    = stm.ImmutableSerializer.String
      val valueSerializer  = Scan.serializer[ I ]
   }

   private sealed trait Impl[ S <: Sys[ S ]]
   extends Proc[ S ]
   {
      proc =>

      import Proc._

      protected def graphVar : S#Var[ Code[ SynthGraph ]]
      protected def name_# : Expr.Var[ S, String ]
      protected def scanMap : SkipList.Map[ S, String, ScanEntry[ S ]]
      protected def graphemeMap : SkipList.Map[ S, String, GraphemeEntry[ S ]]

      final def name( implicit tx: S#Tx ) : Expr[ S, String ] = name_#.get
      final def name_=( s: Expr[ S, String ])( implicit tx: S#Tx ) {
         name_#.set( s )
      }

      final def graph( implicit tx: S#Tx ) : Code[ SynthGraph ] = graphVar.get

      final def graph_=( g: Code[ SynthGraph ])( implicit tx: S#Tx ) {
         val old = graphVar.get
         if( old != g ) {
            graphVar.set( g )
            StateEvent( GraphChange( this, evt.Change( old.value, g.value )))
         }
      }

      // ---- key maps ----

      sealed trait ProcEvent {
         final protected def reader : evt.Reader[ S, Proc[ S ]] = ProcImpl.serializer
         final def node: Proc[ S ] with evt.Node[ S ] = proc
      }

      sealed trait KeyMap[ Value, ValueUpd, OuterUpd ]
      extends evti.EventImpl[ S, OuterUpd, Proc[ S ]]
      with evt.InvariantEvent[ S, OuterUpd, Proc[ S ]]
      with ProcEvent
      with impl.KeyMapImpl[ S, String, Value, ValueUpd ] {
         protected def wrapKey( key: String ) : AssociativeKey

         // ---- keymapimpl details ----

         final protected def fire( added: Set[ String ], removed: Set[ String ])( implicit tx: S#Tx ) {
            StateEvent( Proc.AssociativeChange( proc, added   = added.map( wrapKey ), removed = removed.map( wrapKey )))
         }

         final protected def isConnected( implicit tx: S#Tx ) : Boolean = proc.targets.nonEmpty
      }

      object graphemes extends Graphemes.Modifiable[ S ]
      with KeyMap[ Grapheme[ S ], Grapheme.Update[ S ], Proc.GraphemeChange[ S ]] {
         final val slot = 1

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.GraphemeChange[ S ]] = {
            val changes = foldUpdate( pull )
            if( changes.isEmpty ) None else Some( Proc.GraphemeChange( proc, changes ))
         }

         protected def wrapKey( key: String ) = GraphemeKey( key )

         protected def map: SkipList.Map[ S, String, Entry ] = graphemeMap

         protected def valueInfo = graphemeEntryInfo[ S ]
      }

      object scans extends Scans.Modifiable[ S ]
      with KeyMap[ Scan[ S ], Scan.Update[ S ], Proc.ScanChange[ S ]] {
         final val slot = 2

         protected def wrapKey( key: String ) = ScanKey( key )

         def add( key: String )( implicit tx: S#Tx ) : Scan[ S ] = {
            val scan = Scan[ S ]
            add( key, scan )
            scan
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.ScanChange[ S ]] = {
            val changes = foldUpdate( pull )
            if( changes.isEmpty ) None else Some( Proc.ScanChange( proc, changes ))
         }

         protected def map: SkipList.Map[ S, String, Entry ] = scanMap

         protected def valueInfo = scanEntryInfo[ S ]
      }

      private object StateEvent
      extends evti.TriggerImpl[ S, Proc.StateChange[ S ], Proc[ S ]]
      with evt.InvariantEvent[ S, Proc.StateChange[ S ], Proc[ S ]]
      with evti.Root[ S, Proc.StateChange[ S ]]
      with ProcEvent {
         final val slot = 4
      }

      private object ChangeEvent
      extends evt.Event[ S, Proc.Update[ S ], Proc[ S ]]
      with evt.InvariantSelector[ S ]
      with ProcEvent {
         def slot: Int = opNotSupported

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            graphemes  ---> r
            scans      ---> r
            StateEvent ---> r
         }
         def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            graphemes  -/-> r
            scans      -/-> r
            StateEvent -/-> r
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.Update[ S ]] = {
            if(      graphemes.isSource(  pull )) graphemes.pullUpdate(  pull )
            else if( scans.isSource(      pull )) scans.pullUpdate(      pull )
            else if( StateEvent.isSource( pull )) StateEvent.pullUpdate( pull )
            else None
         }

         def react[ A1 >: Proc.Update[ S ] ]( fun: A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, Proc[ S ]] =
            reactTx( (_: S#Tx) => fun )

         def reactTx[ A1 >: Proc.Update[ S ]]( fun: S#Tx => A1 => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, A1, Proc[ S ]] = {
            val obs = evt.Observer( ProcImpl.serializer[ S ], fun )
            obs.add( graphemes  )
            obs.add( scans      )
            obs.add( StateEvent )
            obs
         }

         def isSource( pull: evt.Pull[ S ]) : Boolean = {
            // I don't know why this method is actually called? But it _is_, so we need to correctly handle the case
            graphemes.isSource( pull ) || scans.isSource( pull ) || StateEvent.isSource( pull )
         }
      }

      final def select( slot: Int, invariant: Boolean ) : Event[ S, Any, Any ] = (slot: @switch) match {
         case graphemes.slot  => graphemes
         case scans.slot      => scans
         case StateEvent.slot => StateEvent
      }

      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
      final def changed :      evt.Event[ S, Update[      S ], Proc[ S ]] = ChangeEvent

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         name_#.write( out )
         graphVar.write( out )
         scanMap.write( out )
         graphemeMap.write( out )
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         name_#.dispose()
         graphVar.dispose()
         scanMap.dispose()
         graphemeMap.dispose()
      }

      override def toString() = "Proc" + id
   }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
      protected val targets   = evt.Targets[ S ]( tx0 )

      protected val name_#    = Strings.newVar[ S ]( "unnamed" )( tx0 )
      protected val graphVar  = {
         implicit val peerSer = SynthGraphSerializer
         tx0.newVar[ Code[ SynthGraph ]]( id, emptyGraph )
      }

      protected val scanMap = {
         implicit val tx = tx0
//         implicit val _scanSer = implicitly[ stm.Serializer[ S#Tx, S#Acc, Scan[ S ]]]
//         implicit val _scanSer : stm.Serializer[ S#Tx, S#Acc, Scan[ S ]] = Scan.serializer
         implicit val _screwYou : stm.Serializer[ S#Tx, S#Acc, ScanEntry[ S ]] = KeyMapImpl.entrySerializer
         SkipList.Map.empty[ S, String, ScanEntry[ S ]]
      }

      protected val graphemeMap = {
         implicit val tx = tx0
         implicit val _screwYou : stm.Serializer[ S#Tx, S#Acc, GraphemeEntry[ S ]] = KeyMapImpl.entrySerializer
         SkipList.Map.empty[ S, String, GraphemeEntry[ S ]]
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
      protected val graphVar  = {
         implicit val peerSer = SynthGraphSerializer
         tx0.readVar[ Code[ SynthGraph ]]( id, in )
      }

      protected val scanMap = {
         implicit val tx = tx0
         implicit val _screwYou : stm.Serializer[ S#Tx, S#Acc, ScanEntry[ S ]] = KeyMapImpl.entrySerializer
         SkipList.Map.read[ S, String, ScanEntry[ S ]]( in, access )
      }

      protected val graphemeMap = {
         implicit val tx = tx0
         implicit val _screwYou : stm.Serializer[ S#Tx, S#Acc, GraphemeEntry[ S ]] = KeyMapImpl.entrySerializer
         SkipList.Map.read[ S, String, GraphemeEntry[ S ]]( in, access )
      }
   }
}