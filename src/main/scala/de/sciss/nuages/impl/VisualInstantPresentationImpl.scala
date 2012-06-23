/*
 *  VisualInstantPresentationImpl.scala
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

package de.sciss.nuages
package impl

import de.sciss.lucre.expr.{BiGroup, Chronos}
import de.sciss.lucre.stm.{Sys, Cursor}
import de.sciss.synth.proc.{Proc, Transport, ProcGroup}
import java.awt.EventQueue
import javax.swing.JComponent
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.Txn

object VisualInstantPresentationImpl {
   def apply[ S <: Sys[ S ], A ]( transport: S#Entry[ A ])
                                ( implicit cursor: Cursor[ S ],
                                  transportView: A => Transport[ S, Proc[ S ]]) : VisualInstantPresentation[ S ] = {

      require( EventQueue.isDispatchThread, "Must be called on EDT" )

      val res = new Impl( transport, cursor, transportView )
      cursor.step { implicit tx =>
         val map     = tx.newInMemoryIDMap[ VisualProc ]
         val t       = transportView( transport.get )
         val vps     = t.iterator.toIndexedSeq.map { proc =>
            val n    = proc.name.value
            val vp   = new VisualProc( n )
            map.put( proc.id, vp )
            vp
         }
         if( vps.nonEmpty ) Txn.afterCommit( _ => {
            res.add( vps: _* )
         })( tx.peer )

//         t.changed.react {
//            case BiGroup.Added(   group, span, elem ) =>
//            case BiGroup.Removed( group, span, elem ) =>
//            case BiGroup.Moved(   group, changes ) =>
//            case BiGroup.Element( group, changes ) =>
//         }

         t.changed.reactTx { implicit tx => {
            case Transport.Seek(    tFresh, time, added, removed ) =>
            case Transport.Advance( tFresh, time, added, removed ) =>
            case Transport.Play(    tFresh ) => Txn.afterCommit( _ => res.playing = true )(  tx.peer )
            case Transport.Stop(    tFresh ) => Txn.afterCommit( _ => res.playing = false )( tx.peer )
         }}
      }

      res
   }

   private final class VisualProc( val name: String )

   private final class Impl[ S <: Sys[ S ], A ]( transport: S#Entry[ A ], cursor: Cursor[ S ],
                                                 transportView: A => Transport[ S, Proc[ S ]])
   extends VisualInstantPresentation[ S ] {
      private var playingVar = false
      private var vps = Set.empty[ VisualProc ]

      def view : JComponent = sys.error( "TODO" )

      def add( procs: VisualProc* ) {
         vps ++= procs
         view.repaint()
      }

      def remove( procs: VisualProc* ) {
         vps --= procs
         view.repaint()
      }

      def playing : Boolean = playingVar
      def playing_=( b: Boolean ) {
         if( playingVar != b ) {
            playingVar = b
            view.repaint()
         }
      }
   }
}
