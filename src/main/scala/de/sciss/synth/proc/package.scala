/*
 *  package.scala
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

import de.sciss.lucre.{bitemp, event => evt}
import bitemp.BiGroup
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import annotation.elidable
import annotation.elidable._

package object proc {
//   private[proc] type I = evt.InMemory

   type ProcGroup[ S <: evt.Sys[ S ]] = BiGroup[ S, Proc[ S ], Proc.Update[ S ]]
   type TimedProc[ S <: evt.Sys[ S ]] = BiGroup.TimedElem[ S, Proc[ S ]]
   type ProcTransport[ S <: evt.Sys[ S ]] = Transport[ S, Proc[ S ], Transport.Proc.Update[ S ]] // Proc.Update[ S ]
   type Param = Double

//   type ScanElem[ S <: evt.Sys[ S ]] = de.sciss.synth.proc.Scan.Elem

//   type Scan[ S <: evt.Sys[ S ]] = BiPin.Expr[ S, Scan_.Elem[ S ]]

//   type Grapheme[ S <: evt.Sys[ S ]] = BiPin[ S, Scan_.Elem[ S ], Scan_.Elem.Update[ S ]]

   private lazy val logHeader = new SimpleDateFormat( "[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US )
   var showLog          = false
   var showTxnLog       = false
   var showAuralLog     = false
   var showTransportLog = false
   var showAllocLog     = false

   @elidable(CONFIG) private[proc] def log( what: => String ) {
      if( showLog ) Console.out.println( logHeader.format( new Date() ) + what )
   }

//   @elidable(CONFIG) private[proc] def logConfig( what: => String ) {
//      if( showLog ) Console.out.println( logHeader.format( new Date() ) + what )
//   }

   @elidable(CONFIG) private[proc] def logAural( what: => String ) {
      if( showAuralLog ) Console.out.println( logHeader.format( new Date() ) + "aural " + what )
   }

   @elidable(CONFIG) private[proc] def logTransport( what: => String ) {
      if( showTransportLog ) Console.out.println( logHeader.format( new Date() ) + "transport " + what )
   }

   @elidable(CONFIG) private[proc] def logTxn( what: => String ) {
      if( showTxnLog ) Console.out.println( logHeader.format( new Date() ) + "txn " + what )
   }

   @elidable(CONFIG) private[proc] def logAlloc( what: => String ) {
      if( showAllocLog ) Console.out.println( logHeader.format( new Date() ) + "block " + what )
   }

   def ??? : Nothing = sys.error( "TODO" )
}