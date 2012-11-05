/*
 *  AuralSystem.scala
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

import impl.{AuralSystemImpl => Impl}
import de.sciss.lucre.{event => evt, stm}

object AuralSystem {
   def apply[ S <: evt.Sys[ S ]]( implicit tx: S#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] = Impl[ S ]

   def start[ S <: evt.Sys[ S ]]( config: Server.Config = Server.Config(), connect: Boolean = false )
                                ( implicit tx: S#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] =
      apply[ S ].start( config, connect = connect )

   trait Client[ S <: evt.Sys[ S ]] {
      def started( s: RichServer )( implicit tx: S#Tx ) : Unit
      def stopped()( implicit tx: S#Tx ) : Unit
   }
}
trait AuralSystem[ S <: evt.Sys[ S ]] {
   import AuralSystem.Client

   def start( config: Server.Config = Server.Config(), connect: Boolean = false  )( implicit tx: S#Tx ) : AuralSystem[ S ]
   def stop()( implicit tx: S#Tx ) : AuralSystem[ S ]

   def addClient(    c: Client[ S ])( implicit tx: S#Tx ) : Unit
   def removeClient( c: Client[ S ])( implicit tx: S#Tx ) : Unit

   def whenStarted( fun: S#Tx => RichServer => Unit )( implicit tx: S#Tx ) : Unit
}
