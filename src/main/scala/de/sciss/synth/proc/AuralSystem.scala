package de.sciss.synth
package proc

import impl.{AuralSystemImpl => Impl}
import de.sciss.lucre.{event => evt, stm}

object AuralSystem {
   def apply[ S <: evt.Sys[ S ]]( implicit tx: S#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] = Impl[ S ]

   def start[ S <: evt.Sys[ S ]]()( implicit tx: S#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] = apply[ S ].start()

   trait Client[ S <: evt.Sys[ S ]] {
      def started( s: RichServer )( implicit tx: S#Tx ) : Unit
      def stopped()( implicit tx: S#Tx ) : Unit
   }
}
trait AuralSystem[ S <: evt.Sys[ S ]] {
   import AuralSystem.Client

   def start( config: Server.Config = Server.Config() )( implicit tx: S#Tx ) : AuralSystem[ S ]
   def stop()( implicit tx: S#Tx ) : AuralSystem[ S ]

   def addClient(    c: Client[ S ])( implicit tx: S#Tx ) : Unit
   def removeClient( c: Client[ S ])( implicit tx: S#Tx ) : Unit

   def whenStarted( fun: S#Tx => RichServer => Unit )( implicit tx: S#Tx ) : Unit
}
