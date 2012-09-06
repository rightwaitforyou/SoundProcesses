package de.sciss.synth
package proc

import impl.{AuralSystemImpl => Impl}

object AuralSystem {
   def apply() : AuralSystem = Impl()

   trait Client {
      def started( s: Server ) : Unit
      def stopped() : Unit
   }
}
trait AuralSystem {
   def start( config: Server.Config = Server.Config() ) : AuralSystem
   def stop() : AuralSystem

   def addClient(    c: AuralSystem.Client ) : AuralSystem
   def removeClient( c: AuralSystem.Client ) : AuralSystem

   def whenStarted( fun: Server => Unit ) : AuralSystem
}
