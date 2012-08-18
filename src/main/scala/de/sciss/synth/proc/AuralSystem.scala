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
   def start( config: Server.Config = Server.Config() ) : Unit
   def stop() : Unit

   def addClient(    c: AuralSystem.Client ) : Unit
   def removeClient( c: AuralSystem.Client ) : Unit
}
