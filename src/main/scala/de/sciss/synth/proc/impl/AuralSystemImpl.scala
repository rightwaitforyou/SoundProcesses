package de.sciss.synth
package proc
package impl

import de.sciss.osc.Dump
import concurrent.stm.{InTxn, TxnExecutor}
import collection.immutable.{IndexedSeq => IIdxSeq}

object AuralSystemImpl {
   var dumpOSC = false

   def apply() : AuralSystem = {
      val impl = new Impl
      Runtime.getRuntime.addShutdownHook( new Thread( new Runnable {
         def run() { impl.stop() }
      }))
      impl
   }

   private final class Impl extends AuralSystem {
      import AuralSystem.Client

      override def toString = "AuralSystem@" + hashCode.toHexString

      private val sync        = new AnyRef
      private var connection  = Option.empty[ ServerLike ]
      private var clients     = IIdxSeq.empty[ Client ]

      private def atomic[ A ]( fun: InTxn => A ) : A = TxnExecutor.defaultAtomic( fun )

      def start( config: Server.Config ) : AuralSystem = {
         sync.synchronized {
            if( connection.isDefined ) return this

            val c = Server.boot( "SoundProcesses", config ) {
               case ServerConnection.Aborted =>
                  sync.synchronized { connection = None }
               case ServerConnection.Running( s ) =>
                  if( dumpOSC ) s.dumpOSC( Dump.Text )
                  atomic { implicit itx =>
                     ProcDemiurg.addServer( s )( ProcTxn() )
                  }
                  sync.synchronized {
                     connection = Some( s )
                     clients.foreach( _.started( s ))
                  }
            }
            connection = Some( c )
         }
         this
      }

      def stop() : AuralSystem = {
         sync.synchronized {
            connection.foreach {
               case c: ServerConnection => c.abort
               case s: Server =>
                  atomic { implicit itx =>
                     ProcDemiurg.removeServer( s )( ProcTxn() )
                  }
                  s.quit()
            }
            connection = None
         }
         this
      }

      def addClient( c: Client ) : AuralSystem = {
         sync.synchronized {
            clients :+= c
            connection match {
               case Some( s: Server ) => c.started( s )
               case _ =>
            }
         }
         this
      }

      def removeClient( c: Client ) : AuralSystem = {
         sync.synchronized {
            clients = clients.filterNot( _ == c )
         }
         this
      }

      def whenStarted( fun: ( Server ) => Unit) : AuralSystem = addClient( new Client {
         def started( s: Server ) {
            fun( s )
         }

         def stopped() {}
      })
   }
}