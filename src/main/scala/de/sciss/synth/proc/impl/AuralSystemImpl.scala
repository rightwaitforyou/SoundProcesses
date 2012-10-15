package de.sciss.synth
package proc
package impl

import de.sciss.osc.Dump
import concurrent.stm.Txn
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.{event => evt, DataInput, DataOutput, stm}

object AuralSystemImpl {
   import AuralSystem.Client

   var dumpOSC = true

   def apply[ S <: evt.Sys[ S ], I <: stm.Sys[ I ]]( implicit tx: S#Tx, bridge: S#Tx => I#Tx, cursor: stm.Cursor[ S ]) : AuralSystem[ S ] = {
      implicit val itx: I#Tx  = tx
      val id                  = itx.newID()
      val startStopCnt        = itx.newIntVar( id, init = 0 )
      implicit val clientsSer = dummySerializer[ IIdxSeq[ Client[ S ]], I ]
      val clients             = itx.newVar[ IIdxSeq[ Client[ S ]]]( id, IIdxSeq.empty )
      implicit val serverSer  = dummySerializer[ Server, I ]
      val server              = itx.newVar[ Option[ Server ]]( id, None )
      new Impl[ S, I ]( startStopCnt, clients, server )
   }

   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]

   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
      def write( v: Nothing, out: DataOutput) {}
      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
   }

   private final class Impl[ S <: evt.Sys[ S ], I <: stm.Sys[ I ]]( startStopCnt: I#Var[ Int ],
                                                                    clients: I#Var[ IIdxSeq[ Client[ S ]]],
                                                                    server: I#Var[ Option[ Server ]])
                                                                  ( implicit bridge: S#Tx => I#Tx, cursor: stm.Cursor[ S ])
   extends AuralSystem[ S ] {
      impl =>

      override def toString = "AuralSystem@" + hashCode.toHexString

      private val sync        = new AnyRef
      private var connection  = Option.empty[ ServerLike ]

      def start( config: Server.Config )( implicit tx: S#Tx ) : AuralSystem[ S ] = {
         implicit val itx: I#Tx  = tx
         val expected = startStopCnt.get + 1
         startStopCnt.set( expected )

         Txn.beforeCommit( _ => {
            if( startStopCnt.get == expected ) doStart( config )
         })( tx.peer )
         this
      }

      def stop()( implicit tx: S#Tx ) : AuralSystem[ S ] = {
         implicit val itx: I#Tx  = tx
         val expected = startStopCnt.get + 1
         startStopCnt.set( expected )

         Txn.beforeCommit( _ => {
            if( startStopCnt.get == expected ) doStop()
         })( tx.peer )
         this
      }

      private def doStart( config: Server.Config ) {
         val c = Server.boot( "SoundProcesses", config ) {
            case ServerConnection.Aborted =>
               sync.synchronized { connection = None }
            case ServerConnection.Running( s ) =>
               if( dumpOSC ) s.dumpOSC( Dump.Text )
               val sOpt = Some( s )
               sync.synchronized {
                  connection = sOpt
               }
               cursor.step { implicit tx =>
                  implicit val itx: I#Tx = tx
                  server.set( sOpt )
                  ProcDemiurg.addServer( s )( ProcTxn()( tx.peer ) )
                  val cs = clients.get
//                  println( "AQUI " + cs )
                  cs.foreach( _.started( s ))
               }
         }

         Runtime.getRuntime.addShutdownHook( new Thread( new Runnable {
            def run() { impl.shutdown() }
         }))

         sync.synchronized {
            connection = Some( c )
         }
      }

      private def shutdown() {
         sync.synchronized {
            connection.foreach {
               case s: Server => s.quit()
               case _ =>
            }
         }
      }

      private def doStop() {
         sync.synchronized {
            connection.foreach {
               case c: ServerConnection => c.abort
               case s: Server =>
                  cursor.step { implicit tx =>
                     implicit val itx: I#Tx = tx
                     clients.get.foreach( _.stopped() )
                     ProcDemiurg.removeServer( s )( ProcTxn()( tx.peer ))
                  }
                  s.quit()
            }
            connection = None
         }
      }

      def addClient( c: Client[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         clients.transform( _ :+ c )
         val sOpt = server.get
         sOpt.foreach { s =>
            c.started( s )
         }
      }

      def removeClient( c: Client[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         clients.transform { _.filterNot( _ == c )}
      }

      def whenStarted( fun: S#Tx => Server => Unit)( implicit tx: S#Tx ) {
         addClient( new Client[ S ] {
            def started( s: Server )( implicit tx: S#Tx ) {
               fun( tx )( s )
            }

            def stopped()( implicit tx: S#Tx ) {}
         })
      }
   }
}