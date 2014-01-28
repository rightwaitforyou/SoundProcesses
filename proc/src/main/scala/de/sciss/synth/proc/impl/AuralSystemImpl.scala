/*
 *  AuralSystemImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.osc.Dump
import scala.concurrent.stm.{TxnExecutor, Ref}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.{Server => SServer, ServerLike => SServerLike, ServerConnection}
import TxnExecutor.{defaultAtomic => atomic}
import de.sciss.lucre.synth.{NodeGraph, Server, Txn}

object AuralSystemImpl {
  import AuralSystem.Client

  var dumpOSC = false

  def apply(): AuralSystem = new Impl

  //   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
  //      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]
  //
  //   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
  //      def write( v: Nothing, out: DataOutput) = ()
  //      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
  //   }

  private final class Impl extends AuralSystem {
    impl =>

    override def toString = "AuralSystem@" + hashCode.toHexString

    private val clients       = Ref(Vec   .empty[Client])
    private val server        = Ref(Option.empty[Server])
    private val connection    = Ref(Option.empty[SServerLike])

    def start(config: Server.Config, connect: Boolean): AuralSystem = {
      doStart(config, connect = connect)
      this
    }

    def offline(server: Server.Offline): Unit = serverStarted(server)

    def stop(): AuralSystem = {
      doStop()
      this
    }

    private def serverStarted(rich: Server): Unit = {
      val cs = atomic { implicit itx =>
        implicit val ptx = Txn.wrap(itx)
        connection() = Some(rich.peer)
        server.set(Some(rich))
        NodeGraph.addServer(rich) // ( ProcTxn()( tx ))
        clients()
      }
      cs.foreach(_.started(rich))
    }

    private def doStart(config: Server.Config, connect: Boolean): Unit = {
      val launch: ServerConnection.Listener => ServerConnection = if (connect) {
        SServer.connect("SoundProcesses", config)
      } else {
        SServer.boot("SoundProcesses", config)
      }

      val c = launch {
        case ServerConnection.Aborted =>
          connection.single() = None

        case ServerConnection.Running(s) =>
          if (dumpOSC) s.dumpOSC(Dump.Text)
          SoundProcesses.pool.submit(new Runnable() {
            def run(): Unit = serverStarted(Server(s))
          })
      }

      Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
        def run(): Unit = impl.shutdown()
      }))

      connection.single() = Some(c)   // XXX TODO: not good
    }

    private def shutdown(): Unit =
      connection.single().foreach {
        case s: SServer => s.quit()
        case _ =>
      }

    private def doStop(): Unit = {
      val opt = atomic { implicit itx =>
        connection.single.swap(None) match {
          case Some(c: ServerConnection) => c.abort(); None
          case Some(s: SServer) =>
            val so = server.swap(None)
            so.foreach { rich =>
              implicit val ptx = Txn.wrap(itx)
              NodeGraph.removeServer(rich) // ( ProcTxn()( tx ))
            }
            so.map(_ -> clients())
          case _ => None
        }
      }
      opt.foreach { case (rich, cs) =>
        cs.foreach(_.stopped())
        rich.peer.quit()
      }
    }

    def addClient(c: Client): Unit = {
      clients.single.transform(_ :+ c)
      //      val sOpt = atomic { implicit itx =>
      //        clients.transform(_ :+ c)
      //        server()
      //      }
      //      sOpt.foreach(c.started(_))
    }

    def serverOption(implicit tx: Txn): Option[Server] = server()(tx.peer)

    def removeClient(c: Client): Unit =
      clients.single.transform { _.filterNot(_ == c) }

    def whenStarted(fun: Server => Unit): Unit =
      addClient(new Client {
        def started(s: Server): Unit = fun(s)    // XXX TODO: should we remove the client?

        def stopped() = ()
      })
  }
}