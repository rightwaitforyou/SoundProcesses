/*
 *  AuralSystemImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.proc
package impl

import de.sciss.osc.Dump
import scala.concurrent.stm.{Txn => ScalaTxn, TxnExecutor, Ref}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm
import de.sciss.synth.{Server => SServer, ServerLike => SServerLike, ServerConnection}
import TxnExecutor.{defaultAtomic => atomic}

object AuralSystemImpl {
  import AuralSystem.Client

  var dumpOSC = false

  def apply(): AuralSystem = new Impl

  //   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
  //      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]
  //
  //   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
  //      def write( v: Nothing, out: DataOutput) {}
  //      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
  //   }

  private final class Impl extends AuralSystem {
    impl =>

    override def toString = "AuralSystem@" + hashCode.toHexString

    private val clients       = Ref(IIdxSeq.empty[Client])
    private val server        = Ref(Option.empty[Server])
    private val connection    = Ref(Option.empty[SServerLike])

    def start(config: Server.Config, connect: Boolean, schoko: Int): AuralSystem = {
      doStart(config, connect = connect)
      this
    }

    def offline(server: Server.Offline, schoko: Int) {
      serverStarted(server)
    }

    def stop(schoko: Int): AuralSystem = {
      doStop()
      this
    }

    private def serverStarted(rich: Server) {
      val cs = atomic { implicit itx =>
        implicit val ptx = Txn.applyPlain(itx)
        connection() = Some(rich.peer)
        server.set(Some(rich))
        ProcDemiurg.addServer(rich) // ( ProcTxn()( tx ))
        clients()
      }
      cs.foreach(_.started(rich))
    }

    private def doStart(config: Server.Config, connect: Boolean) {
      val launch: ServerConnection.Listener => ServerConnection = if (connect) {
        SServer.connect("SoundProcesses", config) _
      } else {
        SServer.boot("SoundProcesses", config) _
      }

      val c = launch {
        case ServerConnection.Aborted =>
          connection.single() = None

        case ServerConnection.Running(s) =>
          if (dumpOSC) s.dumpOSC(Dump.Text)
          SoundProcesses.pool.submit(new Runnable() {
            def run() {
              serverStarted(Server(s))
            }
          })
      }

      Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
        def run() {
          impl.shutdown()
        }
      }))

      connection.single() = Some(c)   // XXX TODO: not good
    }

    private def shutdown() {
      connection.single().foreach {
        case s: SServer => s.quit()
        case _ =>
      }
    }

    private def doStop() {
      val opt = atomic { implicit itx =>
        connection.single.swap(None) match {
          case Some(c: ServerConnection) => c.abort(); None
          case Some(s: SServer) =>
            val so = server.swap(None)
            so.foreach { rich =>
              implicit val ptx = Txn.applyPlain(itx)
              ProcDemiurg.removeServer(rich) // ( ProcTxn()( tx ))
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

    def addClient(c: Client) {
      clients.single.transform(_ :+ c)
      //      val sOpt = atomic { implicit itx =>
      //        clients.transform(_ :+ c)
      //        server()
      //      }
      //      sOpt.foreach(c.started(_))
    }

    def serverOption(implicit tx: Txn): Option[Server] = server()(tx.peer)

    def removeClient(c: Client) {
      clients.single.transform { _.filterNot(_ == c) }
    }

    def whenStarted(fun: Server => Unit) {
      addClient(new Client {
        def started(s: Server) {
          fun(s)    // XXX TODO: should we remove the client?
        }

        def stopped() {}
      })
    }
  }
}