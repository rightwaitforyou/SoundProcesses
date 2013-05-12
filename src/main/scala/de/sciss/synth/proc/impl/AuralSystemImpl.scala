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
import concurrent.stm.{Ref, Txn}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm
import de.sciss.synth.{Server => SServer, ServerLike => SServerLike, ServerConnection}

object AuralSystemImpl {
  import AuralSystem.Client

  var dumpOSC = false

  def apply[S <: Sys[S]](implicit cursor: stm.Cursor[S]): AuralSystem[S] = new Impl[S]

  //   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
  //      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]
  //
  //   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
  //      def write( v: Nothing, out: DataOutput) {}
  //      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
  //   }

  private final class Impl[S <: Sys[S]](implicit cursor: stm.Cursor[S])
    extends AuralSystem[S] {
    impl =>

    override def toString = "AuralSystem@" + hashCode.toHexString

    private val startStopCnt  = Ref(0)
    private val clients       = Ref(IIdxSeq.empty[Client[S]])
    private val server        = Ref(Option.empty[Server])
    private val connection    = Ref(Option.empty[SServerLike])

    def start(config: Server.Config, connect: Boolean)(implicit tx: S#Tx): AuralSystem[S] = {
      implicit val itx = tx.peer
      val expected = startStopCnt.get + 1
      startStopCnt.set(expected)

      Txn.beforeCommit(_ => {
        if (startStopCnt.get == expected) doStart(config, connect = connect)
      })(tx.peer)
      this
    }

    def offline(server: Server.Offline)(implicit tx: S#Tx) {
      serverStarted(server)
    }

    def stop()(implicit tx: S#Tx): AuralSystem[S] = {
      implicit val itx = tx.peer
      val expected = startStopCnt.get + 1
      startStopCnt.set(expected)

      Txn.beforeCommit(_ => {
        if (startStopCnt.get == expected) doStop()
      })(tx.peer)
      this
    }

    private def serverStarted(rich: Server)(implicit tx: S#Tx) {
      implicit val itx = tx.peer
      connection() = Some(rich.peer)
      server.set(Some(rich))
      ProcDemiurg.addServer(rich) // ( ProcTxn()( tx ))
      val cs = clients.get
      //                  println( "AQUI " + cs )
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
              cursor.step { implicit tx =>
                serverStarted(Server(s))
              }
            }
          })
      }

      Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
        def run() {
          impl.shutdown()
        }
      }))

      connection.single() = Some(c)
    }

    private def shutdown() {
      connection.single().foreach {
        case s: SServer => s.quit()
        case _ =>
      }
    }

    private def doStop() {
      connection.single.swap(None).foreach {
        case c: ServerConnection => c.abort()
        case s: SServer =>
          cursor.step {
            implicit tx =>
              implicit val itx = tx.peer
              server.get.foreach { rich =>
                clients.get.foreach(_.stopped())
                ProcDemiurg.removeServer(rich) // ( ProcTxn()( tx ))
              }
          }
          s.quit()
      }
    }

    def addClient(c: Client[S])(implicit tx: S#Tx) {
      implicit val itx = tx.peer
      clients.transform(_ :+ c)
      val sOpt = server.get
      sOpt.foreach { s =>
        c.started(s)
      }
    }

    def removeClient(c: Client[S])(implicit tx: S#Tx) {
      implicit val itx = tx.peer
      clients.transform {
        _.filterNot(_ == c)
      }
    }

    def whenStarted(fun: S#Tx => Server => Unit)(implicit tx: S#Tx) {
      addClient(new Client[S] {
        def started(s: Server)(implicit tx: S#Tx) {
          fun(tx)(s)
        }

        def stopped()(implicit tx: S#Tx) {}
      })
    }
  }
}