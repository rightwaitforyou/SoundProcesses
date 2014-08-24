/*
 *  SensorSystemImpl.scala
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

import scala.concurrent.stm.{TxnExecutor, Ref}
import collection.immutable.{IndexedSeq => Vec}
import TxnExecutor.{defaultAtomic => atomic}
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.osc
import java.net.SocketAddress

object SensorSystemImpl {
  import SensorSystem.{Client, Server}

  var dumpOSC = false

  def apply(): SensorSystem = new Impl

  /* There is a bug in Scala-STM which means
   * that calling atomic from within Txn.afterCommit
   * causes an exception. It seems this has to
   * do with the external decider being set?
   */
  private def afterCommit(code: => Unit)(implicit tx: TxnLike): Unit = tx.afterCommit {
    SoundProcesses.scheduledExecutorService.submit(new Runnable() {
      def run(): Unit = code
    })
  }

  private final class Impl extends SensorSystem {
    impl =>

    private sealed trait State extends Disposable[TxnLike] {
      def serverOption: Option[Server]
      def shutdown(): Unit
    }

    private case object StateStopped extends State {
      def dispose()(implicit tx: TxnLike): Unit = ()
      def serverOption: Option[Server] = None
      def shutdown(): Unit = ()
    }

    private case class StateBooting(config: SensorSystem.Config) extends State {
      //      private lazy val con: osc.ClientConnection = {
      //        val launch: osc.ClientConnection.Listener => osc.ClientConnection = if (connect) {
      //          Sosc.Client.connect("SoundProcesses", config)
      //        } else {
      //          Sosc.Client.boot("SoundProcesses", config)
      //        }
      //
      //        // logA(s"Booting (connect = $connect)")
      //        launch {
      //          case osc.ClientConnection.Aborted =>
      //            state.single.swap(StateStopped)
      //          //            atomic { implicit itx =>
      //          //              implicit val tx = Txn.wrap(itx)
      //          //              state.swap(StateStopped).dispose()
      //          //            }
      //
      //          case osc.ClientConnection.Running(s) =>
      //            if (dumpOSC) s.dumpOSC(Dump.Text)
      //            SoundProcesses.pool.submit(new Runnable() {
      //              def run(): Unit = clientStarted(osc.Client(s))
      //            })
      //        }
      //      }

      def init()(implicit tx: TxnLike): Unit = afterCommit {
        val s = config match {
          case c: osc.UDP.Config =>
            val rcv = osc.UDP.Receiver(c)
            rcv.connect()
            rcv
          // case c: osc.TCP.Config => osc.TCP.Server(c)
        }
        println(s.asInstanceOf[osc.Channel.Net.ConfigLike].localSocketAddress) // XXX TODO
        clientStarted(s)
      }

      def dispose()(implicit tx: TxnLike): Unit = ()

      def serverOption: Option[Server] = None
      def shutdown(): Unit = ()
    }

    private case class StateRunning(server: Server) extends State {
      def dispose()(implicit tx: TxnLike): Unit = {
        // logA("Stopped client")
        // NodeGraph.removeosc.Client(client)
        clients.get(tx.peer).foreach(_.sensorsStopped())

        afterCommit {
          server.action = osc.Channel.Undirected.Input.Net.NoAction
          //          val obs = listener.single.swap(None)
          //          assert(obs.isDefined)
          //          client.peer.removeListener(obs.get)
          if (server.isOpen()) server.close()
        }
      }

      def shutdown(): Unit = server.close()

      def serverOption: Option[Server] = Some(server)

      // private val listener = Ref(Option.empty[Sosc.Client.Listener])

      private def receive(p: osc.Packet, addr: SocketAddress): Unit = {
        if (dumpOSC) osc.Packet.printTextOn(p, osc.PacketCodec.default, System.out)
        p match {
          case osc.Message("/forecast",
              v0: Float, v1: Float, v2: Float, v3: Float, v4: Float, v5: Float, v6: Float, v7: Float,
              v8: Float, v9: Float, va: Float, vb: Float, vc: Float, vd: Float, ve: Float, vf: Float) =>
            val values = Vec(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, va, vb, vc, vd, ve, vf)
            atomic { implicit itx =>
              implicit val tx = TxnLike.wrap(itx)
              clients.get(tx.peer).foreach(_.sensorsUpdate(values))
            }
          case _ =>
        }
      }

      def init()(implicit tx: TxnLike): Unit = {
        // logA("Started client")
        // NodeGraph.addosc.Client(client)
        afterCommit {
          server.action = receive
        }
        clients.get(tx.peer).foreach(_.sensorsStarted(server))

        //        afterCommit {
        //          val list = server.peer.addListener {
        //            case Sosc.Client.Offline =>
        //              atomic { implicit itx =>
        //                implicit val tx = TxnLike.wrap(itx)
        //                state.swap(StateStopped).dispose()
        //              }
        //          }
        //          val old = listener.single.swap(Some(list))
        //          assert(old.isEmpty)
        //        }
      }
    }

    override def toString = s"SensorSystem@${hashCode.toHexString}"

    private val clients = Ref(Vec   .empty[Client])
    private val state   = Ref(StateStopped: State)

    private def clientStarted(rich: Server): Unit =
      atomic { implicit itx =>
        implicit val tx = TxnLike.wrap(itx)
        clientStartedTx(rich)
      }

    private def clientStartedTx(server: Server)(implicit tx: TxnLike): Unit = {
      val running = StateRunning(server)
      state.swap(running)(tx.peer) // .dispose()
      running.init()
    }

    def start(config: SensorSystem.Config)(implicit tx: TxnLike): Unit = state.get(tx.peer) match {
      case StateStopped =>
        installShutdown
        val booting = StateBooting(config)
        state.swap(booting)(tx.peer) // .dispose()
        booting.init()

      case _ =>
    }

    private lazy val installShutdown: Unit = Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      def run(): Unit = impl.shutdown()
    }))

    private def shutdown(): Unit = state.single().shutdown()

    def stop()(implicit tx: TxnLike): Unit =
      state.swap(StateStopped)(tx.peer).dispose()

    def addClient(c: Client)(implicit tx: TxnLike): Unit =
      clients.transform(_ :+ c)(tx.peer)

    def serverOption(implicit tx: TxnLike): Option[Server] = state.get(tx.peer).serverOption

    def removeClient(c: Client)(implicit tx: TxnLike): Unit =
      clients.transform { _.filterNot(_ == c) } (tx.peer)

    def whenStarted(fun: Server => Unit)(implicit tx: TxnLike): Unit = {
      state.get(tx.peer) match {
        case StateRunning(client) => fun(client)
        case _ =>
          val c: Client = new Client {
            def sensorsStarted(s: Server)(implicit tx: TxnLike): Unit = {
              removeClient(this)
              fun(s)
            }

            def sensorsStopped()(implicit tx: TxnLike) = ()

            def sensorsUpdate(values: Vec[Float])(implicit tx: TxnLike): Unit = ()
          }
          addClient(c)
      }
    }
  }
}