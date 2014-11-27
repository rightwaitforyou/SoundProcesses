/*
 *  TransportViewImpl.scala
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
package gui
package impl

import de.sciss.lucre.stm.{Disposable, Cursor}
import scala.swing.{Action, Swing, Orientation, BoxPanel, Component}
import de.sciss.audiowidgets.{Transport => GUITransport, TimelineModel}
import java.awt.event.{ActionEvent, ActionListener}
import Swing._
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.desktop.{KeyStrokes, FocusType}
import scala.swing.event.Key
import de.sciss.span.Span
import de.sciss.synth.proc.{Timeline, Transport}
import de.sciss.desktop.Implicits._

object TransportViewImpl {
  def apply[S <: Sys[S]](transport: Transport[S] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */,
                         model: TimelineModel, hasMillis: Boolean, hasLoop: Boolean)
                        (implicit tx: S#Tx, cursor: Cursor[S]): TransportView[S] = {
    val view    = new Impl(transport, model)
    val srk     = 1000 / Timeline.SampleRate // transport.sampleRate

    view.observer = transport.react { implicit tx => {
      case Transport.Play(_, time) => view.startedPlaying(time)
      case Transport.Stop(_, time) => view.stoppedPlaying(time)
      case Transport.Seek(_, time, p) =>
        if (p) view.startedPlaying(time) else view.stoppedPlaying(time)
      case _ =>
    }}

    val initPlaying = transport.isPlaying // .playing.value
    val initMillis = (transport.position * srk).toLong
    deferTx {
      view.guiInit(initPlaying, initMillis, hasMillis = hasMillis, hasLoop = hasLoop)
    }
    view
  }

  private final class Impl[S <: Sys[S]](val transport: Transport[S] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */,
                                        val timelineModel: TimelineModel)
                                       (implicit protected val cursor: Cursor[S])
    extends TransportView[S] with ComponentHolder[Component] with CursorHolder[S] {

    private val modOpt  = timelineModel.modifiableOption

    var observer: Disposable[S#Tx] = _

    // private var millisVar: Long = 0L
    private var playTimer: javax.swing.Timer = _
    private var cueTimer : javax.swing.Timer = _
    // private var playingVar   = false
    // private var cueDirection = 1

    private var timerFrame  = 0L
    private var timerSys    = 0L
    private val srm         = 0.001 * Timeline.SampleRate // transport.sampleRate

    private var transportStrip: Component with GUITransport.ButtonStrip = _

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      deferTx {
        playTimer.stop()
        cueTimer .stop()
      }
    }

    // ---- transport ----

    def startedPlaying(time: Long)(implicit tx: S#Tx): Unit =
      deferTx {
        playTimer.stop()
        cueTimer .stop()
        timerFrame  = time
        timerSys    = System.currentTimeMillis()
        playTimer.start()
        transportStrip.button(GUITransport.Play).foreach(_.selected = true )
        transportStrip.button(GUITransport.Stop).foreach(_.selected = false)
      }

    def stoppedPlaying(time: Long)(implicit tx: S#Tx): Unit =
      deferTx {
        playTimer.stop()
        // cueTimer .stop()
        modOpt.foreach(_.position = time) // XXX TODO if Cursor follows play-head
        transportStrip.button(GUITransport.Play).foreach(_.selected = false)
        transportStrip.button(GUITransport.Stop).foreach(_.selected = true )
      }

    private def rtz(): Unit = {
      stop()
      modOpt.foreach { mod =>
        val start     = mod.bounds.start
        mod.position  = start
        mod.visible   = Span(start, start + mod.visible.length)
      }
    }

    private def rewind() = ()

    private def playOrStop(): Unit =
      atomic { implicit tx =>
        if (transport.isPlaying) transport.stop() else {
          transport.seek(timelineModel.position)
          transport.play()
        }
      }

    private def stop(): Unit =
      atomic { implicit tx => transport.stop() }

    private def play(): Unit =
      atomic { implicit tx =>
        transport.stop()
        transport.seek(timelineModel.position)
        transport.play()
      }

    private def fastForward() = ()

    private def toggleLoop(): Unit = {
      println("--todo : toggleLoop--")
      //      val sel       = timelineModel.selection
      //      val isLooping = atomic { implicit tx =>
      //        val loopSpan = if (transport.loop == Span.Void) sel else Span.Void
      //        transport.loop  = loopSpan
      //        !loopSpan.isEmpty
      //      }
      //      transportStrip.button(GUITransport.Loop).foreach(_.selected = isLooping)
    }

    //    private def playing: Boolean = playingVar
    //    private def playing_=(value: Boolean): Unit = {
    //      playingVar = value
    //      cueTimer.stop()
    //      for (ggPlay <- buttons.button(Play); ggStop <- buttons.button(Stop)) {
    //        ggPlay.selected = value
    //        ggStop.selected = !value
    //      }
    //    }

    //    def cue(mils: Long): Unit = {
    //      requireEDT()
    //      millis_=(mils)
    //    }

    //    private def millis_=(value: Long): Unit = {
    //      //println( "----millis_(" + value + "); was " + millisVar )
    //      if (millisVar != value) {
    //        millisVar = value
    //        lbTime.millis = value
    //      }
    //    }

    def guiInit(initPlaying: Boolean, initMillis: Long, hasMillis: Boolean, hasLoop: Boolean): Unit = {
      val timeDisplay = TimeDisplay(timelineModel, hasMillis = hasMillis)

      import GUITransport.{Action => _, _}
      val actions0 = Vector(
        GoToBegin   { rtz         () },
        Rewind      { rewind      () },
        Stop        { stop        () },
        Play        { play        () },
        FastForward { fastForward () }
      )
      val actions1 = if (hasLoop) actions0 :+ Loop { toggleLoop() } else actions0
      transportStrip = GUITransport.makeButtonStrip(actions1)
      transportStrip.button(Stop).foreach(_.selected = true)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents += timeDisplay.component
        contents += HStrut(8)
        contents += transportStrip
      }
      transportPane.addAction("play-stop", focus = FocusType.Window, action = new Action("play-stop") {
        accelerator = Some(KeyStrokes.plain + Key.Space)
        def apply(): Unit = playOrStop()
      })
      transportPane.addAction("rtz", focus = FocusType.Window, action = new Action("rtz") {
        accelerator = Some(KeyStrokes.plain + Key.Enter)
        enabled     = modOpt.isDefined
        def apply(): Unit =
          transportStrip.button(GoToBegin).foreach(_.doClick())
      })

      //       // use a prime number so that the visual milli update is nice
      //       playTimer = new javax.swing.Timer(47, new ActionListener {
      //         def actionPerformed(e: ActionEvent): Unit =
      //           millis_=(millisVar + 47)
      //       })

      playTimer = new javax.swing.Timer(47,
        Swing.ActionListener(modOpt.fold((_: ActionEvent) => ()) { mod => (e: ActionEvent) =>
          val elapsed = ((System.currentTimeMillis() - timerSys) * srm).toLong
          mod.position = timerFrame + elapsed
        })
      )

      cueTimer = new javax.swing.Timer(63, new ActionListener {
        def actionPerformed(e: ActionEvent): Unit = {
          //           if (!playing) atomic { implicit tx =>
          //             val t = transport
          //             t.seek(t.time + (t.sampleRate * 0.25 * cueDirection).toLong)
          //           }
        }
      })

      //      Seq(Rewind -> -1, FastForward -> 1).foreach {
      //        case (act, dir) => strip.button(act).foreach { b =>
      //          val m = b.peer.getModel
      //          m.addChangeListener(new ChangeListener {
      //            var pressed = false
      //
      //            def stateChanged(e: ChangeEvent): Unit = {
      //              val p = m.isPressed
      //              if (p != pressed) {
      //                pressed = p
      //                if (p) {
      //                  //println( "-restart" )
      //                  cueDirection = dir
      //                  cueTimer.restart()
      //                } else {
      //                  //println( "-stop" )
      //                  cueTimer.stop()
      //                }
      //              }
      //            }
      //          })
      //        }
      //      }

      // playing_=(initPlaying)
      // millis_=(initMillis)

      component = transportPane
    }
  }
}
