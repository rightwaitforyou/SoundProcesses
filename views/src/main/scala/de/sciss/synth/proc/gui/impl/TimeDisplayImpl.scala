/*
 *  TimeDisplayImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.audiowidgets.{TimelineModel, LCDPanel, LCDColors, LCDFont, AxisFormat}
import de.sciss.desktop.impl.DynamicComponentImpl
import scala.swing.{Swing, Orientation, BoxPanel, Component, Label}
import Swing._
import de.sciss.model.Change

final class TimeDisplayImpl(model: TimelineModel, hasMillis: Boolean) extends TimeDisplay {
  private val lcdFormat = AxisFormat.Time(hours = true, millis = hasMillis)
  private val lcd: Label = new Label with DynamicComponentImpl {
    // protected def component: Component = this

    private val decimals  = if (hasMillis)  3 else 0
    private val pad       = if (hasMillis) 12 else 8

    private def updateText(frame: Long): Unit = {
      val secs = frame / model.sampleRate
      text = lcdFormat.format(secs, decimals = decimals, pad = pad)
    }

    private val tlmListener: TimelineModel.Listener = {
      case TimelineModel.Position(_, Change(_, frame)) =>
        updateText(frame)
    }

    protected def componentShown(): Unit = {
      model.addListener(tlmListener)
      updateText(model.position)
    }

    protected def componentHidden(): Unit =
      model.removeListener(tlmListener)

    //    override protected def paintComponent(g2: java.awt.Graphics2D): Unit = {
    //      val atOrig  = g2.getTransform
    //      try {
    //        // stupid lcd font has wrong ascent
    //        g2.translate(0, 3)
    //        // g2.setColor(java.awt.Color.red)
    //        // g2.fillRect(0, 0, 100, 100)
    //        super.paintComponent(g2)
    //      } finally {
    //        g2.setTransform(atOrig)
    //      }
    //    }

    font        = LCDFont() // .deriveFont(11.5f)
    foreground  = LCDColors.defaultFg
    updateText(model.position)

    maximumSize = preferredSize
    minimumSize = preferredSize
  }
  //      lcd.setMinimumSize(lcd.getPreferredSize)
  //      lcd.setMaximumSize(lcd.getPreferredSize)
  private val lcdFrame  = new LCDPanel {
    contents   += lcd
    maximumSize = preferredSize
    minimumSize = preferredSize
  }
  private val lcdPane = new BoxPanel(Orientation.Vertical) {
    contents += VGlue
    contents += lcdFrame
    contents += VGlue
  }

  def component: Component = lcdPane
}