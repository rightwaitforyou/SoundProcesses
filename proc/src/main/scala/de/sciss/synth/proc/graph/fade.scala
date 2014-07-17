/*
 *  fade.scala
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

package de.sciss.synth
package proc
package graph

import de.sciss.synth.proc.UGenGraphBuilder
import de.sciss.synth.proc.UGenGraphBuilder.Input
import ugen._

object Fade {
  private[graph] abstract class Base extends GE.Lazy {
    protected def mkEnv(b: UGenGraphBuilder[_], dur: GE): IEnv

    def rate: Rate

    final def makeUGens: UGenInLike = {
      val b = UGenGraphBuilder.get
      val off     = Offset.ir
      val dur     = Duration.ir
      val phase   = Line.ar(off, dur, dur - off /*, doneAction = freeSelf */)
      val env     = mkEnv(b, dur)
      IEnvGen.ar(env, phase)
    }

    /** Returns (dur, shape, floor) */
    final protected def readCtl(b: UGenGraphBuilder[_], key: String): (GE, Env.Curve, GE) = {
      val numCh   = b.requestInput(Input.Attribute(key, numChannels = 4)).value
      assert (numCh == 4)
      // if (numCh != 4) throw new IllegalStateException(s"$this - requires a 4-channel attribute (found $numCh)")
      // b.addAttributeIn(key)
      val ctlName = attribute.controlName(key)
      val ctl     = ctlName.ir(Seq(0f, 0f, 0f, 0f))  // dur, shape-id, shape-curvature, floor
      (ctl \ 0, Env.Curve(ctl \ 1, ctl \ 2), ctl \ 3)
    }
  }

  private[graph] abstract class SingleBase extends Base {
    protected def key: String

    final protected def mkEnv(b: UGenGraphBuilder[_], dur: GE): IEnv = {
      val (fadeDur, shape, floor) = readCtl(b, key)
      mkSingleEnv(dur, fadeDur, shape, floor)
    }

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv
  }

  private[graph] final case class In(key: String, rate: Rate) extends SingleBase {

    override def productPrefix  = "Fade$In"
    override def toString       = s"""FadeIn("$key").${rate.methodName}"""

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv =
      IEnv(floor, Env.Segment(fadeDur, 1, shape) :: Nil)
  }

  // @SerialVersionUID(6793156274707521366L)
  private[graph] final case class Out(key: String, rate: Rate)  extends SingleBase {

    override def productPrefix  = "Fade$Out"
    override def toString       = s"""FadeOut("$key").${rate.methodName}"""

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv =
      IEnv(1, Env.Segment(fadeDur, floor, shape) :: Nil, totalDur - fadeDur)
  }

  // @SerialVersionUID(6793156274707521366L)
  private[graph] final case class InOut(inKey: String, outKey: String, rate: Rate) extends Base {

    override def productPrefix  = "Fade$InOut"
    override def toString       = s"""FadeInOut("$inKey", "$outKey").${rate.methodName}"""

    protected def mkEnv(b: UGenGraphBuilder[_], totalDur: GE): IEnv = {
      val (fadeDurIn , shapeIn , floorIn ) = readCtl(b, inKey )
      val (fadeDurOut, shapeOut, floorOut) = readCtl(b, outKey)
      IEnv(floorIn,
        Env.Segment(fadeDurIn, 1, shapeIn) ::
        Env.Segment(totalDur - (fadeDurIn + fadeDurOut), 1, Curve.step) ::
        Env.Segment(fadeDurOut, floorOut, shapeOut) :: Nil
      )
    }
  }
}
final case class FadeIn(key: String) {
  def kr: GE = Fade.In(key, control)
  def ar: GE = Fade.In(key, audio  )
}

final case class FadeOut(key: String) {
  def kr: GE = Fade.Out(key, control)
  def ar: GE = Fade.Out(key, audio  )
}

final case class FadeInOut(inKey: String, outKey: String) {
  def kr: GE = Fade.InOut(inKey, outKey, control)
  def ar: GE = Fade.InOut(inKey, outKey, audio  )
}