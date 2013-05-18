package de.sciss.synth
package proc
package graph

import ugen._

object Fade {
  private[graph] abstract class Base extends GE.Lazy {
    protected def mkEnv(b: UGenGraphBuilder[_], dur: GE): IEnv

    def rate: Rate

    final def makeUGens: UGenInLike = {
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          val off     = Offset.ir
          val dur     = Duration.ir
          val phase   = Line.ar(off, dur, dur - off /*, doneAction = freeSelf */)
          val env     = mkEnv(b, dur)
          IEnvGen.ar(env, phase)

        case other => UGenGraphBuilder.outsideOfContext()
      }
    }

    /** Returns (dur, shape, floor) */
    final protected def readCtl(b: UGenGraphBuilder[_], key: String): (GE, Env.Shape, GE) = {
      b.addAttributeIn(key)
      val ctlName = attribute.controlName(key)
      val ctl     = ctlName.ir(0f, 0f, 0f, 0f)  // dur, shape-id, shape-curvature, floor
      (ctl \ 0, varShape(ctl \ 1, ctl \ 2), ctl \ 3)
    }
  }

  private[graph] abstract class SingleBase extends Base {
    protected def key: String

    def displayName: String

    override def toString = s"""$displayName("$key", $rate)"""

    final protected def mkEnv(b: UGenGraphBuilder[_], dur: GE): IEnv = {
      val (fadeDur, shape, floor) = readCtl(b, key)
      mkSingleEnv(dur, fadeDur, shape, floor)
    }

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Shape, floor: GE): IEnv
  }

  @SerialVersionUID(6793156274707521366L)
  private[graph] final case class In(key: String, rate: Rate) extends SingleBase {

    def displayName = "FadeIn"

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Shape, floor: GE): IEnv =
      IEnv(floor, Env.Seg(fadeDur, 1, shape) :: Nil)
  }

  @SerialVersionUID(6793156274707521366L)
  private[graph] final case class Out(key: String, rate: Rate)  extends SingleBase {

    def displayName = "FadeOut"

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Shape, floor: GE): IEnv =
      IEnv(1, Env.Seg(fadeDur, floor, shape) :: Nil, totalDur - fadeDur)
  }

  @SerialVersionUID(6793156274707521366L)
  private[graph] final case class InOut(inKey: String, outKey: String, rate: Rate) extends Base {

    def displayName = "FadeInOut"

    protected def mkEnv(b: UGenGraphBuilder[_], totalDur: GE): IEnv = {
      val (fadeDurIn , shapeIn , floorIn ) = readCtl(b, inKey )
      val (fadeDurOut, shapeOut, floorOut) = readCtl(b, outKey)
      IEnv(floorIn,
        Env.Seg(fadeDurIn, 1, shapeIn) ::
        Env.Seg(totalDur - (fadeDurIn + fadeDurOut), 1, stepShape) ::
        Env.Seg(fadeDurOut, floorOut, shapeOut) :: Nil
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