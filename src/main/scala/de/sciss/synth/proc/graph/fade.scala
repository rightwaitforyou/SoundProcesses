package de.sciss.synth
package proc
package graph

import ugen._

object Fade {
  private[graph] abstract class Base extends GE.Lazy {
    protected def mkEnv(segm: Env.Seg, floor: GE): Env

    protected def key: String
    def rate: Rate

    final def makeUGens: UGenInLike = {
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          b.addAttributeIn(key)
          val ctlName = attribute.controlName(key)
          val ctl     = ctlName.ir(0f, 0f, 0f, 0f)  // dur, shape-id, shape-curvature, floor
          val segm    = Env.Seg(ctl \ 0, 1, varShape(ctl \ 1, ctl \ 2))
          val env     = mkEnv(segm, ctl \3)
          EnvGen(rate, env) // .expand

        case other => UGenGraphBuilder.outsideOfContext()
      }
    }
  }

  @SerialVersionUID(6793156274707521366L) private[graph] final case class In(key: String, rate: Rate) extends Base {

    def displayName = "FadeIn"

    override def toString = s"""$displayName("$key", $rate)"""

    protected def mkEnv(segm: Env.Seg, floor: GE): Env = {
      Env(floor, segm :: Nil)
    }
  }
}
final case class FadeIn(key: String) {
  def kr: GE = Fade.In(key, control)
  def ar: GE = Fade.In(key, audio  )
}


//val sig =
//val env = fade.In("adfsd").ar
//val bus = attribute("bus").ir
//Out.ar(sig, eenv)
