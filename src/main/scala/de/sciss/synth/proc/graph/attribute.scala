package de.sciss.synth
package proc
package graph

object attribute {
  private[proc] def controlName(key: String): String = "$attr_"  + key

  @SerialVersionUID(6793156274707521366L) private final case class AttributeIn(key: String, default: Double)
    extends GE.Lazy /* with Elem */ with ScalarRated {

    def displayName = "AttributeIn"

    override def toString = s"""$displayName("$key")"""

    def makeUGens: UGenInLike = {
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          b.addAttributeIn(key)
          val ctlName = controlName(key)
          ctlName.ir(default).expand

        case other => UGenGraphBuilder.outsideOfContext()
      }
    }
  }
}
final case class attribute(key: String) {
  def ir: GE = ir(0.0)
  def ir(default: Double): GE = attribute.AttributeIn(key, default)
}