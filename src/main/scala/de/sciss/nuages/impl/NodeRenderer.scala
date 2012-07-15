package de.sciss.nuages.impl

import prefuse.render.AbstractShapeRenderer
import prefuse.visual.VisualItem
import java.awt.{Graphics2D, Shape}
import java.awt.geom.{Point2D, RoundRectangle2D}
import annotation.switch
import prefuse.Constants
import prefuse.util.ColorLib

object NodeRenderer {
//   val LABEL = "nuages.label"

   private def calcAlignedPoint( p: Point2D, vi: VisualItem, w: Double, h: Double, xAlign: Int, yAlign: Int ) {
      val xShift =  (xAlign: @switch) match {
         case Constants.CENTER => -w/2
         case Constants.RIGHT  => -w
         case _ => 0.0
      }
      val yShift =  (yAlign: @switch) match {
         case Constants.CENTER => -h/2
         case Constants.RIGHT  => -h
         case _ => 0.0
      }

      val x = {
         val x0 = vi.getX
         if( java.lang.Double.isNaN( x0 ) || java.lang.Double.isInfinite( x0 )) 0.0 else x0
      }

      val y = {
         val y0 = vi.getY
         if( java.lang.Double.isNaN( y0 ) || java.lang.Double.isInfinite( y0 )) 0.0 else y0
      }

       p.setLocation( x + xShift, y + yShift )
   }
}
final class NodeRenderer( val labelField: String = VisualItem.LABEL ) extends AbstractShapeRenderer {
   import NodeRenderer._

   private val shape = new RoundRectangle2D.Double()
   private val pt    = new Point2D.Double()

   protected def getRawShape( vi: VisualItem ) : Shape = {
      val w = 100.0
      val h = 40.0
      calcAlignedPoint( pt, vi, w, h, Constants.CENTER, Constants.CENTER )
      shape.setRoundRect( pt.x, pt.y, w, h, 4.0, 4.0 )
      shape
   }

   private def getText( vi: VisualItem ) : Option[ String ] = {
      if( vi.canGetString( labelField )) Option( vi.getString( labelField )) else None
   }

   override def render( g: Graphics2D, vi: VisualItem ) {
      super.render( g, vi )
      getText( vi ).foreach { name =>
         g.setPaint( ColorLib.getColor( vi.getTextColor ))
         g.drawString( name, shape.x.toFloat + 6f, shape.y.toFloat + 12f )
//         println( "Aqui: " + name )
      }
   }
}
