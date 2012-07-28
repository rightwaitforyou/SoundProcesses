package de.sciss.nuages.impl

import prefuse.render.AbstractShapeRenderer
import prefuse.visual.VisualItem
import java.awt.{RenderingHints, Graphics2D, Shape}
import java.awt.geom.{Line2D, Point2D, RoundRectangle2D}
import annotation.switch
import prefuse.Constants
import prefuse.util.ColorLib
import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale

object NodeRenderer {
//   val LABEL = "nuages.label"

   private val parValFmt = {
      val res = NumberFormat.getInstance( Locale.US )
      res match {
         case df: DecimalFormat =>
            df.setMinimumFractionDigits( 1 )
            df.setMaximumFractionDigits( 1 )
         case _ =>
      }
      res
   }

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
final class NodeRenderer( val dataColumn: String ) extends AbstractShapeRenderer {
   import NodeRenderer._

   private val shape = new RoundRectangle2D.Double()
   private val pt    = new Point2D.Double()
   private val ln    = new Line2D.Double()

   protected def getRawShape( vi: VisualItem ) : Shape = {
      val w = 100.0
      val numPar = getData( vi ).map( _.par.size ).getOrElse( 0 )
      val h = 30.0 + numPar * 15
      calcAlignedPoint( pt, vi, w, h, Constants.CENTER, Constants.CENTER )
      shape.setRoundRect( pt.x, pt.y, w, h, 4.0, 4.0 )
      shape
   }

   private def getData( vi: VisualItem ) : Option[ VisualProc ] = {
      if( vi.canGet( dataColumn, classOf[ VisualProc ])) Option( vi.get( dataColumn ).asInstanceOf[ VisualProc ]) else None
   }

   override def render( g: Graphics2D, vi: VisualItem ) {
//      g.setRenderingHint( RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON )
//      g.setRenderingHint( RenderingHints.KEY_STROKE_CONTROL,    RenderingHints.VALUE_STROKE_PURE )
      super.render( g, vi )
      getData( vi ).foreach { vp =>
         g.setPaint( ColorLib.getColor( vi.getTextColor ))
         val x  = shape.x
         val tx = x.toFloat + 6f
         var ty = shape.y.toFloat + 14f
         g.drawString( vp.name, tx, ty )
//         println( "Aqui: " + name )
         val par = vp.par
         if( par.nonEmpty ) {
            ln.setLine( x, ty + 5, x + shape.width - 1, ty + 5 )
            g.draw( ln )
            ty += 5
            par.foreach { case (parName, parVal) =>
               ty += 15f
               g.drawString( parName, tx, ty )
               g.drawString( parValFmt.format( parVal ), tx + 44f, ty )
            }
         }
      }
   }
}
