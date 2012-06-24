package de.sciss.synth.proc

import de.sciss.lucre.stm.{Cursor, Sys, InMemory}
import de.sciss.synth.expr.{Longs, ExprImplicits}
import de.sciss.lucre.expr.{Span, SpanLike}
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{WindowConstants, JFrame}
import de.sciss.nuages.VisualInstantPresentation

object VisTest {
   def apply() : VisTest[ InMemory ] = {
      implicit val system = InMemory()
      new VisTest( system )
   }
}
final class VisTest[ S <: Sys[ S ]]( system: S )( implicit cursor: Cursor[ S ]) extends ExprImplicits[ S ] {
   def t[ A ]( fun: S#Tx => A ) : A = cursor.step( fun )

   val (group, trans) = t { implicit tx =>
      implicit def longType = Longs
      val g = ProcGroupX.newVar[ S ]
      g.changed.react { upd =>
         println( "Group observed: " + upd )
      }
      val tr   = Transport( g )
      tr.changed.react { upd =>
         println( "Transport observed: " + upd )
      }
      val trv  = tx.newVar[ Transport[ S, Proc[ S ]]]( tr.id, tr )
      (g, trv)
   }

   def proc( name: String ) : Proc[ S ] = t { implicit tx =>
      val res = Proc[ S ]()
      res.name = name
      res
   }

   def next( time: Long ) : Option[ Long ] = t { implicit tx =>
      group.nearestEventAfter( time )
   }

   def prev( time: Long ) : Option[ Long ] = t { implicit tx =>
      group.nearestEventBefore( time )
   }

   def add( span: SpanLike = Span( 33, 44 ), name: String = "Proc" ) {
      t { implicit tx => group.add( span, proc( name ))}
   }

   def play() {
      t { implicit tx => trans.get.playing = true }
   }

   def stop() {
      t { implicit tx => trans.get.playing = false }
   }

   def rewind() { seek( 0L )}

   def seek( pos: Long ) { t { implicit tx =>
      val tr = trans.get
      tr.playing = false
      tr.seek( pos )
   }}

   def within( span: SpanLike ) = t { implicit tx =>
      group.intersect( span ).toIndexedSeq
   }

   def range( start: SpanLike, stop: SpanLike ) = t { implicit tx =>
      group.rangeSearch( start, stop ).toIndexedSeq
   }

   def defer( thunk: => Unit ) { EventQueue.invokeLater( new Runnable {
      def run() { thunk }
   })}

   private var frameVar: JFrame = null
   def frame = frameVar

   def gui() {
      if( frame == null ) defer {
         val f    = new JFrame( "Vis" )
         frameVar = f
         val cp   = f.getContentPane
         val vis  = VisualInstantPresentation( system.asEntry( trans ))
         cp.add( vis.view, BorderLayout.CENTER )
         f.pack()
         f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
         f.setLocationRelativeTo( null )
         f.setLocation( f.getX, 0 )
         f.setAlwaysOnTop( true )
         f.setVisible( true )
      }
   }
}
