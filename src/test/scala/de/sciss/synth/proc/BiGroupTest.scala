package de.sciss.synth.proc

import de.sciss.lucre.{stm, expr, bitemp, event => evt}
import stm.Cursor
import evt.{InMemory, Sys}
import de.sciss.synth.expr.{SpanLikes, ExprImplicits, Longs}
import bitemp.{SpanLike, Span, BiGroup}
import expr.Expr

object BiGroupTest {
   def apply() : BiGroupTest[ InMemory ] = new BiGroupTest( InMemory() )
}
class BiGroupTest[ S <: Sys[ S ]]( cursor: Cursor[ S ]) extends ExprImplicits[ S ] {
   def t[ A ]( fun: S#Tx => A ) : A = cursor.step( fun )

   val bi = t { implicit tx =>
      implicit def longType = Longs
      val res = BiGroup.Expr.Modifiable[ S, Long ]
      res.changed.react { upd =>
         println( "Observed: " + upd )
      }
      res
   }

   def add( span: SpanLike = Span( 33, 44 ), elem: Long = 55 ) {
      t { implicit tx => bi.add( span, elem )}
   }

   def addValVar( span: SpanLike = Span( 33, 44 ), init: Long = 66 ) : Expr.Var[ S, Long ] = {
      t { implicit tx =>
         val elem = Longs.newVar[ S ]( init )
         bi.add( span, elem )
         elem
      }
   }

   def addKeyVar( init: SpanLike = Span( 33, 44 ), elem: Long = 77 ) : Expr.Var[ S, SpanLike ] = {
      t { implicit tx =>
         val span = SpanLikes.newVar[ S ]( init )
         bi.add( span, elem )
         span
      }
   }

   def at( time: Long ) = t { implicit tx =>
      bi.intersect( time ).toIndexedSeq
   }

   def within( span: SpanLike ) = t { implicit tx =>
      bi.intersect( span ).toIndexedSeq
   }

   def after( time: Long ) = t { implicit tx =>
      bi.nearestEventAfter( time )
   }

   def before( time: Long ) = t { implicit tx =>
      bi.nearestEventBefore( time )
   }

   def eventsAt( time: Long ) = t { implicit tx =>
      val (a, b) = bi.eventsAt( time )
      (a.toIndexedSeq, b.toIndexedSeq)
   }

   def list() { val li = t { implicit tx =>
         bi.debugList()
      }
      println( li )
   }
}
