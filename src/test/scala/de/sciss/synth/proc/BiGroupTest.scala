package de.sciss.synth.proc

import de.sciss.lucre.stm.{InMemory, Cursor, Sys}
import de.sciss.synth.expr.{ExprImplicits, Longs}
import de.sciss.lucre.expr.{Span, BiGroup}

object BiGroupTest {
   def apply() : BiGroupTest[ InMemory ] = new BiGroupTest( InMemory() )
}
class BiGroupTest[ S <: Sys[ S ]]( cursor: Cursor[ S ]) extends ExprImplicits[ S ] {
   def t[ A ]( fun: S#Tx => A ) : A = cursor.step( fun )

   val bi = t { implicit tx =>
      implicit def longType = Longs
      BiGroup.newVar[ S, Long ]
   }

   def add( span: Span = Span( 33, 44 ), elem: Int = 55 ) {
      t { implicit tx => bi.add( span, elem )}
   }

   def at( time: Long ) = t { implicit tx =>
      bi.iteratorAt( time ).toIndexedSeq
   }
}
