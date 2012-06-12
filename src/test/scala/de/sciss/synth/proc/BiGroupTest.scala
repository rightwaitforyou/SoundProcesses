package de.sciss.synth.proc

import de.sciss.lucre.stm.{InMemory, Cursor, Sys}
import de.sciss.synth.expr.{ExprImplicits, Longs}
import de.sciss.lucre.expr.{Span, BiGroup}

object BiGroupTest {
   def mem() : BiGroupTest[ InMemory ] = new BiGroupTest( InMemory() )
}
class BiGroupTest[ S <: Sys[ S ]]( cursor: Cursor[ S ]) extends ExprImplicits[ S ] {
   def t[ A ]( fun: S#Tx => A ) : A = cursor.step( fun )

   val bi = t { implicit tx =>
      implicit def longType = Longs
      BiGroup.newVar[ S, Long ]
   }

   def testAdd() {
      t { implicit tx => bi.add( Span( 33, 44 ), 55 )}
   }

   def at( time: Long ) = t { implicit tx =>
      bi.iteratorAt( time ).toIndexedSeq
   }
}
