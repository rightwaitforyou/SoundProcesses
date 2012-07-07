package de.sciss.lucre.expr

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import collection.immutable.{IndexedSeq => IIdxSeq}

class SpanSpec extends FlatSpec with ShouldMatchers {
   val span1   = Span( 30, 40 )
   val span2   = Span( 40, 50 )
   val span3   = Span( 40, 40 )
   val sfrom   = Span.from( 40 )
   val suntl   = Span.until( 40 )
   val salle   = Span.all
   val svoid   = Span.void
   val sseq    = IIdxSeq[ SpanLike ]( span1, span2, span3, sfrom, suntl, salle, svoid )
   val spans   = IIdxSeq( span1, span2, span3 )
   val sseqi   = sseq.zipWithIndex
   val pos     = IIdxSeq( 25, 30, 35, 40, 45, 50, 55 )
   val shifts  = IIdxSeq( -3, 0, 3 )

   val span4   = Span( 30, 50 )
   val span5   = Span( 35, 45 )
   val span6   = Span( 31, 39 )
   val sseq2   = sseq ++ IIdxSeq( span4, span5, span6 )
   val mcont   = IIdxSeq(
   // span1  span2  span3  sfrom  suntl  salle  svoid  span4  span5  span6
      true,  false, true,  false, false, false, false, false, false, true,    // span1
      false, true,  true,  false, false, false, false, false, false, false,   // span2
      false, false, true,  false, false, false, false, false, false, false,   // span3
      false, true,  true,  true,  false, false, false, false, false, false,   // sfrom
      true,  false, true,  false, true,  false, false, false, false, true,    // suntl
      true,  true,  true,  true,  true,  true,  true,  true,  true,  true,    // salle
      false, false, false, false, false, false, false, false, false, false,   // svoid
      true,  true,  true,  false, false, false, false, true,  true,  true,    // span4
      false, false, true,  false, false, false, false, false, true,  false,   // span5
      false, false, false, false, false, false, false, false, false, true
   )


   "A SpanLike" should "be consistently test for equality" in {
      sseqi.foreach { case (s1, idx1) =>
         sseqi.foreach { case (s2, idx2) =>
            if( idx1 == idx2 ) {
               s1 should equal (s2)
            } else {
               s1 should not equal (s2)
            }
         }
      }
   }

   it should "correctly report its start, stop and length" in {
      spans.foreach { sp => sp.length should equal (sp.stop - sp.start)}
      span1.start should equal (30)
      span1.stop  should equal (40)
      span2.start should equal (40)
      span2.stop  should equal (50)
      span3.start should equal (40)
      span3.stop  should equal (40)

      sfrom.start should equal (40)
      suntl.stop  should equal (40)
   }

   it should "report correctly whether its empty" in {
      sseq.foreach { sp =>
         val isEmpty = sp match {
            case Span.Void => true
            case Span( start, stop ) if start == stop => true
            case _ => false
         }
         sp.isEmpty  should equal (isEmpty)
         sp.nonEmpty should equal (!isEmpty)
      }
   }

   it should "correctly respond to clip and contains for given points" in {
      spans.foreach { sp =>
         pos.foreach { p =>
            val cl = sp.clip( p )
            sp.contains( p ) should equal (p >= sp.start && p < sp.stop)
            if( sp.contains( p ) || sp.stop == p ) {
               p should equal (cl)
            } else {
               p should not equal (cl)
            }
         }
      }
      pos.foreach { p =>
         svoid.contains( p ) should equal (false)
         p should equal (svoid.clip( p ))
      }
      pos.foreach { p =>
         salle.contains( p ) should equal (true)
         p should equal (salle.clip( p ))
      }
      pos.foreach { p =>
         sfrom.contains( p ) should equal (p >= sfrom.start)
         val cl = sfrom.clip( p )
         if( sfrom.contains( p )) {
            p should equal (cl)
         } else {
            p should not equal (cl)
         }
      }
      pos.foreach { p =>
         suntl.contains( p ) should equal (p < suntl.stop)
         val cl = suntl.clip( p )
         if( suntl.contains( p ) || (suntl.stop == p) ) {
            p should equal (cl)
         } else {
            p should not equal (cl)
         }
      }
   }

   it should "correctly handle shift for given deltas" in {
      shifts.foreach { d =>
         spans.foreach { sp =>
            sp.shift( d ) should equal (Span( sp.start + d, sp.stop + d ))
         }
         sfrom.shift( d ) should equal (Span.from( sfrom.start + d ))
         suntl.shift( d ) should equal (Span.until( suntl.stop + d ))
         salle.shift( d ) should equal (salle)
         svoid.shift( d ) should equal (svoid)
      }
   }

   it should "correctly answer contains with respect to other spans" in {
      mcont.zipWithIndex.foreach { case (res, idx) =>
         val row = idx / sseq2.size
         val col = idx % sseq2.size
//println( "" + row + " - " + col )
         sseq2( row ).contains( sseq2( col )) should equal (res)
      }
   }

   // contains, overlaps, touches ; union, intersect, subtract

   "A Span.Open" should "produce a correct inversion" in {
      sfrom.invert should equal (suntl)
      suntl.invert should equal (sfrom)
      salle.invert should equal (svoid)
      svoid.invert should equal (salle)
   }

   "A Span" should "throw an IllegalArgumentException if stop < start" in {
     evaluating { Span( 40, 39 )} should produce [IllegalArgumentException]
   }
}
