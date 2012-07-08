package de.sciss.lucre.expr

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import collection.immutable.{IndexedSeq => IIdxSeq}

class SpanSpec extends FlatSpec with ShouldMatchers {
   val span1   = Span( 30, 40 )
   val span2   = Span( 40, 50 )
   val span3   = Span( 40, 40 )
   val span4   = Span( 30, 50 )
   val span5   = Span( 35, 45 )
   val span6   = Span( 31, 39 )
   val sfrom   = Span.from( 40 )
   val suntl   = Span.until( 40 )
   val salle   = Span.all
   val svoid   = Span.void
   val sseq    = IIdxSeq[ SpanLike ]( span1, span2, span3, sfrom, suntl, salle, svoid )
   val spans   = IIdxSeq( span1, span2, span3 )
   val sseqi   = sseq.zipWithIndex
   val sseq2   = sseq ++ IIdxSeq( span4, span5, span6 )
   val pos     = IIdxSeq( 25, 30, 35, 40, 45, 50, 55 )
   val shifts  = IIdxSeq( -3, 0, 3 )

   val mcont   = IIdxSeq(
   // span1  span2  span3  sfrom  suntl  salle  svoid  span4  span5  span6
      true,  false, true,  false, false, false, false, false, false, true,    // span1
      false, true,  true,  false, false, false, false, false, false, false,   // span2
      false, false, true,  false, false, false, false, false, false, false,   // span3
      false, true,  true,  true,  false, false, false, false, false, false,   // sfrom
      true,  false, true,  false, true,  false, false, false, false, true,    // suntl
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // salle
      false, false, false, false, false, false, false, false, false, false,   // svoid
      true,  true,  true,  false, false, false, false, true,  true,  true,    // span4
      false, false, true,  false, false, false, false, false, true,  false,   // span5
      false, false, false, false, false, false, false, false, false, true     // span6
   )
   val mover   = IIdxSeq(
   // span1  span2  span3  sfrom  suntl  salle  svoid  span4  span5  span6
      true,  false, false, false, true,  true,  false, true,  true,  true,    // span1
      false, true,  false, true,  false, true,  false, true,  true,  false,   // span2
      false, false, false, false, false, false, false, false, false, false,   // span3
      false, true,  false, true,  false, true,  false, true,  true,  false,   // sfrom
      true,  false, false, false, true,  true,  false, true,  true,  true,    // suntl
      true,  true,  false, true,  true,  true,  false, true,  true,  true,    // salle
      false, false, false, false, false, false, false, false, false, false,   // svoid
      true,  true,  false, true,  true,  true,  false, true,  true,  true,    // span4
      true,  true,  false, true,  true,  true,  false, true,  true,  true,    // span5
      true,  false, false, false, true,  true,  false, true,  true,  true     // span6
   )
   val mtouch  = IIdxSeq(
   // span1  span2  span3  sfrom  suntl  salle  svoid  span4  span5  span6
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // span1
      true,  true,  true,  true,  true,  true,  false, true,  true,  false,   // span2
      true,  true,  true,  true,  true,  true,  false, true,  true,  false,   // span3
      true,  true,  true,  true,  true,  true,  false, true,  true,  false,   // sfrom
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // suntl
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // salle
      false, false, false, false, false, false, false, false, false, false,   // svoid
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // span4
      true,  true,  true,  true,  true,  true,  false, true,  true,  true,    // span5
      true,  false, false, false, true,  true,  false, true,  true,  true     // span6
   )

   val from30 = Span.from(30)
   val from35 = Span.from(35)
   val from31 = Span.from(31)
   val untl50 = Span.until(50)
   val untl45 = Span.until(45)
   val span7  = Span(30,45)
   val span8  = Span(31,40)
   val span9  = Span(35,50)
   val span10 = Span(31,45)
   val span11 = Span(31,50)
   val munion = IIdxSeq(
   // span1   span2   span3   sfrom   suntl   salle  svoid  span4   span5   span6
      span1,  span4,  span1,  from30, suntl,  salle, span1, span4,  span7,  span1,     // span1
      span4,  span2,  span2,  sfrom,  untl50, salle, span2, span4 , span9,  span11,    // span2
      span1,  span2,  span3,  sfrom,  suntl,  salle, span3, span4,  span5,  span8,     // span3
      from30, sfrom,  sfrom,  sfrom,  salle,  salle, sfrom, from30, from35, from31,    // sfrom
      suntl,  untl50, suntl,  salle,  suntl,  salle, suntl, untl50, untl45, suntl,     // suntl
      salle,  salle,  salle,  salle,  salle,  salle, salle, salle,  salle,  salle,     // salle
      span1,  span2,  span3,  sfrom,  suntl,  salle, svoid, span4,  span5,  span6,     // svoid
      span4,  span4,  span4,  from30, untl50, salle, span4, span4,  span4,  span4,     // span4
      span7,  span9,  span5,  from35, untl45, salle, span5, span4,  span5,  span10,    // span5
      span1,  span11, span8,  from31, suntl,  salle, span6, span4,  span10, span6      // span6
   )

   //   val span1   = Span( 30, 40 )
   //   val span2   = Span( 40, 50 )
   //   val span3   = Span( 40, 40 )
   //   val span4   = Span( 30, 50 )
   //   val span5   = Span( 35, 45 )
   //   val span6   = Span( 31, 39 )

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

   it should "correctly respond to `clip` and `contains` for given points" in {
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

   it should "correctly handle `shift` for given deltas" in {
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

   it should "correctly answer `contains` with respect to other spans" in {
      mcont.zipWithIndex.foreach { case (res, idx) =>
         val row = idx / sseq2.size
         val col = idx % sseq2.size
//println( "" + row + " - " + col )
         sseq2( row ).contains( sseq2( col )) should equal (res)
      }
   }

   it should "correctly answer `overlaps` with respect to other spans" in {
      mover.zipWithIndex.foreach { case (res, idx) =>
         val row = idx / sseq2.size
         val col = idx % sseq2.size
         val res1 = sseq2( row ).overlaps( sseq2( col ))
         val res2 = sseq2( col ).overlaps( sseq2( row ))
         res1 should equal (res)
         res1 should equal (res2)                  // commutative
      }
   }

   it should "correctly answer `touches` with respect to other spans" in {
      mtouch.zipWithIndex.foreach { case (res, idx) =>
         val row = idx / sseq2.size
         val col = idx % sseq2.size
         val res1 = sseq2( row ).touches( sseq2( col ))
         val commut = sseq2( col ).touches( sseq2( row ))
         val overlaps = sseq2( row ).overlaps( sseq2( col ))
         res1 should equal (res)
         res1 should equal (commut)                   // commutative
         if( overlaps ) res1 should equal (true)      // overlap is stronger condition
      }
   }

   it should "correctly answer `union` with respect to other spans" in {
      munion.zipWithIndex.foreach { case (res, idx) =>
         val row     = idx / sseq2.size
         val col     = idx % sseq2.size
         val res1    = sseq2( row ).union( sseq2( col ))
         val commut  = sseq2( col ).union( sseq2( row ))
//         val touches = sseq2( row ).touches( sseq2( col ))
         val contains = sseq2( row ).contains( sseq2( col ))
//println( "" + row + " - " + col )
         res1 should equal (res)
         res1 should equal (commut)                // commutative
//         if( !touches ) res1 should equal (svoid)
         if( contains ) res1 should equal (sseq2( row ))
      }
   }

   // intersect, subtract

   "A Span.Open" should "produce a correct `invert`" in {
      sfrom.invert should equal (suntl)
      suntl.invert should equal (sfrom)
      salle.invert should equal (svoid)
      svoid.invert should equal (salle)
   }

   "A Span" should "throw an IllegalArgumentException if stop < start" in {
     evaluating { Span( 40, 39 )} should produce [IllegalArgumentException]
   }
}
