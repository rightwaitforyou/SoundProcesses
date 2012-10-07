//package de.sciss.synth.proc
//
//import org.scalatest.fixture
//import org.scalatest.matchers.ShouldMatchers
//import de.sciss.synth.expr.{Doubles, Longs, ExprImplicits, Ints}
//import de.sciss.lucre.{stm, confluent}
//import confluent.reactive.ConfluentReactive
//import concurrent.stm.TxnLocal
//import collection.immutable.{IndexedSeq => IIdxSeq}
//import de.sciss.lucre.expr.Expr
//import de.sciss.synth.{linShape, Env}
//import de.sciss.lucre.bitemp.Span
//
///**
// * To run only this suite:
// *
// * test-only de.sciss.synth.proc.ScanSpec
// */
//class ScanSpec extends fixture.FlatSpec with ShouldMatchers {
//   type FixtureParam = ConfluentReactive
//   type S = FixtureParam
//
//   implicit val IntType = Ints
//
//   def withFixture( test: OneArgTest ) {
//      val system = ConfluentReactive.tmp()
//      try {
//         system.root( _ => () )
//         test( system )
//      }
//      finally {
//         system.close()
//      }
//   }
//
//   private final class Observation[ S <: stm.Sys[ S ]] {
//      private val seqRef = TxnLocal( init = IIdxSeq.empty[ Any ])
//
//      def register( tx: S#Tx )( upd: Any ) {
//         seqRef.transform( _ :+ upd )( tx.peer )
//      }
//
//      def assertEquals( expected: Any* )( implicit tx: S#Tx ) {
//         val ob = seqRef.get( tx.peer )
//         assert( ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString( "[", ", ", "]" )
//            + "\n...but observed\n   " + ob.mkString( "[", ", ", "]" ))
//      }
//
//      def clear()( implicit tx: S#Tx ) {
//         seqRef.set( IIdxSeq.empty )( tx.peer )
//      }
//
//      def assertEmpty()( implicit tx: S#Tx ) { assertEquals() }
//   }
//
//   "Proc" should "notify observers about all relevant events" in { system =>
//      val obs  = new Observation[ S ]
//      val ph   = system.step { implicit tx =>
//         val p = Proc[ S ]
//         p.changed.reactTx( obs.register )
//         val res = tx.newHandle( p )( Proc.serializer[ S ])
//         obs.assertEmpty()
//         res
//      }
//
////      implicit val bipSer = BiPin.Expr.Modifiable.serializer[ S, Int ]
//      val imp = ExprImplicits[ S ]
//      import imp._
//
//      system.step { implicit tx =>
//         val p = ph.get
//         p.scans.add( "amp" )
//         p.scans.add( "freq" )
//         p.scans.remove( "amp" )
//         val gr = Grapheme.Modifiable[ S ]
//         p.graphemes.add( "test", gr )
//         p.graphemes.remove( "test" )
//         p.graphemes.add( "gr", gr )
//         val gr2 = Grapheme.Modifiable[ S ]
//         p.graphemes.add( "gr", gr2 )
//         gr.dispose()
//         obs.assertEquals(
//            Proc.AssociativeChange( p, added = Set( Proc.ScanKey( "amp"  )), removed = Set.empty ),
//            Proc.AssociativeChange( p, added = Set( Proc.ScanKey( "freq" )), removed = Set.empty ),
//            Proc.AssociativeChange( p, added = Set.empty, removed = Set( Proc.ScanKey( "amp" ))),
//            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "test"  )), removed = Set.empty ),
//            Proc.AssociativeChange( p, added = Set.empty, removed = Set( Proc.GraphemeKey( "test" ))),
//            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "gr" )), removed = Set.empty ),
//            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "gr" )), removed = Set( Proc.GraphemeKey( "gr" )))
//         )
//      }
//
//      def curve( amp: Expr[ S, Double ], shape: Env.ConstShape = linShape ) = Grapheme.Elem.Curve( amp -> shape )
//
//      system.step { implicit tx =>
//         val p = ph.get
//         val Some( Grapheme.Modifiable( gr )) = p.graphemes.get( "gr" )
//         val Some( scan ) = p.scans.get( "freq" )
//         gr.add( 0L, curve( 1234.0 ))                       // should be observed only directly through proc (but not scan)
//         obs.assertEquals(
//            Proc.GraphemeChange( p, Map( "gr" -> IIdxSeq( Grapheme.Update( gr, IIdxSeq( Grapheme.Value.Const( Span.from( 0L ), 1234.0 ))))))
//         )
//         obs.clear()
//         scan.source_=( Some( Scan.Link.Grapheme( gr )))    // should be observed
//         obs.assertEquals(
//            Proc.ScanChange( p, Map( "freq" -> IIdxSeq( Scan.SourceChanged( scan, Some( Scan.Link.Grapheme( gr ))))))
//         )
//         obs.clear()
//         gr.add( 2000L, curve( 5678.0 ))                    // ...
//         obs.assertEquals(
//            Proc.ScanChange( p, Map( "freq" -> IIdxSeq( Scan.SourceUpdate( scan,
//               Grapheme.Update( gr, IIdxSeq( Grapheme.Value.Segment( Span( 0L, 2000L ), (1234.0, 5678.0, linShape) ),
//                                             Grapheme.Value.Const( Span.from( 2000L ), 5678.0 ))
//               )
//            ))))
//         )
//         scan.source_=( None )                              // ...
//         val timeVar = Longs.newVar[ S ]( 3000L )
//         val ampVar  = Doubles.newVar[ S ]( 9876.0 )
//         gr.add( timeVar, curve( ampVar ))                  // should not be observed
//         scan.source_=( Some( Scan.Link.Grapheme( gr )))    // should be observed
//         timeVar.set( 4000L )                               // ...
//         ampVar.set( 5432.0 )                               // ...
//      }
//   }
//}