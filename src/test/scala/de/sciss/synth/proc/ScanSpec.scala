package de.sciss
package synth
package proc

import expr.{Doubles, Longs}
import lucre.bitemp
import collection.immutable.{IndexedSeq => IIdxSeq}
import lucre.expr.Expr
import bitemp.Span

/**
 * To run only this suite:
 *
 * test-only de.sciss.synth.proc.ScanSpec
 */
class ScanSpec extends ConfluentEventSpec {
   import imp._

   "Proc" should "notify observers about all relevant events" in { system =>
      val obs  = new Observation[ S ]
      val ph   = system.step { implicit tx =>
         val p = Proc[ S ]
         p.changed.reactTx( obs.register )
         val res = tx.newHandle( p )( Proc.serializer[ S ])
         obs.assertEmpty()
         res
      }

      system.step { implicit tx =>
         val p = ph.get
         p.scans.add( "amp" )
         p.scans.add( "freq" )
         p.scans.remove( "amp" )
         val gr = Grapheme.Modifiable[ S ]
         p.graphemes.add( "test", gr )
         p.graphemes.remove( "test" )
         p.graphemes.add( "gr", gr )
         val gr2 = Grapheme.Modifiable[ S ]
         p.graphemes.add( "gr", gr2 )
         gr.dispose()
         obs.assertEquals(
            Proc.AssociativeChange( p, added = Set( Proc.ScanKey( "amp"  )), removed = Set.empty ),
            Proc.AssociativeChange( p, added = Set( Proc.ScanKey( "freq" )), removed = Set.empty ),
            Proc.AssociativeChange( p, added = Set.empty, removed = Set( Proc.ScanKey( "amp" ))),
            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "test"  )), removed = Set.empty ),
            Proc.AssociativeChange( p, added = Set.empty, removed = Set( Proc.GraphemeKey( "test" ))),
            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "gr" )), removed = Set.empty ),
            Proc.AssociativeChange( p, added = Set( Proc.GraphemeKey( "gr" )), removed = Set( Proc.GraphemeKey( "gr" )))
         )
      }

      def curve( amp: Expr[ S, Double ], shape: Env.ConstShape = linShape )( implicit tx: S#Tx ) =
         Grapheme.Elem.Curve( amp -> shape )

      system.step { implicit tx =>
         val p = ph.get
         val Some( Grapheme.Modifiable( gr )) = p.graphemes.get( "gr" )
         val Some( scan ) = p.scans.get( "freq" )

         gr.add( 0L, curve( 1234.0 ))                       // should be observed only directly through proc (but not scan)
         obs.assertEquals(
            Proc.GraphemeChange( p, Map(
               "gr" -> Grapheme.Update( gr, IIdxSeq( Grapheme.Segment.Const( Span.from( 0L ), IIdxSeq( 1234.0 ))))
            ))
         )
         obs.clear()

         scan.source_=( Some( Scan.Link.Grapheme( gr )))    // should be observed
         obs.assertEquals(
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceChanged( scan, Some( Scan.Link.Grapheme( gr )))))
         )
         obs.clear()

         gr.add( 2000L, curve( 5678.0 ))                    // ...
         obs.assertEquals(
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceUpdate( scan,
               Grapheme.Update( gr, IIdxSeq( Grapheme.Segment.Curve( Span( 0L, 2000L ), IIdxSeq( (1234.0, 5678.0, linShape) )),
                                             Grapheme.Segment.Const( Span.from( 2000L ), IIdxSeq( 5678.0 )))
               )
            )))
         )
         obs.clear()

         scan.source_=( None )                              // ...
         val timeVar = Longs.newVar[   S ]( 3000L )
         val ampVar  = Doubles.newVar[ S ]( 9876.0 )
         gr.add( timeVar, curve( ampVar ))                  // should not be observed
         scan.source_=( Some( Scan.Link.Grapheme( gr )))    // should be observed
         obs.assertEquals(
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceChanged( scan, None ))),
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceChanged( scan, Some( Scan.Link.Grapheme( gr )))))
         )
         obs.clear()

//lucre.event.showLog = true
         timeVar.set( 4000L )                               // ...
//lucre.event.showLog = false
         obs.assertEquals(
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceUpdate( scan,
               Grapheme.Update( gr, IIdxSeq( Grapheme.Segment.Curve( Span( 2000L, 4000L ), IIdxSeq( (5678.0, 9876.0, linShape) )),
                                             Grapheme.Segment.Const( Span.from( 4000L ), IIdxSeq( 9876.0 )))
               )
            )))
         )
         obs.clear()

         ampVar.set( 5432.0 )                             // ...
         obs.assertEquals(
            Proc.ScanChange( p, Map( "freq" -> Scan.SourceUpdate( scan,
               Grapheme.Update( gr, IIdxSeq( Grapheme.Segment.Curve( Span( 2000L, 4000L ), IIdxSeq( (5678.0, 5432.0, linShape) )),
                                             Grapheme.Segment.Const( Span.from( 4000L ), IIdxSeq( 5432.0 )))
               )
            )))
         )
         obs.clear()
      }
   }
}