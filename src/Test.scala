trait Schoko {
   type V
}

object Test {
   def test( p: Proc[ ESystem ])( implicit c: ECtx ) {
      p.switch.get
   }

   def test2( p: Proc[ ESystem ])( implicit c: KCtx ) {
      p.switch.get
   }

   def test3[ S <: ESystem ]( p: Proc[ S ])( implicit c: KCtx ) {
      p.switch.get
   }

//   def test4( p: Proc[ KSystem ])( implicit c: KCtx ) {
//      test3( p )( c )
//   }
}