trait Schoko {
   type V
}

object Test {
   def test( p: Proc[ ESystem ])( implicit c: ECtx ) {
      p.switch.get
   }

   def test2( p: Proc[ ESystem ])( implicit c: KCtx ) {
      p.switch.get( c.eph )
   }

   def test3[ S <: ESystem ]( p: Proc[ S ])( implicit c: KCtx ) {
      p.switch.get( c.eph )
   }

   def test4[ S <: KSystem ]( p: Proc[ S ])( implicit c: KCtx ) {
      p.switch.get
   }

   def test5[ S <: KSystem ]( p: Proc[ S ], csr: Cursor[ S ]) {
      val res = csr.t { implicit c =>
//         p.switch.get
         p.switch.range( 1, 2 )( c.eph )
      }
   }

   def test6[ S <: ESystem ]( p: Proc[ S ]) {
      val res = p.sys.t { implicit c => p.switch.get }
   }


//   def test6[ S <: KSystem ]( p: Proc[ S ], csr: Cursor[ S ]) {
//      p.sys.toString {
//         p.switch.range( 1, 2 )
//      }
//   }
   
//   def test4( p: Proc[ KSystem ])( implicit c: KCtx ) {
//      test3( p )( c )
//   }
}