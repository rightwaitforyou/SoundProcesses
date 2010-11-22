object Test {
   // possible to access a particular system
   // with its own context
   def test( p: Proc[ ESystem ])( implicit c: ECtx ) {
      p.switch.get
   }

   // possible to access a particular system
   // with a more specific context
   def test2( p: Proc[ ESystem ])( implicit c: KCtx ) {
      p.switch.get( c.eph )
   }

   // possible to access a system which has a cursor provider.
   // possible to degenerate a ktemporal context to ephemeral
   def test5( p: Proc[ KSystem with CursorProvider[ KSystem ]]) {
      val res = p.sys.cursor.t { implicit c =>
         p.switch.range( 1, 2 )( c.eph )
      }
   }

   // possible to initiate an ephemeral transaction
   def test6( p: Proc[ ESystem ]) {
      val res = p.sys.t { implicit c => p.switch.get }
   }

   // possible to get a KCtx cursor from KSystem
   def test7( p: Proc[ KSystem ]) {
      val res = p.sys.in( 0 ).t { implicit c => p.switch.get }
   }

   // possible to use the cursor of any system which comes
   // an appropriate cursor-provider
   def test9[ S <: System with CursorProvider[ S ]]( p: Proc[ S ]) {
      val csr = p.sys.cursor 
      val res = csr.t { implicit c =>
         csr.read( p.switch )
      }
   }

   // possible to access any system ktemporally which comes
   // an appropriate kaccess-provider
   def test10[ S <: System with KAccessProvider[ S ]]( p: Proc[ S ]) {
      val kacc = p.sys.kaccess
      val res = p.sys.t { implicit c =>
         kacc.range( p.switch, 1, 2 )( c.eph )
      }
   }

   // initiate a transaction and create a proc
   def test11[ S <: System with CursorProvider[ S ]]( sys: S ) {
      val csr = sys.cursor
      val res = csr.tp { implicit cp =>
         ProcFactory.apply[ S with CtxProvider[ S ]]( "Testin One Two" )( cp )
      }
   }
}