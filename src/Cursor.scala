//trait Cursor[ S <: System ] {
//   val sys: S
//   def t[ R ]( fun: sys.Ctx => R ) : R
//   def read[ T ]( v: sys.Var[ T ])( implicit c: sys.Ctx ) : T
//}

trait Cursor[ S <: System, C, V[ _ ]] {
//   val sys: S
   def t[ R ]( fun: C => R ) : R
   def read[ T ]( v: V[ T ])( implicit c: C ) : T
}

trait CursorProvider[ S <: System ] {
   sys: S =>

   def provide : Cursor[ S, sys.Ctx, sys.Var ]
}

//trait Access[ S <: System ] {
//   val sys: S
//   def read[ T ]( v: sys.Var[ T ])( implicit csr: Cursor[ S ]) : T
//}