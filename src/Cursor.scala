trait Cursor[ S <: System, C, V[ _ ]] {
   def t[ R ]( fun: C => R ) : R
   def read[ T ]( vr: V[ T ])( implicit c: C ) : T
   def write[ T ]( vr: V[ T ], v: T )( implicit c: C ) : Unit
}

trait KAccess[ S <: System, C, V[ _ ]] {
   def range[ T ]( vr: V[ T ], start: Int, stop: Int )( implicit c: C ) : Traversable[ T ]
}

trait CursorProvider[ S <: System ] {
   sys: S =>
   def cursor : Cursor[ S, sys.Ctx, sys.Var ]
}

trait KAccessProvider[ S <: System ] {
   sys: S =>
   def kaccess : KAccess[ S, ECtx, sys.Var ]
}
