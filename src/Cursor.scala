trait Cursor[ S <: System ] {
   val sys: S
   def t[ R ]( fun: sys.Ctx => R ) : R
}