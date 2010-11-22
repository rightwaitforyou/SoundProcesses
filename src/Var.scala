trait EVar[ C, T ] {
   def get( implicit c: C ) : T
   def set( v: T )( implicit c: C ) : Unit
}

trait KVar[ C, T ] extends EVar[ C, T ] {
   def range( vStart: Int, vStop: Int )( implicit c: ECtx ) : Traversable[ T ]
}
