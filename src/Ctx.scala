trait Listener[ -C, -T ] {
   def updated( v: T )( implicit c: C ) : Unit
}

trait EVar[ C, T ] {
   def get( implicit c: C ) : T
   def set( v: T )( implicit c: C ) : Unit
   def addListener( l: Listener[ C, T ])( implicit c: ECtx ) : Unit
   def removeListener( l: Listener[ C, T ])( implicit c: ECtx ) : Unit
}

//trait EVarX[ T ] extends EVar[ ECtx, T ]
//trait KVarX[ T ] extends KVar[ KCtx, T ]

trait KVar[ C, T ] extends EVar[ C, T ] {
   def range( vStart: Int, vStop: Int )( implicit c: ECtx ) : Traversable[ T ]
}

trait ECtx {
   def v[ T ]( init: T ) : EVar[ ECtx, T ]
}

trait KCtx /* extends ECtx */ {
//   def kv[ T ]( init: T ) : KVarX[ T ] // KVar[ KCtx, T ]
   def v[ T ]( init: T ) : KVar[ KCtx, T ]
   def eph : ECtx
}
