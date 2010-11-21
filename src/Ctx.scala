trait Listener[ -C, -T ] {
   def updated( v: T )( implicit c: C ) : Unit
}

trait EVar[ C, T ] {
   def get( implicit c: C ) : T
   def set( v: T )( implicit c: C ) : Unit
//   def addListener[    D <: ECtx ]( l: Listener[ C, T ])( implicit c: D ) : Unit
//   def removeListener[ D <: ECtx ]( l: Listener[ C, T ])( implicit c: D ) : Unit
}

trait EVarX[ T ] extends EVar[ ECtx, T ]
trait KVarX[ T ] extends KVar[ KCtx, T ]

trait KVar[ C, T ] extends EVar[ C, T ] {
   def range( vStart: Int, vStop: Int )( implicit c: C ) : Traversable[ T ]
}

trait ECtx {
   def v[ T ]( init: T ) : EVarX[ T ] // EVar[ ECtx, T ]
}

trait KCtx extends ECtx {
   def kv[ T ]( init: T ) : KVarX[ T ] // KVar[ KCtx, T ]
   def eph : ECtx
}
