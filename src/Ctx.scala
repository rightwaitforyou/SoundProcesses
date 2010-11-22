trait Txn

trait CtxLike[ S <: System ] {
   val sys : S 
   def txn: Txn
   def eph : ECtx
}

trait ECtx extends CtxLike[ ESystem ] {
   def v[ T ]( init: T ) : EVar[ ECtx, T ]
}

trait KCtx extends CtxLike[ KSystem ] {
   def v[ T ]( init: T ) : KVar[ KCtx, T ]
//   def eph : ECtx
}
