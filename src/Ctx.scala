trait Txn

trait CtxLike {
   def txn: Txn
   def eph : ECtx
}

trait ECtx extends CtxLike {
   def v[ T ]( init: T ) : EVar[ ECtx, T ]
}

trait KCtx extends CtxLike {
   def v[ T ]( init: T ) : KVar[ KCtx, T ]
//   def eph : ECtx
}
