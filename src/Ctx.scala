trait Txn

trait TxnHolder { def txn: Txn }

trait ECtx extends TxnHolder {
   def v[ T ]( init: T ) : EVar[ ECtx, T ]
}

trait KCtx extends TxnHolder {
   def v[ T ]( init: T ) : KVar[ KCtx, T ]
   def eph : ECtx
}
