trait System[ C <: CtxLike, V[ _ ] <: EVar[ C, _ ]] {
//   type Var[ _ ]
//   type Ctx <: CtxLike
   def t[ R ]( fun: C => R ) : R // any system can initiate an ephemeral transaction
   def v[ T ]( init: T )( implicit c: C ) : V[ T ]
}

object ESystem {
   type Var[ A ] = EVar[ ECtx, A ]
}
trait ESystem extends System[ ECtx, ESystem.Var ]
/* with Cursor[ ESystem, ECtx, ESystem.Var ] with CursorProvider[ ESystem ] */ {
//   type Var[ T ] = EVar[ Ctx, T ]
//   type Ctx = ECtx
}

object KSystem {
   type Var[ A ] = KVar[ KCtx, A ]
}
trait KSystem extends System[ KCtx, KSystem.Var ] /* with KAccessProvider[ KSystem ] */ {
//   type Var[ T ] = KVar[ KCtx, T ]
//   type Ctx = KCtx
//   def in( v: Int ) : Cursor[ KSystem, KCtx, KSystem.Var ]
}
