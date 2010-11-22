trait System {
   type Var[ _ ]
   type Ctx <: CtxLike
   def t[ R ]( fun: Ctx => R ) : R // any system can initiate an ephemeral transaction
}

object ESystem {
   private type Var[ A ] = EVar[ ECtx, A ]
}
trait ESystem extends System
with Cursor[ ESystem, ECtx, ESystem.Var ] with CursorProvider[ ESystem ] {
   type Var[ T ] = EVar[ Ctx, T ]
   type Ctx = ECtx
}

object KSystem {
   private type Var[ A ] = KVar[ KCtx, A ]
}
trait KSystem extends System with KAccessProvider[ KSystem ] {
   type Var[ T ] = KVar[ KCtx, T ]
   type Ctx = KCtx
   def in( v: Int ) : Cursor[ KSystem, KCtx, KSystem.Var ]
}
