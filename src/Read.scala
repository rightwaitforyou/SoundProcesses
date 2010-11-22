trait System {
   type Var[ _ ]
   type Ctx
   def t[ R ]( fun: ECtx => R ) : R // any system can initiate an ephemeral transaction
}

object ESystem {
   type FuckYou[ A ] = EVar[ ECtx, A ]
}
trait ESystem extends System
with Cursor[ ESystem, ECtx, ESystem.FuckYou ] with CursorProvider[ ESystem ] {
   type Var[ T ] = EVar[ ECtx, T ]
   type Ctx = ECtx
}

object KSystem {
   type FuckYou[ A ] = KVar[ KCtx, A ]
}
trait KSystem extends System with KAccessProvider[ KSystem ] {
   type Var[ T ] = KVar[ KCtx, T ]
   type Ctx = KCtx
   def in( v: Int ) : Cursor[ KSystem, KCtx, KSystem.FuckYou ]
}

trait Proc[ S <: System ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
