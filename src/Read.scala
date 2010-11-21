trait System {
   type Var[ _ ]
   type Ctx
   def read[ T ]( v: Var[ T ])( implicit c: Ctx ) : T
}

object ESystem {
   type FuckYou[ A ] = EVar[ ECtx, A ]
}
trait ESystem extends System with Cursor[ ESystem, ECtx, ESystem.FuckYou ] {
   type Var[ T ] = EVar[ ECtx, T ]
   type Ctx = ECtx
}

object KSystem {
   type FuckYou[ A ] = KVar[ KCtx, A ]
}
trait KSystem extends System {
   type Var[ T ] = KVar[ KCtx, T ]
   type Ctx = KCtx
   def in( v: Int ) : Cursor[ KSystem, KCtx, KSystem.FuckYou ]
}

trait Proc[ S <: System ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
