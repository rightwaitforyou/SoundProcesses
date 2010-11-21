trait System {
   type Var[ _ ]
   type Ctx
//   def read[ T ]( v: Var[ T ])( implicit c: Ctx ) : T
   def t[ R ]( fun: ECtx => R ) : R
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

//trait PSystem extends System with PAccessProvider[ PSystem ] {
//   type Var[ T ] = PVar[ PCtx, T ]
//   type Ctx = KCtx
//   def in( v: Int ) : Cursor[ KSystem, KCtx, KSystem.FuckYou ]
//}
//
//trait BSystem extends System {
//   def kaccess( from: Double, to: Double ) : KAccessProvider[ KSystem ]
//}

trait Proc[ S <: System ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
