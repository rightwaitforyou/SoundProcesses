trait System {
   type Var[ _ ]
   type Ctx
}

trait ESystem extends System with Cursor[ ESystem ] {
   type Var[ T ] = EVar[ ECtx, T ]
   type Ctx = ECtx
}

trait KSystem extends System {
   type Var[ T ] = KVar[ KCtx, T ]
   type Ctx = KCtx
}

trait Proc[ S <: System ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
