//package de.sciss.synth.proc.dsl
//
//trait Universe {
//  def self: Action
//
//  trait Obj {
//    def attr: AttrMap
//  }
//
//  trait ObjType[A]
//
//  object Action extends ObjType[Action]
//  trait Action extends Obj {
//
//  }
//
//  trait IntObj extends Obj {
//
//  }
//
//  trait AttrMap {
//    def apply[A: ObjType](key: String): Option[A]
//    def get(key: String): Option[Obj]
//  }
//}
