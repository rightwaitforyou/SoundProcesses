///*
// *  ProcGroup.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//
//import de.sciss.lucre.{bitemp, event => evt}
//import bitemp.BiGroup
//import evt.{EventLike, Sys}
//import de.sciss.serial.{Serializer, DataInput}
//import de.sciss.synth.proc
//
//// ---- Elem ----
//
//object ProcGroupElem {
//  def apply[S <: Sys[S]](peer: ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] =
//    proc.impl.ElemImpl.ProcGroup(peer)
//
//  object Obj {
//    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ProcGroupElem]] =
//      if (obj.elem.isInstanceOf[ProcGroupElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ProcGroupElem]])
//      else None
//  }
//
//  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroupElem[S]] =
//    proc.impl.ElemImpl.ProcGroup.serializer[S]
//}
//trait ProcGroupElem[S <: Sys[S]] extends proc.Elem[S] {
//  type Peer       = ProcGroup[S]
//  type PeerUpdate = ProcGroup.Update[S]
//}
