/*
 *  Implicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.data
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{String => StringEx, Boolean => BooleanEx}

object Implicits {
  implicit class ExprAsVar[A, S <: Sys[S]](val `this`: Expr[S, A]) extends AnyVal { me =>
    import me.{`this` => ex}

    /** Resolves the expression as a variable. If the expression is not a variable,
      * throws an exception.
      */
    def asVar: Expr.Var[S, A] = Expr.Var.unapply(ex).getOrElse(sys.error(s"Not a variable: $ex"))
  }

  implicit class SecFrames(val `this`: Double) extends AnyVal { me =>
    import me.{`this` => d}

    /** Interprets the number as a duration in seconds, and converts it to sample frames,
      * based on the standard `Timeline` sample-rate.
      */
    def secframes: Long = (d * Timeline.SampleRate + 0.5).toLong
  }

  implicit class ScanOps[S <: Sys[S]](val `this`: Scan[S]) extends AnyVal {
    /** Connects this scan as source to that scan as sink. */
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.add(Scan.Link.Scan(that))

    /** Disconnects this scan as source from that scan as sink. */
    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.remove(Scan.Link.Scan(that))
  }

  // Scala 2.10.4 has a compiler bug that prevents putting this
  // code inside a value class
  private[this] def getScanLinks[S <: Sys[S]](in: data.Iterator[S#Tx, Scan.Link[S]])
                                             (implicit tx: S#Tx): Set[Scan.Link.Scan[S]] =
    in.collect {
      case l @ Scan.Link.Scan(_) => l
    } .toSet

  implicit class ProcPairOps[S <: Sys[S]](val `this`: (Proc.Obj[S], Proc.Obj[S])) extends AnyVal { me =>
    import me.{`this` => pair}

    private def getLayerIn(implicit tx: S#Tx): Scan[S] = {
      val inObj = pair._1
      inObj.elem.peer.inputs.get("in").getOrElse(sys.error(s"Proc ${inObj.name} does not have scan 'in'"))
    }

    private def getLayerOut(implicit tx: S#Tx): Scan[S] = {
      val outObj = pair._2
      outObj.elem.peer.outputs.get("out").getOrElse(sys.error(s"Proc ${outObj.name} does not have scan 'out'"))
    }

    /** Removes the signal chain signified by the input proc pair from its predecessors and successors.
      * It does so by removing the sources of the `_1` scan named `"in"` and the sinks of the
      * `_2` scan named `"out"`. It re-connects the predecessors and successors thus found.
      */
    def unlink()(implicit tx: S#Tx): Unit = {
      val layerIn   = getLayerIn
      val layerOut  = getLayerOut

      val oldLayerIn  = getScanLinks(layerIn .iterator)
      val oldLayerOut = getScanLinks(layerOut.iterator)

      // disconnect old inputs
      oldLayerIn .foreach(layerIn .remove)
      // disconnect old outputs
      oldLayerOut.foreach(layerOut.remove)
      // connect old layer inputs to old layer outputs
      oldLayerIn.foreach { in =>
        oldLayerOut.foreach { out =>
          in.peer.add(out)
        }
      }
    }

    def linkAfter(out: Proc.Obj[S])(implicit tx: S#Tx): Unit = {
      val target = out.elem.peer.outputs.get("out").getOrElse(sys.error(s"Successor ${out.name} does not have scan 'out'"))
      link1(target, isAfter = true)
    }

    def linkBefore(in: Proc.Obj[S])(implicit tx: S#Tx): Unit = {
      val target = in.elem.peer.inputs.get("in").getOrElse(sys.error(s"Predecessor ${in.name} does not have scan 'in'"))
      link1(target, isAfter = false)
    }

    private def link1(target: Scan[S], isAfter: Boolean)(implicit tx: S#Tx): Unit = {
      val layerIn  = getLayerIn
      val layerOut = getLayerOut

      val targetIt  = target.iterator
      val oldTargetLinks = getScanLinks(targetIt)
      val layerLink = Scan.Link.Scan(if (isAfter) layerIn else layerOut)
      // only act if we're not there
      if (!oldTargetLinks.contains(layerLink)) {
        unlink()
        if (isAfter) {
          // disconnect old diff outputs
          oldTargetLinks.foreach(target.remove)
          // connect old diff inputs as new layer inputs
          oldTargetLinks.foreach(layerOut.add)
          // connect layer output to diff input
          target.add(layerLink)
        } else {
          // disconnect old diff inputs
          oldTargetLinks.foreach(target.remove)
          // connect old diff inputs as new layer inputs
          oldTargetLinks.foreach(layerIn.add  )
          // connect layer output to diff input
          target.add(layerLink)
        }
      }
    }
  }

  implicit class FolderOps[S <: Sys[S]](val `this`: Folder[S]) extends AnyVal { me =>
    import me.{`this` => folder}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = folder.iterator.filter { obj =>
        obj.name == child
      } .toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in $folder")
      res
    }
  }

  implicit class EnsembleOps[S <: Sys[S]](val `this`: Ensemble.Obj[S]) extends AnyVal { me =>
    import me.{`this` => ensemble}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = ensemble.elem.peer.folder.iterator.filter { obj =>
        obj.name == child
      }.toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in ${ensemble.attr.name}")
      res
    }

    def play()(implicit tx: S#Tx): Unit = play1(value = true )
    def stop()(implicit tx: S#Tx): Unit = play1(value = false)

    private def play1(value: Boolean)(implicit tx: S#Tx): Unit = {
      val vr = ensemble.elem.peer.playing.asVar
      val imp = ExprImplicits[S]
      import imp._
      vr() = value
    }

    def isPlaying(implicit tx: S#Tx): Boolean = ensemble.elem.peer.playing.value
  }

  implicit final class ObjOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal { me =>
    import me.{`this` => obj}

    /** Short cut for accessing the attribute `"name"`.
      * If their is no value found, a dummy string `"&lt;unnamed&gt;"` is returned.
      */
    def name(implicit tx: S#Tx): String =
      obj.attr[StringElem](ObjKeys.attrName).fold("<unnamed>")(_.value)

    /** Short cut for updating the attribute `"name"`. */
    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      val valueC = StringEx.newConst[S](value)
      obj.attr[StringElem](ObjKeys.attrName) match {
        case Some(Expr.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = StringEx.newVar(valueC)
          obj.attr.put(ObjKeys.attrName, Obj(StringElem[S](valueVr)))
      }
    }

    /** Short cut for accessing the attribute `"mute"`. */
    def muted(implicit tx: S#Tx): Boolean =
      obj.attr[BooleanElem](ObjKeys.attrMute).fold(false)(_.value)

    /** Short cut for updating the attribute `"mute"`. */
    def muted_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      val valueC = BooleanEx.newConst[S](value)
      obj.attr[BooleanElem](ObjKeys.attrMute) match {
        case Some(Expr.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = BooleanEx.newVar(valueC)
          obj.attr.put(ObjKeys.attrMute, Obj(BooleanElem[S](valueVr)))
      }
    }
  }
}