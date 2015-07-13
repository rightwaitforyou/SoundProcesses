package de.sciss
package synth
package proc

import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx, BiGroup}
import de.sciss.lucre.expr.{Boolean => BooleanEx}
import de.sciss.lucre.stm.Disposable
import de.sciss.span.Span

import scala.collection.immutable.{IndexedSeq => Vec}

/*
  To run only this suite:

  test-only de.sciss.synth.proc.Issue15

  */
class Issue15 extends ConfluentEventSpec {
  "AttrMap" should "dispatch events after repeated listener (un)registration" in { system =>
    val obs = new Observation

    de.sciss.lucre.event.showLog = true

    // ---- we create the "workspace" ----
    val (fH, pObjH, tlH, timedIDH, spanH) = system.step { implicit tx =>
      val p         = Proc[S]
      val pObj      = Obj(Proc.Elem(p))
      pObj.attr // initialize for debugger
      val tl        = Timeline[S]
      val span      = SpanLikeEx.newVar(SpanLikeEx.newConst[S](Span(0L, 10000L)))
      val timed     = tl.add(span, pObj)
      val _pObjH    = tx.newHandle(pObj)
      val _tlH      = tx.newHandle(tl)
      import de.sciss.lucre.synth.expr.IdentifierSerializer
      val _timedIDH = tx.newHandle(timed.id)(IdentifierSerializer[S])
      import SpanLikeEx.varSerializer
      val _spanH    = tx.newHandle(span)
      val f         = Folder[S]
      val tlObj     = Obj(Timeline.Elem(tl))
      f.addLast(tlObj)
      implicit val fSer = Folder.serializer[S]
      val _fH       = tx.newHandle(f)
      (_fH, _pObjH, _tlH, _timedIDH, _spanH)
    }

    def printChildren(header: String)(implicit tx: S#Tx): Unit = {
      val tl = tlH()
      val ch = de.sciss.lucre.event.Peek.targets(tl)
      println(s"\n---- $header ----")
      ch.foreach(println)
      println()
    }

    // ---- we "open the folder" view; this is crucial for the bug to appear ----
    system.step { implicit tx =>
      val f = fH()
      f.changed.react { implicit tx => upd =>
        // nada
      }
    }

    def timelineObservation(): Disposable[S#Tx] = system.step { implicit tx =>
      val tl   = tlH()
      val _obs = tl.changed.react(obs.register)
      obs.assertEmpty()
      printChildren(s"AFTER TL OBSERVATION (${_obs})")
      _obs
    }

    // ---- we "open the timeline" view ----
    val obs1 = timelineObservation()

    def muteObservation(): Unit = {
      val muteH = system.step { implicit tx =>
        printChildren("BEFORE FIRST MUTATION")

        val pObj    = pObjH()
        val tl      = tlH()
        val muteObj = Obj(BooleanElem(BooleanEx.newVar(BooleanEx.newConst[S](true))))
        val timed   = BiGroup.TimedElem(timedIDH(), spanH(), pObj)
        pObj.attr.put(ObjKeys.attrMute, muteObj)
        obs.assertEquals(BiGroup.Update(tl, Vec(
          BiGroup.ElementMutated(timed, Obj.UpdateT(pObj, Vec(
            Obj.AttrAdded(ObjKeys.attrMute, muteObj)
          )))
        )))
        tx.newHandle(muteObj)
      }

      system.step { implicit tx =>
        printChildren("BEFORE SECOND MUTATION")

        val pObj    = pObjH()
        val tl      = tlH()
        val muteObj = muteH()
        val timed   = BiGroup.TimedElem(timedIDH(), spanH(), pObj)
        pObj.attr.remove(ObjKeys.attrMute)
        obs.assertEquals(BiGroup.Update(tl, Vec(
          BiGroup.ElementMutated(timed, Obj.UpdateT(pObj, Vec(
            Obj.AttrRemoved(ObjKeys.attrMute, muteObj)
          )))
        )))

        printChildren("AFTER SECOND MUTATION")
      }
    }

    // ---- we "mute and un-mute" ----
    muteObservation()

    // ---- we "close" the timeline view; this produces the illegal state somehow ----
    system.step { implicit tx =>
      printChildren("BEFORE DISPOSAL")
      obs1.dispose()
      printChildren("AFTER DISPOSAL")
    }

    // ---- we "open" the view again ----
    /* val obs2 = */ timelineObservation()

    // ---- we "mute and un-mute"; this is were the bug occurs ----
    muteObservation()
  }
}