/*
 *  TxnImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import de.sciss.osc
import concurrent.stm.InTxn
import de.sciss.synth.message
import scala.concurrent.stm.{Txn => ScalaTxn}

object TxnImpl {
  //   private val errOffMsg   = osc.Message( "/error", -1 )
  //   private val errOnMsg    = osc.Message( "/error", -2 )

  var timeoutFun: () => Unit = () => ()

  private final val noBundles = Txn.Bundles(0, Vector.empty)

  //   var TIMEOUT_MILLIS = 10000L
}

sealed trait TxnImpl /* [ S <: Sys[ S ]] */ extends Txn /* Sys.Txn[ S ] */ {
  tx =>

  import TxnImpl._

  private var bundlesMap = Map.empty[Server, Txn.Bundles]

  final protected def flush(): Unit =
    bundlesMap.foreach { case (server, bundles) =>
      log("flush " + server + " -> " + bundles.payload.size + " bundles")
      NodeGraph.send(server, bundles)
    }

  protected def markBundlesDirty(): Unit

  final def addMessage(resource: Resource, m: osc.Message with message.Send, audible: Boolean, dependencies: Seq[Resource],
                       noErrors: Boolean): Unit = {

    //      val rsrc = system.resources

    val server        = resource.server
    val rsrcStampOld  = resource.timeStamp(tx)
    require(rsrcStampOld >= 0, "Already disposed : " + resource)

    implicit val itx  = peer
    val txnCnt        = NodeGraph.messageTimeStamp(server)(tx)
    val txnStopCnt    = txnCnt.get
    val bOld          = bundlesMap.getOrElse(server, noBundles)
    val txnStartCnt   = txnStopCnt - bOld.payload.size

    // calculate the maximum time stamp from the dependencies. this includes
    // the resource as its own dependency (since we should send out messages
    // in monotonic order)
    var depStampMax = math.max(txnStartCnt << 1, rsrcStampOld)
    dependencies.foreach { dep =>
      val depStamp = dep.timeStamp(tx)
      require(depStamp >= 0, "Dependency already disposed : " + dep)
      if (depStamp > depStampMax) depStampMax = depStamp
      //         dep.addDependent( resource )( tx )  // validates dep's server
    }

    //      val dAsync     = (dTsMax & 1) == 1
    val msgAsync = !m.isSynchronous

    // if the message is asynchronous, it suffices to ensure that the time stamp's async bit is set.
    // otherwise clear the async flag (& ~1), and if the maximum dependency is async, increase the time stamp
    // (from bit 1, i.e. `+ 2`); this second case is efficiently produced through 'rounding up' (`(_ + 1) & ~1`).
    val rsrcStampNew = if (msgAsync) depStampMax | 1 else (depStampMax + 1) & ~1

    log(s"addMessage($resource, $m) -> stamp = $rsrcStampNew")
    if (rsrcStampNew != rsrcStampOld) resource.timeStamp_=(rsrcStampNew)(tx)

    val bNew = if (bOld.payload.isEmpty) {
      markBundlesDirty()
      //         log( "registering after commit handler" )
      //         afterCommit( flush() )
      val txnStartCntNew = rsrcStampNew >> 1
      assert(txnStartCntNew == txnStartCnt)
      txnCnt += 1
      Txn.Bundles(txnStartCntNew, Vector(Vector(m)))

    } else {
      val cntOld = bOld.firstCnt
      val rsrcCnt = rsrcStampNew >> 1
      val payOld = bOld.payload
      val szOld = payOld.size
      //         if( rsrcCnt == cntOld - 1 ) {   // prepend to front
      //            val payNew = Vec( message ) +: payOld
      //            bOld.copy( firstCnt = rsrcCnt, payload = payNew )
      //
      //         } else
      if (rsrcCnt == cntOld + szOld) {
        // append to back
        val payNew = payOld :+ Vector(m)
        txnCnt += 1
        bOld.copy(payload = payNew)

      } else {
        // we don't need the assertion, since we are going to call payload.apply which would
        // through an out of bounds exception if the assertion wouldn't hold
        //            assert( idxNew >= idxOld && idxNew < idxOld + szOld )
        val payIdx = rsrcCnt - cntOld
        val payNew = payOld.updated(payIdx, payOld(payIdx) :+ m)
        bOld.copy(payload = payNew)
      }
    }

    bundlesMap += server -> bNew
  }

  //      // clumping
  //      var clumpIdx   = 0
  //      var clumpMap   = Map.empty[ Entry, Int ]
  //      var clumps     = IntMap.empty[ List[ Entry ]]
  //      val audibleIdx = Int.MaxValue
  //      topo.vertices.foreach( targetEntry => {
  //         if( targetEntry.audible ) {
  //            clumps += audibleIdx -> (targetEntry :: clumps.getOrElse( audibleIdx, Nil ))
  //            clumpMap += targetEntry -> audibleIdx
  //         } else {
  //            val depIdx = clumpEdges.get( targetEntry ).map( set => {
  //               set.map( clumpMap.getOrElse( _, sys.error( "Unsatisfied dependancy " + targetEntry ))).max
  //            }).getOrElse( -1 )
  //            if( depIdx > clumpIdx ) sys.error( "Unsatisfied dependancy " + targetEntry )
  //            if( depIdx == clumpIdx ) clumpIdx += 1
  //            clumps += clumpIdx -> (targetEntry :: clumps.getOrElse( clumpIdx, Nil ))
  //            clumpMap += targetEntry -> clumpIdx
  //         }
  //      })
}

trait TxnFullImpl[S <: Sys[S]] extends TxnImpl with Sys.Txn[S] {
  final protected def markBundlesDirty(): Unit = {
    log("registering after commit handler")
    afterCommit(flush())
  }
}

final class TxnPlainImpl(val peer: InTxn) extends TxnImpl {
  override def toString = "proc.Txn<plain>@" + hashCode().toHexString

  protected def markBundlesDirty(): Unit = {
    log("registering after commit handler")
    ScalaTxn.afterCommit(_ => flush())(peer)
  }
}