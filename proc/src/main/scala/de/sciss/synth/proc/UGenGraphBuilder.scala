/*
 *  UGenGraphBuilder.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc

import impl.{UGenGraphBuilderImpl => Impl}
import util.control.ControlThrowable
import de.sciss.lucre.synth.Sys

private[proc] object UGenGraphBuilder {
  def outsideOfContext() = sys.error("Expansion out of context")

  /** An exception thrown when during incremental build an input is required for which the underlying source
    * cannot yet be determined.
    *
    * This can be a case class because it is used only within the same transaction,
    * and thereby the `timed` argument does not become stale.
    *
    * @param scan    the scan which is the ''source'' of the required input
    */
  final case class MissingIn[S <: Sys[S]](scan: Scan[S]) extends ControlThrowable

  /** '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
    * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
    * be stored in a `TxnLocal`, but not a full STM ref.
    */
  def apply[S <: Sys[S]](aural: AuralPresentation.Running[S], timed: TimedProc[S], time: Long)
                        (implicit tx: S#Tx): UGenGraphBuilder[S] =
    Impl(aural, timed, time)

  case class ScanIn(numChannels: Int, fixed: Boolean)
}
private[proc] trait UGenGraphBuilder[S <: Sys[S]] extends UGenGraph.Builder {
  import UGenGraphBuilder._

  /** This method should only be invoked by the `graph.scan.Elem` instances. It requests a scan input, and
    * the method returns the corresponding number of channels, or throws a `MissingIn` exception which
    * is then caught by the main builder body.
    *
    * @param  key           the scan input key
    * @param  numChannels   a given number of channels (if `>= 0`) or `-1` in which case the number of channels
    *                       is determined using the scan.
    */
  def addScanIn(key: String, numChannels: Int): Int

  /** This method should only be invoked by the `graph.attribute.In` instances. It registers a control input. */
  def addAttributeIn(key: String): Int

  /** This method should only be invoked by the `graph.stream.X` instances. It registers a control input
    * for a streaming buffer.
    */
  def addStreamIn(key: String): Int

  /** This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
    * with the number of channels written to it.
    */
  def addScanOut(key: String, numChannels: Int): Unit

  /** Current set of used inputs (scan keys to number of channels).
    * This is guaranteed to only grow during incremental building, never shrink.
    */
  def scanIns: Map[String, ScanIn]

  /** Current set of used outputs (scan keys to number of channels).
    * This is guaranteed to only grow during incremental building, never shrink.
    */
  def scanOuts: Map[String, Int]

  /** Returns the attribute keys for scalar controls as seen during expansion of the synth graph. */
  def attributeIns: Set[String]

  /** Returns the attribute keys for streaming buffers as seen during expansion of the synth graph. */
  def streamIns: Set[String]

  /** Current set of missing scan inputs. This may shrink during incremental build, and will be empty when
    * `tryBuild` returns `true`.
    */
  def missingIns: Set[MissingIn[S]]

  /** Determines whether the graph was fully build (`true`) or is incomplete (`false`) due to
    * missing inputs. Once this method returns `true`, it is safe to call `finish`.
    */
  def isComplete: Boolean

  /** The process which is building this graph. */
  def timed: TimedProc[S]

  /** The transport time at which this graph is built. */
  def time: Long

  //  /** The offset from which this proc if played. */
  //  def offset: Long
  //
  //  /** The total number of frames. */
  //  def numFrames: Long

  def tx: S#Tx

  /** Builds or continues to build the ugen graph. Since the builder is mutable, `tryBuild` should be called
    * repeatably on the same object as long as a `false` result is obtained, until either the transaction is aborted,
    * or a `true` result is obtained.
    *
    * @return  the status of completion after the iteration, i.e. `true` if the graph has no more missing elements
    *          and can be completed by calling `finish`, otherwise `false`
    */
  def tryBuild(): Boolean // UGenGraphBuilder.BuildResult[ S ]

  /** Finishes the build process and returns the ugen graph. If the builder is not complete
    * (`isComplete` returns `false`) this throws a runtime exception.
    */
  def finish: UGenGraph
}