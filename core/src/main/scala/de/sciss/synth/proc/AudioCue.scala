/*
 *  AudioCue.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.file.File
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.expr.{Type, LongObj, DoubleObj, Expr => _Expr}
import de.sciss.lucre
import de.sciss.lucre.{stm, expr}
import de.sciss.lucre.stm.{Elem, Copy, Sys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, ImmutableSerializer, DataOutput}
import de.sciss.synth.io.AudioFileSpec

object AudioCue {
  final val typeID = 13

  def init(): Unit = Obj.init()

  private final val COOKIE = 0x4143 // 'AC'

  implicit object serializer extends ImmutableSerializer[AudioCue] {
    def write(v: AudioCue, out: DataOutput): Unit = {
      import v._
      // out.writeByte(audioCookie)
      out.writeShort(AudioCue.COOKIE)
      out.writeUTF(artifact.getPath) // artifact.write(out)
      AudioFileSpec.Serializer.write(spec, out)
      out.writeLong(offset)
      out.writeDouble(gain)
    }

    def read(in: DataInput): AudioCue = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val artifact  = new File(in.readUTF())
      val spec      = AudioFileSpec.Serializer.read(in)
      val offset    = in.readLong()
      val gain      = in.readDouble()
      AudioCue(artifact = artifact, spec = spec, offset = offset, gain = gain)
    }
  }

  object Obj extends expr.impl.ExprTypeImpl[AudioCue, AudioCue.Obj] {
    def typeID = AudioCue.typeID

    import AudioCue.{Obj => Repr}

    private[this] lazy val _init: Unit = registerExtension(Apply)

    override def init(): Unit = {
      super.init()
      _init
    }

    protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private[this] final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
      extends ConstImpl[S] with Repr[S]

    private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
      extends VarImpl[S] with Repr[S]

    def valueSerializer: ImmutableSerializer[AudioCue] = AudioCue.serializer

    def apply[S <: Sys[S]](artifact: Artifact[S], spec: AudioFileSpec, offset: LongObj[S], gain: DoubleObj[S])
                          (implicit tx: S#Tx): Obj[S] = {
      val targets = Targets[S]
      new Apply(targets, artifact = artifact, spec = spec, offset = offset, gain = gain).connect()
    }

    def unapply[S <: Sys[S]](expr: Obj[S]): Option[(Artifact[S], AudioFileSpec, LongObj[S], DoubleObj[S])] =
      expr match {
        case impl: Apply[S] => Some((impl.artifact, impl.spec, impl.offset, impl.gain))
        case _ => None
      }

    private object Apply extends Type.Extension1[Obj] {
      final val opID = 0

      def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): Obj[S] = {
        val artifact  = Artifact .read(in, access)
        val spec      = AudioFileSpec.Serializer.read(in)
        val offset    = LongObj  .read(in, access)
        val gain      = DoubleObj.read(in, access)
        new Apply(targets, artifact = artifact, spec = spec, offset = offset, gain = gain)
      }

      def name: String = "Apply"

      val opHi: Int = opID
      val opLo: Int = opID
    }
    private final class Apply[S <: Sys[S]](protected val targets: Targets[S],
                                           val artifact: Artifact[S],
                                           val spec: AudioFileSpec,
                                           val offset: LongObj[S],
                                           val gain: DoubleObj[S])
      extends lucre.expr.impl.NodeImpl[S, AudioCue] with Obj[S] {

      def tpe: stm.Obj.Type = AudioCue.Obj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Apply(Targets[Out], artifact = context(artifact), spec = spec, offset = context(offset),
          gain = context(gain)).connect()

      def value(implicit tx: S#Tx): AudioCue =
        AudioCue(artifact = artifact.value, spec = spec, offset = offset.value, gain = gain.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[AudioCue]] = {
          val artifactEvt = artifact.changed
          val artifactChO = if (pull.contains(artifactEvt)) pull(artifactEvt) else None
          val offsetEvt   = offset.changed
          val offsetChO   = if (pull.contains(offsetEvt  )) pull(offsetEvt  ) else None
          val gainEvt     = gain.changed
          val gainChO     = if (pull.contains(gainEvt    )) pull(gainEvt    ) else None

          if (artifactChO.isEmpty && offsetChO.isEmpty && gainChO.isEmpty) return None

          val artifactCh = artifactChO.getOrElse {
            val artifactV = artifact.value
            Change(artifactV, artifactV)
          }

          val offsetCh = offsetChO.getOrElse {
            val offsetV = offset.value
            Change(offsetV, offsetV)
          }

          val gainCh = gainChO.getOrElse {
            val gainV = gain.value
            Change(gainV, gainV)
          }

          val before  = AudioCue(artifactCh.before, spec, offset = offsetCh.before, gain = gainCh.before)
          val now     = AudioCue(artifactCh.now   , spec, offset = offsetCh.now,    gain = gainCh.now   )

          Some(Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(Apply.opID)
        artifact .write(out)
        AudioFileSpec.Serializer.write(spec, out)
        offset   .write(out)
        gain     .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        artifact.changed ---> changed
        offset  .changed ---> changed
        gain    .changed ---> changed
        this
      }

      private[this] def disconnect()(implicit tx: S#Tx): Unit = {
        artifact.changed -/-> changed
        offset  .changed -/-> changed
        gain    .changed -/-> changed
      }
    }
  }
  trait Obj[S <: Sys[S]] extends _Expr[S, AudioCue]
}
final case class AudioCue(artifact: Artifact.Value, spec: AudioFileSpec, offset: Long, gain: Double) {
  /** A simple forward to `spec.numChannels` */
  def numChannels: Int = spec.numChannels
}