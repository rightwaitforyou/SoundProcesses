package de.sciss.synth
package proc

import de.sciss.lucre.{stm, bitemp, event => evt}
import stm.Cursor
import bitemp.{BiExpr, BiGroup}
import de.sciss.lucre.expr.{ExprType, Expr}
import java.awt.EventQueue
import de.sciss.synth.io.AudioFile
import de.sciss.span.{Span, SpanLike}
import de.sciss.serial.Serializer
import de.sciss.synth.Curve.linear
import de.sciss.{lucre, synth}
import java.io.File
import concurrent.stm.{Txn => STMTxn, Ref => STMRef}
import synth.SynthGraph
import de.sciss.lucre.synth.{InMemory, Sys}
import proc.Implicits._

object VisTest {
  def apply(): VisTest[InMemory, InMemory] = {
    implicit val system = InMemory()
    new VisTest[InMemory, InMemory](system)
  }

  def baseDir   = new File(sys.props("user.home"), "sound_processes")
  def dataDir   = new File(baseDir, "db")
  def audioDir  = new File(baseDir, "artifacts")

  def inMem(): VisTest[InMemory, InMemory] = {
    implicit val system = InMemory()
    new VisTest[InMemory, InMemory](system)
  }

  //   def conf() : VisTest[ Confluent ] = {
  //      val dir              = dataDir
  //      dir.mkdirs()
  //      val store            = BerkeleyDB.factory( dir )
  //      implicit val system  = Confluent( store )
  //      new VisTest( system )
  //   }

  def wipe(sure: Boolean = false): Unit = {
    if (!sure) return
    dataDir.listFiles().foreach(_.delete())
    dataDir.delete()
  }

  def main(args: Array[String]): Unit = {
    //      TemporalObjects.showConfluentLog = true
    //val vis = VisTest.conf()
    val vis = VisTest.inMem()
    import vis._
    add()
    aural()
    Thread.sleep(8000L)
    play()
  }
}
final class VisTest[S <: Sys[S], I <: evt.Sys[I]](system: S)(implicit cursor: Cursor[S], bridge: S#Tx => I#Tx)
  extends ExprImplicits[S] {
  //   type S  = Sy
  type Tx = S#Tx

  def t[A](fun: S#Tx => A): A = {
    val peer = STMTxn.findCurrent
    require(peer.isEmpty, peer)
    cursor.step(fun)
  }

  //   private type PG = ProcGroup.Modifiable[ S ]
  private type PG = ProcGroup.Modifiable[S] // BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]
  type Acc = (PG, ArtifactLocation.Modifiable[S])

  object Implicits {
    //      implicit def procVarSer: Serializer[ S#Tx, S#Acc, PG ] = ProcGroup.Modifiable.serializer[ S ]
    implicit         val spanLikes : ExprType[SpanLike] = bitemp.SpanLike
    implicit private val procSer   : Serializer[S#Tx, S#Acc, Proc[S]] = Proc.serializer[S]
    implicit         val procVarSer: Serializer[S#Tx, S#Acc, PG     ] =
      ProcGroup.Modifiable.serializer[S] // , Proc[S], Proc.Update[S]](_.changed)
    //      implicit val accessTransport: Acc => Transport[ S, Proc[ S ]] = _._2
    //      implicit val transportSer: Serializer[ S#Tx, S#Acc, ProcTransport[ S ]] = ?? // Transport.serializer[ S ]( cursor )
  }

  import Implicits._

  lazy val access: stm.Source[S#Tx, Acc] = system.root { implicit tx =>
    implicit def longType = lucre.expr.Long
    val g = ProcGroup.Modifiable[S]
    g.changed.react { _ => upd =>
      println("Group observed: " + upd)
    }
    val loc = ArtifactLocation.Modifiable[S](VisTest.audioDir)
    g -> loc
  }

  access // initialize !

  val (trans) = cursor.step { implicit tx =>
    val g = group
    val tr = TransportOLD[S, I](g)
    tr.react {
      _ => upd =>
        println("Transport observed: " + upd)
    }
    tr // -> ArtifactStore[ S ]( VisTest.audioDir )
  }
  //      val trv  = tx.newVar[ Transport[ S, Proc[ S ]]]( tr.id, tr )


  //   val groupAccess:     Source[ S#Tx, ProcGroup.Modifiable[ S ]] = Source.map( access )( _._1 )
  //   val transportAccess: Source[ S#Tx, Transport[ S, Proc[ S ]]]   = Source.map( access )( _._2 )

  def group(implicit tx: S#Tx): ProcGroup.Modifiable[S] = access()._1

  def artifactStore(implicit tx: S#Tx) = access()._2 // ._1
  // def loc( implicit tx: S#Tx )  = access()._2._2

  def grapheme(implicit tx: S#Tx): Grapheme.Modifiable[S] = Grapheme.Modifiable[S](1)

  def curve(amp: Expr[S, Double], shape: Curve = linear)(implicit tx: S#Tx) = Grapheme.Expr.Curve(amp -> shape)

  def proc(name: String)(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
    // implicit val chr: Chronos[S] = Chronos(0L)
    val p = Proc[S]
    // p.name_=( name )
    p.graph() = SynthGraph {
      import synth._
      import ugen._
      val f = graph.scan.In("freq", 50) // fundamental frequency
      val p = 20 // number of partials per channel
      val m = Mix.tabulate(p) {
          i =>
            FSinOsc.ar(f * (i + 1)) *
              (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
        }
      //         Out_~( "sig", WhiteNoise.ar )
      //         SinOsc.ar( In_~( "freq", 441 ))
      //         Out_!( "trig", Dust.kr( 1 ))
      //         Decay.ar( In_!( "trig2" ))
      //         Trigger( "silence", DetectSilence.ar( m ))

      Out.ar(0, m)
    }
    val g = Grapheme.Modifiable[S](1)
    val scan = p.scans.add("freq")
    scan.addSource(Scan.Link.Grapheme(g))

    //      p.trigger( "silence" )

    //      p.playing_=( true )
    val obj = Obj(Proc.Elem(p))
    obj.attr.name = name
    obj
  }

  def next(time: Long): Option[Long] = t { implicit tx =>
    group.nearestEventAfter(time)
  }

  def prev(time: Long): Option[Long] = t { implicit tx =>
    group.nearestEventBefore(time)
  }

  def clear(): Unit =
    t { implicit tx =>
      group.clear()
    }

  def add(span: SpanLike = Span(3 * 44100, 6 * 44100), name: String = "Proc"): Unit =
    t { implicit tx =>
      val p = proc(name)
      group.add(span, p)
    }

  def play(): Unit =
    t { implicit tx => trans.play() }

  def stop(): Unit =
    t { implicit tx => trans.stop() }

  def rewind(): Unit = seek(0L)

  def seek(pos: Long): Unit =
    t { implicit tx =>
      trans.stop()
      trans.seek(pos)
    }

  def within(span: SpanLike) = t { implicit tx =>
    group.intersect(span).toIndexedSeq
  }

  def range(start: SpanLike, stop: SpanLike) = t { implicit tx =>
    group.rangeSearch(start, stop).toIndexedSeq
  }

  def defer(thunk: => Unit): Unit =
    EventQueue.invokeLater(new Runnable {
      def run(): Unit = thunk
    })

  //   private var frameVar: JFrame = null
  //   def frame = frameVar
  //
  //   private val visVar = STMRef( Option.empty[ VisualInstantPresentation[ S ]])
  //
  //   def gui() { t { implicit tx =>
  //      implicit val itx = tx.peer
  //      if( visVar().isEmpty ) {
  //         val vis = VisualInstantPresentation( trans )
  //         visVar.set( Some( vis ))
  //         STMTxn.afterCommit( _ => defer {
  //            val f    = new JFrame( "Vis" )
  //            frameVar = f
  //            val cp   = f.getContentPane
  //            cp.add( vis.view, BorderLayout.CENTER )
  //            f.pack()
  //            f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
  //            f.setLocationRelativeTo( null )
  //            f.setLocation( f.getX, 0 )
  //            f.setAlwaysOnTop( true )
  //            f.setVisible( true )
  //         })
  //      }
  //   }}

  private val auralVar = STMRef(Option.empty[AuralPresentation[S]])

  def aural(): Unit = {
    if (auralVar.single().isDefined) return

    t { implicit tx =>
      val as = AuralSystem.start()
      implicit val itx = tx.peer
      implicit val _artifactStore = artifactStore
      auralVar() = Some(AuralPresentation.run[S](trans, as))
    }
  }

  def pr(time: Long = 4 * 44100)(implicit tx: S#Tx) = group.intersect(time).next._2.head.value

  def addFreq(time: Expr[S, Long] = 0, freq: Expr[S, Param]): Unit =
    t { implicit tx =>
      addFreq2(time -> curve(freq))
    }

  def audioFile(path: String)(implicit tx: S#Tx): Grapheme.Value.Audio = {
    val f = new File(path)
    val loc = artifactStore
    val artifact = loc.add(f) // Artifact(Nil, path)
    val spec = AudioFile.readSpec(f) // artifact.toFile )
    val offset = 0L
    val gain = 1.0
    Grapheme.Value.Audio(artifact.value, spec, offset, gain)
  }

  def addAudio(time: Expr[S, Long] = 0, freq: Grapheme.Value.Audio): Unit =
    t { implicit tx =>
      addFreq2(time -> freq)
    }

  def audioTest()(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
    val af = audioFile("283_7WTConWhiteCCRsmpLp.aif")

    t { implicit tx =>
      val p = proc("AudioFilePlayer")
      p.elem.peer.graph() = SynthGraph {
        import synth._
        import ugen._
        val in = graph.scan.In("in")
        Out.ar(0, in * SinOsc.ar(3))
      }
      val g     = Grapheme.Modifiable[S](af.numChannels)
      val scan  = p.elem.peer.scans.add("in")
      scan.addSource(Scan.Link.Grapheme(g))

      g.add(0L -> af)
      group.add(Span(0.sec, 4.sec), p)
      p
    }
  }

  private def addFreq2(value: BiExpr[S, Grapheme.Value])(implicit tx: S#Tx): Unit =
    pr().elem.peer.scans.get("freq").foreach { scan =>
      scan.sources.foreach {
        case Scan.Link.Grapheme(Grapheme.Modifiable(peer)) => peer.add(value)
        case _ =>
      }
    }

  //   implicit def richNum( d: Double ) : RichDouble = new RichDouble( d )

  implicit final class RichDouble private[VisTest](d: Double) {
    def sec: Long = (d * 44100).toLong
  }
}
