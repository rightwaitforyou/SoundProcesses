name := "soundprocesses"

version := "0.40-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses3" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2" // "2.10.0-M7"

// scalaBinaryVersion := "2.10.0-M7"

// crossScalaVersions in ThisBuild := Seq( "2.10.0-M6", "2.9.2" )

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "1.1.+", // "0.34",
   "de.sciss" %% "temporalobjects" % "1.0.+",
   "de.sciss" %% "lucreexpr" % "1.1.+",
   "de.sciss" % "prefuse-core" % "0.21"
)

libraryDependencies in ThisBuild <+= scalaVersion { sv =>
   val v = sv match {
      case "2.10.0-M7" => "1.9-2.10.0-M7-B1"
      case _ => "1.8"
   }
   "org.scalatest" %% "scalatest" % v % "test"
}

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )   // "-Xelide-below", "INFO"

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

initialCommands in console := """// thanks to Rex Kerr for this trick (http://www.scala-lang.org/node/11813)
def shortresults[T](t: => T) = {
   val s = t.toString
   val name = s.takeWhile(_ != ':')
   val idx = s.indexOf(" = ")
   val full = if (idx >= 0) name + s.substring(idx) else s
   val short = if (full.length>799) full.substring(0,796)+"..." else full
   print(short)
   t
}
import de.sciss.synth._; import ugen._; import proc._
import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.expr.{Expr, LinkedList}
import de.sciss.lucre.bitemp.Span
import de.sciss.lucre.{event => evt}
import expr.{Ints, Longs, Doubles, Spans}
println( "To disable result types:\n :power\n :wrap shortresults\n: silent" )
"""
