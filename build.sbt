name := "soundprocesses"

version := "0.40-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses3" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.34",
   "de.sciss" %% "temporalobjects" % "0.33-SNAPSHOT",
   "de.sciss" %% "lucreexpr" % "0.11-SNAPSHOT",
   "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )   // "-Xelide-below", "INFO"

initialCommands in console := """import de.sciss.synth._; import ugen._; import proc._
import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.expr.Span
// type S = InMemory
// implicit val system: S = de.sciss.lucre.stm.InMemory()
// def t[ A ]( fun: S#Tx => A ) : A = system.step( fun )
"""
