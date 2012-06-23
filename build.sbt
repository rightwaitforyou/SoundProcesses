name := "soundprocesses"

version := "0.40-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses3" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.34",
   "de.sciss" %% "temporalobjects" % "0.33",
   "de.sciss" %% "lucreexpr" % "0.33",
   "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )   // "-Xelide-below", "INFO"

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
import de.sciss.lucre.expr.Span
println( "To disable result types:\n :power\n :wrap shortresults\n: silent" )
"""
