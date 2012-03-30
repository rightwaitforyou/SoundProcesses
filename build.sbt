name := "soundprocesses"

version := "0.40-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses3" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.33",
   "de.sciss" %% "temporalobjects" % "0.31-SNAPSHOT",
   "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )   // "-Xelide-below", "INFO"


