name := "SoundProcesses"

version := "1.3.2-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.10.3"

// crossScalaVersions := Seq( "2.10.0", "2.9.2" )

resolvers in ThisBuild += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "1.3.+",
   "de.sciss" %% "lucreconfluent-event" % "1.6.1+",
   "de.sciss" %% "lucreevent-expr" % "1.6.+",
   "de.sciss" % "prefuse-core" % "0.21",
   "de.sciss" %% "lucrestm-bdb" % "1.6.+" % "test",
   "org.scalatest" %% "scalatest" % "1.9.2" /* cross CrossVersion.full */ % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )   // "-Xelide-below", "INFO"

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

initialCommands in console :=
   """// thanks to Rex Kerr for this trick (http://www.scala-lang.org/node/11813)
     |def shortresults[T](t: => T) = {
     |  val s = t.toString
     |  val name = s.takeWhile(_ != ':')
     |  val idx = s.indexOf(" = ")
     |  val full = if (idx >= 0) name + s.substring(idx) else s
     |  val short = if (full.length>799) full.substring(0,796)+"..." else full
     |  print(short)
     |  t
     |}
     |import de.sciss.synth._; import ugen._; import proc._
     |import de.sciss.lucre.stm.InMemory
     |import de.sciss.lucre.expr.{Expr, LinkedList}
     |import de.sciss.lucre.bitemp.Span
     |import de.sciss.lucre.{event => evt}
     |import expr.{Ints, Longs, Doubles, Spans}
     |println( "To disable result types:\n :power\n :wrap shortresults\n: silent" )""".stripMargin +
   "\nprintln( \"\"\"" +
   """
     |val vis = VisTest()
     |import vis._
     |val p     = t { implicit tx => val res = proc( "test" ); res.changed.react { u => println( "Proc observed: " + u )}; res }
     |val scan  = t { implicit tx => p.scans.add( "freq" )}
     |val g     = t { implicit tx => grapheme }
     |t { implicit tx => scan.source = Some( g )}
     |t { implicit tx => g.add( 0L, curve( 456.7 ))}
   """.stripMargin + "\"\"\")"

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
   BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
   BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth.proc"

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
<scm>
  <url>git@github.com:Sciss/SoundProcesses.git</url>
  <connection>scm:git:git@github.com:Sciss/SoundProcesses.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

// ---- ls.implicit.ly ----

// seq( lsSettings :_* )
// 
// (LsKeys.tags in LsKeys.lsync) := Seq( "sound", "music", "sound-synthesis", "computer-music" )
// 
// (LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )
// 
// (LsKeys.ghRepo in LsKeys.lsync) := Some( "SoundProcesses" )
// 
// // bug in ls -- doesn't find the licenses from global scope
// (licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

