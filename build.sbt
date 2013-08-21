name := "SoundProcesses"

version := "1.9.1-SNAPSHOT"

organization := "de.sciss"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion := "2.10.2"

resolvers in ThisBuild += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

libraryDependencies += "de.sciss" %% "lucredata-core" % "2.2.1+"  // debugSanitize!

libraryDependencies ++= {
  val confluentVersion = "2.5.+"
  val eventVersion     = "2.4.+"
  val stmVersion       = "2.0.+"
  Seq(
    "de.sciss" %% "scalacollider"   % "1.9.+",
    "de.sciss" %  "prefuse-core"    % "0.21",
    "de.sciss" %% "lucreconfluent"  % confluentVersion,
    "de.sciss" %% "lucreevent-expr" % eventVersion,
    "de.sciss" %% "span"            % "1.2.+",
    "de.sciss"      %% "lucrestm-bdb" % stmVersion % "test",
    "org.scalatest" %% "scalatest"    % "1.9.1"    % "test"
  )
}

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")   // "-Xelide-below", "INFO"

// SI-7481
// scalacOptions += "-no-specialization"

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
     |import de.sciss.span.Span
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
     |""".stripMargin + "\"\"\")"

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth.proc"

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { v =>
  Some(if (v endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra <<= name { n =>
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- ls.implicit.ly ----

seq(lsSettings :_*)

(LsKeys.tags in LsKeys.lsync) := Seq("sound", "music", "sound-synthesis", "computer-music")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) <<= name(Some(_))
