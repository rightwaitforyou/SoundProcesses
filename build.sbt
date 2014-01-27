lazy val logicalName = "SoundProcesses"

name                      := logicalName + "-full"

version in ThisBuild      := "2.1.0-SNAPSHOT"

organization in ThisBuild := "de.sciss"

homepage in ThisBuild     := Some(url("https://github.com/Sciss/" + logicalName))

description               := "A framework for creating and managing ScalaCollider based sound processes"

licenses in ThisBuild     := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

scalaVersion in ThisBuild := "2.10.3"

resolvers in ThisBuild += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

lazy val lucreCoreVersion       = "2.0.+"

lazy val lucreDataVersion       = "2.2.1+"   // debugSanitize!

lazy val lucreEventVersion      = "2.5.+"

lazy val lucreConfluentVersion  = "2.6.+"

lazy val scalaColliderVersion   = "1.10.+"

lazy val scalaTestVersion       = "1.9.1"

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")   // "-Xelide-below", "INFO"

// SI-7481
// scalacOptions += "-no-specialization"

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

initialCommands in console :=
   """// thanks to Rex Kerr for this trick (http://www.scala-lang.org/node/11813)
     |def shortResults[T](t: => T): T = {
     |  val s = t.toString
     |  val name = s.takeWhile(_ != ':')
     |  val idx = s.indexOf(" = ")
     |  val full = if (idx >= 0) name + s.substring(idx) else s
     |  val short = if (full.length > 799) full.substring(0, 796) + "..." else full
     |  print(short)
     |  t
     |}
     |import de.sciss.synth._; import ugen._; import proc._
     |import de.sciss.lucre.stm.InMemory
     |import de.sciss.lucre.expr.{Expr, LinkedList}
     |import de.sciss.span.Span
     |import de.sciss.lucre.{event => evt}
     |import expr.{Ints, Longs, Doubles, Spans}
     |println("To disable result types:\n :power\n :wrap shortResults\n: silent")""".stripMargin +
   "\nprintln(\"\"\"" +
   """
     |val vis = VisTest()
     |import vis._
     |val p     = t { implicit tx => val res = proc("test"); res.changed.react { u => println("Proc observed: " + u)}; res }
     |val scan  = t { implicit tx => p.scans.add("freq")}
     |val g     = t { implicit tx => grapheme }
     |t { implicit tx => scan.source = Some(g)}
     |t { implicit tx => g.add(0L, curve(456.7))}
     |""".stripMargin + "\"\"\")"

// ---- sub-projects ----

lazy val root = project.in(file("."))
  .aggregate(lucrebitemp, lucresynth, `lucresynth-expr`, soundprocesses)
  .dependsOn(lucrebitemp, lucresynth, `lucresynth-expr`, soundprocesses)
  .settings(
    //    publishArtifact in(Compile, packageBin) := false, // there are no binaries
    //    publishArtifact in(Compile, packageDoc) := false, // there are no javadocs
    //    publishArtifact in(Compile, packageSrc) := false, // there are no sources
    packagedArtifacts := Map.empty
    // autoScalaLibrary := false
  )

lazy val lucrebitemp = project.in(file("bitemp")).settings(
  description := "Bitemporal Lucre extensions using Long expressions for time",
  libraryDependencies ++= Seq(
    "de.sciss"      %% "lucredata-core"  % lucreDataVersion,
    "de.sciss"      %% "lucreevent-expr" % lucreEventVersion,
    "de.sciss"      %% "span"            % "1.2.+"
  )
)

lazy val `lucresynth-expr` = project.in(file("synth-expr")).dependsOn(lucrebitemp).settings(
  description := "Bitemporal expression types for SoundProcesses",
  libraryDependencies ++= Seq(
    "de.sciss"      %% "scalacollider"  % scalaColliderVersion,
    "de.sciss"      %% "numbers"        % "0.1.+",
    "de.sciss"      %% "lucrestm-bdb"   % lucreCoreVersion      % "test",
    "org.scalatest" %% "scalatest"      % scalaTestVersion      % "test",
    "de.sciss"      %% "lucreconfluent" % lucreConfluentVersion % "test"
  )
)

lazy val lucresynth = project.in(file("synth")).settings(
  description := "Transactional extension for ScalaCollider",
  libraryDependencies ++= Seq(
    "de.sciss" %% "lucreevent-core" % lucreEventVersion,
    "de.sciss" %% "scalacollider"   % scalaColliderVersion
  )
)

lazy val soundprocesses = project.in(file("proc")).dependsOn(lucrebitemp, lucresynth, `lucresynth-expr`)
  .settings(buildInfoSettings: _*).settings(
    description :=  "A framework for creating and managing ScalaCollider based sound processes",
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucreconfluent" % lucreConfluentVersion,
      "org.scalatest" %% "scalatest"      % scalaTestVersion      % "test",
      "de.sciss"      %% "lucrestm-bdb"   % lucreCoreVersion      % "test"
    )
  )

// ---- publishing ----

publishMavenStyle in ThisBuild := true

publishTo in ThisBuild :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild := { val n = logicalName
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

(LsKeys.tags   in LsKeys.lsync) := Seq("sound", "music", "sound-synthesis", "computer-music")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) := Some(logicalName)
