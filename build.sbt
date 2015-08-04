lazy val baseName  = "SoundProcesses"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "2.22.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  description        := "A framework for creating and managing ScalaCollider based sound processes",
  licenses           := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")),
  scalaVersion       := "2.11.7",
  crossScalaVersions := Seq("2.11.7", "2.10.5"),
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat
) ++ publishSettings

lazy val lucreCoreVersion          = "2.1.2"
lazy val lucreDataVersion          = "2.3.3"
lazy val lucreEventVersion         = "2.7.5"
lazy val lucreConfluentVersion     = "2.11.3"
lazy val scalaColliderVersion      = "1.17.3"
lazy val scalaColliderSwingVersion = "1.25.2"
lazy val spanVersion               = "1.3.1"
lazy val lucreSwingVersion         = "0.9.1"
lazy val audioWidgetsVersion       = "1.9.1"
lazy val fileUtilVersion           = "1.1.1"
lazy val topologyVersion           = "1.0.0"

// ---- test-only ----

lazy val scalaTestVersion          = "2.2.5"
lazy val loggingEnabled            = true
lazy val bdb                       = "bdb"  // either "bdb" or "bdb6"

scalacOptions in ThisBuild ++= {
  // "-Xlint" -- produces problems with implicit objects and traits in package object
  // "-Xfatal-warnings" -- breaks for cross-scala-build and deprecations
  val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")
  if (loggingEnabled || isSnapshot.value) xs else xs ++ Seq("-Xelide-below", "INFO")
}

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
     |import de.sciss.lucre.expr.Expr
     |import de.sciss.span.Span
     |import de.sciss.lucre.{event => evt}
     |import de.sciss.lucre.expr.{Int => IntEx, Long => LongEx, Double => DoubleEx}
     |import de.sciss.lucre.bitemp.{Span => SpanEx}
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

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(bitemp, synth, expr, core, views, compiler).
  dependsOn(bitemp, synth, expr, core, views, compiler).
  settings(commonSettings).
  settings(
    name := baseName,
    publishArtifact in(Compile, packageBin) := false, // there are no binaries
    publishArtifact in(Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in(Compile, packageSrc) := false, // there are no sources
    // packagedArtifacts := Map.empty
    autoScalaLibrary := false
  )

lazy val bitemp = Project(id = "lucrebitemp", base = file("bitemp")).
  settings(commonSettings).
  settings(
    description := "Bitemporal Lucre extensions using Long expressions for time",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucredata-core"  % lucreDataVersion,
      "de.sciss"      %% "lucreevent-expr" % lucreEventVersion,
      "de.sciss"      %% "span"            % spanVersion
    )
  )

lazy val expr = Project(id = "lucresynth-expr", base = file("synth-expr")).
  dependsOn(bitemp).
  settings(commonSettings).
  settings(
    description := "Bitemporal expression types for SoundProcesses",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "scalacollider"  % scalaColliderVersion,
      "de.sciss"      %% s"lucrestm-$bdb" % lucreCoreVersion      % "test",
      "org.scalatest" %% "scalatest"      % scalaTestVersion      % "test",
      "de.sciss"      %% "lucreconfluent" % lucreConfluentVersion % "test"
    )
  )

lazy val synth = Project(id = "lucresynth", base = file("synth")).
  settings(commonSettings).
  settings(
    description := "Transactional extension for ScalaCollider",
    libraryDependencies ++= Seq(
      "de.sciss" %% "topology"        % topologyVersion,
      "de.sciss" %% "lucrestm-core"   % lucreCoreVersion,
      "de.sciss" %% "lucreevent-core" % lucreEventVersion,
      "de.sciss" %% "scalacollider"   % scalaColliderVersion
    )
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  dependsOn(bitemp, synth, expr).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    description := "A framework for creating and managing ScalaCollider based sound processes",
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.synth.proc",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "lucreconfluent"       % lucreConfluentVersion,
      "de.sciss"      %% "lucreevent-artifact"  % lucreEventVersion,
      "de.sciss"      %% "fileutil"             % fileUtilVersion,
      "org.scalatest" %% "scalatest"            % scalaTestVersion      % "test",
      "de.sciss"      %% s"lucrestm-$bdb"       % lucreCoreVersion      % "test"
    )
  )

lazy val views = Project(id = s"$baseNameL-views", base = file("views")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    description := "Views for Sound Processes",
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucreswing"       % lucreSwingVersion,
      "de.sciss" %% "audiowidgets-app" % audioWidgetsVersion
    )
  )

lazy val compiler = Project(id = s"$baseNameL-compiler", base = file("compiler")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    description := "Compiler-support for Sound Processes",
    libraryDependencies ++= Seq(
      "org.scala-lang" %  "scala-compiler"          % scalaVersion.value,
      "de.sciss"       %% s"lucrestm-$bdb"          % lucreCoreVersion          % "test",
      "de.sciss"       %% "fileutil"                % fileUtilVersion           % "test",
      "de.sciss"       %% "lucreswing"              % lucreSwingVersion         % "test",
      "de.sciss"       %% "scalacolliderswing-core" % scalaColliderSwingVersion % "test"
    )
  )

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
<scm>
  <url>git@github.com:Sciss/{baseName}.git</url>
  <connection>scm:git:git@github.com:Sciss/{baseName}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)

// ---- ls.implicit.ly ----

// seq(lsSettings :_*)
// (LsKeys.tags   in LsKeys.lsync) := Seq("sound", "music", "sound-synthesis", "computer-music")
// (LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")
// (LsKeys.ghRepo in LsKeys.lsync) := Some(logicalName)
