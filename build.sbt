import scala.language.postfixOps
import scala.sys.process.*
import org.scalajs.linker.interface.{CheckedBehavior, ModuleSplitStyle}

organization := "org.scorexfoundation"

name := "sigma-state"

lazy val scala213 = "2.13.11"
lazy val scala212 = "2.12.18"
lazy val scala211 = "2.11.12"

lazy val allConfigDependency = "compile->compile;test->test"

lazy val commonSettings = Seq(
  organization := "org.scorexfoundation",
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        Seq("-Ywarn-unused:_,imports", "-Ywarn-unused:imports", "-Wconf:src=src_managed/.*:silent", "-release", "8")
      case Some((2, 12)) =>
        Seq("-Ywarn-unused:_,imports", "-Ywarn-unused:imports", "-release", "8")
      case Some((2, 11)) =>
        Seq()
      case _ => sys.error("Unsupported scala version")
    }
  },
  javacOptions ++= javacReleaseOption,
  resolvers += Resolver.sonatypeRepo("public"),
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  homepage := Some(url("https://github.com/ScorexFoundation/sigmastate-interpreter")),
  description := "Interpreter of a Sigma-State language",
  pomExtra :=
      <developers>
        <developer>
          <id>kushti</id>
          <name>Alexander Chepurnoy</name>
          <url>http://chepurnoy.org/</url>
        </developer>
        <developer>
          <id>aslesarenko</id>
          <name>Alexander Slesarenko</name>
          <url>https://github.com/aslesarenko/</url>
        </developer>
        <developer>
          <id>greenhat</id>
          <name>Denys Zadorozhnyi</name>
          <url>https://github.com/greenhat/</url>
        </developer>
      </developers>,
  publishMavenStyle := true,
  publishTo := sonatypePublishToBundle.value,
  scmInfo := Some(
      ScmInfo(
          url("https://github.com/ScorexFoundation/sigmastate-interpreter"),
          "scm:git@github.com:ScorexFoundation/sigmastate-interpreter.git"
      )
  )
)

lazy val crossScalaSettings = Seq(
  crossScalaVersions := Seq(scala213, scala212, scala211),
  scalaVersion := scala213
)
lazy val crossScalaSettingsJS = Seq(
  crossScalaVersions := Seq(scala213),
  scalaVersion := scala213
)

def javacReleaseOption = {
  if (System.getProperty("java.version").startsWith("1."))
  // java <9 "--release" is not supported
    Seq()
  else
    Seq("--release", "8")
}

// suffix version with "-SNAPSHOT" for builds without a git tag
ThisBuild / dynverSonatypeSnapshots := true
// use "-" instead of default "+"
ThisBuild / dynverSeparator := "-"

val bouncycastleBcprov = "org.bouncycastle" % "bcprov-jdk15on" % "1.66"

val scrypto            = "org.scorexfoundation" %% "scrypto" % "3.0.0"
val scryptoDependency =
  libraryDependencies += "org.scorexfoundation" %%% "scrypto" % "3.0.0"

val scorexUtil         = "org.scorexfoundation" %% "scorex-util" % "0.2.1"
val scorexUtilDependency =
  libraryDependencies += "org.scorexfoundation" %%% "scorex-util" % "0.2.1"

val debox              = "org.scorexfoundation" %% "debox" % "0.10.0"
val spireMacros        = "org.typelevel" %% "spire-macros" % "0.17.0-M1"

val fastparse          = "com.lihaoyi" %% "fastparse" % "2.3.3"
val fastparseDependency =
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.3"

val supertaggedDependency =
  libraryDependencies += "org.rudogma" %%% "supertagged" % "2.0-RC2"

val scalaCompat        = "org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0"
lazy val scodecBitsDependency =
  libraryDependencies += "org.scodec" %%% "scodec-bits" % "1.1.34"

lazy val circeCore211 = "io.circe" %% "circe-core" % "0.10.0"
lazy val circeGeneric211 = "io.circe" %% "circe-generic" % "0.10.0"
lazy val circeParser211 = "io.circe" %% "circe-parser" % "0.10.0"

lazy val circeCore = "io.circe" %% "circe-core" % "0.13.0"
lazy val circeGeneric = "io.circe" %% "circe-generic" % "0.13.0"
lazy val circeParser = "io.circe" %% "circe-parser" % "0.13.0"

def circeDeps(scalaVersion: String) = if (scalaVersion == scala211)
  Seq(circeCore211, circeGeneric211, circeParser211)
else
  Seq(circeCore, circeGeneric, circeParser)

def circeDependency = {
  libraryDependencies ++= {
    val version = scalaVersion.value
    val deps211 = Seq(
      "io.circe" %%% "circe-core" % "0.10.0",
      "io.circe" %%% "circe-generic" % "0.10.0",
      "io.circe" %%% "circe-parser" % "0.10.0")
    val deps212 = Seq(
      "io.circe" %%% "circe-core" % "0.13.0",
      "io.circe" %%% "circe-generic" % "0.13.0",
      "io.circe" %%% "circe-parser" % "0.13.0")
    if (version == scala211) deps211 else deps212
  }
}

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.14" % Test
lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.14" % Test
lazy val pprint = "com.lihaoyi" %% "pprint" % "0.6.3" % Test
lazy val scalameter = "com.storm-enroute" %% "scalameter" % "0.19" % Test

lazy val testingDependencies = Seq(
  scalatest, scalactic,
  "org.scalacheck" %% "scalacheck" % "1.15.2" % Test,          // last supporting Scala 2.11
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test, // last supporting Scala 2.11
  pprint,
  scalameter
)

lazy val testingDependencies2 =
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.2.14" % Test,
    "org.scalactic" %%% "scalactic" % "3.2.14" % Test,
    "org.scalacheck" %%% "scalacheck" % "1.15.2" % Test,          // last supporting Scala 2.11
    "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.3.0" % Test, // last supporting Scala 2.11
    "com.lihaoyi" %%% "pprint" % "0.6.3" % Test
  )

lazy val testSettings = Seq(
  libraryDependencies ++= testingDependencies,
  Test / parallelExecution := false,
  Test / baseDirectory := file("."),
  Test / publishArtifact := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  assembly / test := {})

lazy val testSettings2 = Seq(
  Test / parallelExecution := true,
  Test / baseDirectory := file("."),
  Test / publishArtifact := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  assembly / test := {})

scalacOptions ++= Seq("-feature", "-deprecation")

Test / parallelExecution := false
Test / publishArtifact := true

pomIncludeRepository := { _ => false }

def libraryDefSettings = commonSettings ++ crossScalaSettings ++ testSettings

lazy val commonDependenies2 = libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scorexfoundation" %%% "debox" % "0.10.0",
  "org.scala-lang.modules" %%% "scala-collection-compat" % "2.7.0"
)

val sigmajsCryptoFacadeVersion = "0.0.7"

lazy val core   = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .settings(commonSettings ++ testSettings2,
    commonDependenies2,
    testingDependencies2,
    scorexUtilDependency,
    publish / skip := true
  )
  .jvmSettings(
    crossScalaSettings,
    libraryDependencies ++= Seq(
      bouncycastleBcprov
    )
  )
  .jsSettings(
    crossScalaSettingsJS,
    scalacOptions ++= Seq(
      // Suppress warning about the global execution context in Scala.js is based on JS
      // Promises (microtasks). Using it may prevent macrotasks (I/O, timers, UI
      // rendering) from running fairly.
      "-P:scalajs:nowarnGlobalExecutionContext"
    ),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"
    ),
    useYarn := true
  )
lazy val coreJS = core.js
    .enablePlugins(ScalaJSBundlerPlugin)
    .enablePlugins(ScalablyTypedConverterGenSourcePlugin)
    .settings(
      stOutputPackage := "sigmastate",
      scalaJSLinkerConfig ~= { conf =>
        conf.withSourceMap(false)
            .withModuleKind(ModuleKind.CommonJSModule)
      },
      Compile / npmDependencies ++= Seq(
        "sigmajs-crypto-facade" -> sigmajsCryptoFacadeVersion,
        "@fleet-sdk/common" -> "0.1.3"
      )
    )

lazy val data = crossProject(JVMPlatform, JSPlatform)
  .in(file("data"))
  .dependsOn(core % allConfigDependency)
  .settings(
    commonSettings ++ testSettings2,
    commonDependenies2,
    testingDependencies2,
    scorexUtilDependency, fastparseDependency, circeDependency, scryptoDependency,
    publish / skip := true
  )
  .jvmSettings( crossScalaSettings )
  .jsSettings(
    crossScalaSettingsJS,
    useYarn := true
  )
lazy val dataJS = data.js
    .enablePlugins(ScalaJSBundlerPlugin)

lazy val interpreter = crossProject(JVMPlatform, JSPlatform)
  .in(file("interpreter"))
  .dependsOn(core % allConfigDependency, data % allConfigDependency)
  .settings(
    commonSettings ++ testSettings2,
    commonDependenies2,
    testingDependencies2,
    scorexUtilDependency, fastparseDependency, circeDependency, scryptoDependency,
    publish / skip := true
  )
  .jvmSettings( crossScalaSettings )
  .jsSettings(
    crossScalaSettingsJS,
    useYarn := true
  )
lazy val interpreterJS = interpreter.js
    .enablePlugins(ScalaJSBundlerPlugin)

lazy val parsers = crossProject(JVMPlatform, JSPlatform)
    .in(file("parsers"))
    .dependsOn(interpreter % allConfigDependency)
    .settings(
      commonSettings ++ testSettings2,
      commonDependenies2,
      testingDependencies2,
      scorexUtilDependency, fastparseDependency, circeDependency,
      publish / skip := true
    )
    .jvmSettings(
      crossScalaSettings
    )
    .jsSettings(
      crossScalaSettingsJS,
      useYarn := true
    )
lazy val parsersJS = parsers.js
    .enablePlugins(ScalaJSBundlerPlugin)
    .settings(
      scalaJSLinkerConfig ~= { conf =>
        conf.withSourceMap(false)
      },
    )

lazy val sdk = crossProject(JVMPlatform, JSPlatform)
    .in(file("sdk"))
    .dependsOn(core % allConfigDependency, data % allConfigDependency, interpreter % allConfigDependency, parsers % allConfigDependency)
    .settings(commonSettings ++ testSettings2,
      commonDependenies2,
      testingDependencies2,
      scodecBitsDependency,
      circeDependency,
      publish / skip := true
    )
    .jvmSettings(
      crossScalaSettings
    )
    .jsSettings(
      crossScalaSettingsJS,
      useYarn := true
    )
lazy val sdkJS = sdk.js
    .enablePlugins(ScalaJSBundlerPlugin)
    .settings(
      scalaJSLinkerConfig ~= { conf =>
        conf.withSourceMap(false)
            .withModuleKind(ModuleKind.CommonJSModule)
      },
    )

lazy val sc = crossProject(JVMPlatform, JSPlatform)
    .in(file("sc"))
    .dependsOn(
      interpreter % allConfigDependency,
      parsers % allConfigDependency,
      sdk % allConfigDependency
    )
    .settings(
      commonSettings ++ testSettings2,
      commonDependenies2,
      testingDependencies2,
      scorexUtilDependency, fastparseDependency, circeDependency,
      scryptoDependency,
      Test / parallelExecution := false
    )
    .settings(publish / skip := true)
    .jvmSettings(
      crossScalaSettings,
      libraryDependencies ++= Seq(scalameter)
    )
    .jsSettings(
      crossScalaSettingsJS,
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0"
      ),
      useYarn := true
    )
lazy val scJS = sc.js
    .enablePlugins(ScalaJSBundlerPlugin)
    .settings(publish / skip := false)
    .settings(
      scalaJSLinkerConfig ~= { conf =>
        conf.withSourceMap(false)
            .withModuleKind(ModuleKind.CommonJSModule)
            .withSemantics(sem =>
              // compliance with JVM semantics is required for tests to pass on JS
              // we sacrifice some optimizations (and performance) for that
              sem.withAsInstanceOfs(CheckedBehavior.Compliant)
                  .withArrayIndexOutOfBounds(CheckedBehavior.Compliant)
            )
      },
    )


lazy val sigma = (project in file("."))
  .aggregate(core.jvm, data.jvm, interpreter.jvm, parsers.jvm, sdk.jvm, sc.jvm)
  .settings(libraryDefSettings, rootSettings)
  .settings(publish / aggregate := false)
  .settings(publishLocal / aggregate := false)

lazy val aggregateCompile = ScopeFilter(
  inProjects(core.jvm, data.jvm, interpreter.jvm, parsers.jvm, sdk.jvm, sc.jvm),
  inConfigurations(Compile))

lazy val rootSettings = Seq(
  Compile / sources := sources.all(aggregateCompile).value.flatten,
  Compile / sourceDirectories := sourceDirectories.all(aggregateCompile).value.flatten,
  libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten,
  mappings in (Compile, packageSrc) ++= (mappings in(Compile, packageSrc)).all(aggregateCompile).value.flatten,
  mappings in (Test, packageBin) ++= (mappings in(Test, packageBin)).all(aggregateCompile).value.flatten,
  mappings in(Test, packageSrc) ++= (mappings in(Test, packageSrc)).all(aggregateCompile).value.flatten
)

val credentialFile = Path.userHome / ".sbt" / ".sigma-sonatype-credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq


// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
pgpPublicRing := file("ci/pubring.asc")
pgpSecretRing := file("ci/secring.asc")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("C1FD62B4D44BDF702CDF2B726FF59DA944B150DD")

