
lazy val commonSettings = Seq(
  name := "TomsOpt",
  version := "0.5",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12"
  ),
  scalacOptions ++= Seq(
//    "-Yinline-warnings",
//    "-deprecation",
    "-optimize"
  ),
  javaOptions in run ++= Seq(
    "-Xmx12G",
    "-XX:+AggressiveOpts"
//    "-XX:+UnlockDiagnosticVMOptions",
//    "-XX:+PrintInlining",
  )
)

lazy val core = project
  .dependsOn(native % Runtime)
  .settings(commonSettings)
  .settings(target in javah := (sourceDirectory in nativeCompile in native).value / "include")
  .settings(libraryDependencies ++= Seq(
    "com.github.scopt" %% "scopt" % "3.4.0",
    "com.github.aishfenton" %% "vegas" % "0.2.4",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
  ))

lazy val native = project
  .settings(commonSettings)
  .settings(sourceDirectory in nativeCompile := sourceDirectory.value)
  .enablePlugins(JniNative)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)


lazy val benchmark = project
  .settings(moduleName := "tomsopt-benchmark")
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .dependsOn(core)
  .settings(
    // XXX not sure why can't find lib, so needed for now
    javaOptions ++= Seq(
      "-Djava.library.path=" + (baseDirectory in LocalRootProject).value.getAbsolutePath + "/native/target/native/x86_64-darwin/bin"
    )
  )

addCommandAlias("bench", "benchmark/jmh:run -i 4 -wi 4 -f1 EndToEnd")

lazy val root = (project in file("."))
  .aggregate(core, benchmark)
  .settings(commonSettings)

// Clears screen between refreshes in continuous mode
maxErrors := 5
triggeredMessage := Watched.clearWhenTriggered
