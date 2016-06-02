
lazy val commonSettings = Seq(
  name := "TomsOpt",
  version := "0.5",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "com.github.scopt" %% "scopt" % "3.4.0",
    "com.github.aishfenton" %% "vegas" % "0.2.4"
  ),
  scalacOptions += "-optimize",
  javaOptions in run += "-Xmx8G"
)

lazy val core = project
  .settings(commonSettings)

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

addCommandAlias("bench", "benchmark/jmh:run -i 10 -wi 10 -f1")
