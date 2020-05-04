ThisBuild / scalaVersion := "2.13.2"
organization := "Sledge"

lazy val root = project.in(file(".")).
  aggregate(dataPack.js, dataPack.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val dataPack = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "Scala DataPack",
    version := "0.6-SNAPSHOT",
    scalacOptions += "-deprecation",
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := false,
  )
