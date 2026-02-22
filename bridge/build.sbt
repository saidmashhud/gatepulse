ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "io.hookline"

lazy val root = (project in file("."))
  .settings(
    name := "hookline-bridge",
    libraryDependencies ++= Seq(
      // ZIO core
      "dev.zio" %% "zio"                     % "2.1.9",
      "dev.zio" %% "zio-streams"             % "2.1.9",
      // Kafka
      "dev.zio" %% "zio-kafka"               % "2.7.4",
      // HTTP client
      "dev.zio" %% "zio-http"                % "3.0.1",
      // JSON
      "dev.zio" %% "zio-json"                % "0.7.3",
      // Logging
      "dev.zio" %% "zio-logging"             % "2.3.2",
      "dev.zio" %% "zio-logging-slf4j2"      % "2.3.2",
      "org.slf4j"  % "slf4j-simple"          % "2.0.16",
      // Test
      "dev.zio" %% "zio-test"                % "2.1.9" % Test,
      "dev.zio" %% "zio-test-sbt"            % "2.1.9" % Test,
      "dev.zio" %% "zio-kafka-testkit"       % "2.7.4" % Test,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    assembly / mainClass := Some("io.hookline.bridge.Main"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF")          => MergeStrategy.discard
      case PathList("META-INF", "services", xs @ _*)    => MergeStrategy.concat
      case PathList("META-INF", xs @ _*)                => MergeStrategy.discard
      case PathList("reference.conf")                   => MergeStrategy.concat
      case _                                            => MergeStrategy.first
    }
  )
