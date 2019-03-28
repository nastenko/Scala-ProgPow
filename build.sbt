name := "ProgPow"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++=
  Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.17",
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.56.0.0",
    "org.scalatest" %% "scalatest" % "3.0.+" % "test"
  )