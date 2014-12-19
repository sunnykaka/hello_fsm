name := "hello_fsm"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.5",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.5",
  "com.twitter" %% "util-core" % "6.12.1"
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.13"


