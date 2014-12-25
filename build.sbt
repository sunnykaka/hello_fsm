name := "hello_fsm"

version := "1.0"

scalaVersion := "2.11.4"

resolvers += "Twitter" at "http://maven.twttr.com"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.5",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.5",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.5",
  "com.twitter" %% "util-core" % "6.23.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.13"

