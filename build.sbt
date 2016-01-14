name := """impatient"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.13"
libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.3.11"
// libraryDependencies += "com.typesafe.akka" %% "akka-stm" % "2.3.11"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.11"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.11" % "test"
libraryDependencies += "junit" % "junit" % "4.12"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5"

//javaOptions ++= Seq(
//"-Dconfig.file=conf/application.conf",
//"-Denv=dev"
//)
