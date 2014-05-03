name := "butler"

version := "1.0-SNAPSHOT"

resolvers += "Sonatype OSS releases, snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "ws.securesocial" %% "securesocial" % "master-SNAPSHOT",
  "org.squeryl" % "squeryl_2.10" % "0.9.5-6",
  "mysql" % "mysql-connector-java" % "5.1.27",
  "org.scalatest" %% "scalatest" % "2.1.0" % "test",
  "com.evernote" % "evernote-api" % "1.25.1",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "io.spray" %%  "spray-json" % "1.2.6"
)     
             

play.Project.playScalaSettings
