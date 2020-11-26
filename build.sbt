import sbt.Credentials

name := "scala-geo"

organization := "ch.wavein"

//version := "0.2-play24"

scalaVersion := "2.11.12"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.5.18"
libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

lazy val root = (project in file(".")).enablePlugins(
  GitVersioning
).settings(
  git.useGitDescribe := true
)

publishMavenStyle := true

githubOwner := "waveinch"
githubRepository := "scala-geo"

githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_TOKEN")



