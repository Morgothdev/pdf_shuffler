import AssemblyKeys._

assemblySettings

jarName in assembly := "shuffler.jar"

mainClass in assembly := Some("consoleApp.Runner")

name := "shuffler"

version := "1.0"

scalaVersion := "2.10.3"

resolvers += Resolver.sonatypeRepo("public")

resolvers += "Maven Central Server" at "http://repo1.maven.org/maven2"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.8.4"
