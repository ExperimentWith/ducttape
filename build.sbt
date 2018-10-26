name := "ducttape"
scalaVersion := "2.10.7"
version := "0.4.0-SNAPSHOT"
isSnapshot := true

resolvers += "Clojars" at "https://clojars.org/repo"

libraryDependencies ++= Seq(
  "asm" % "asm" % "3.3.1",
  "commons-io" % "commons-io" % "2.2",
  "org.apache.commons" % "commons-lang3" % "3.3.1",
  "org.clapper" %% "grizzled-slf4j" % "1.0.1",
  "org.eclipse.jetty.aggregate" % "jetty-all" % "8.0.4.v20111024",
  "org.parboiled" % "parboiled-core" % "1.0.2",
  "org.parboiled" % "parboiled-java" % "1.0.2",
  "org.pegdown" % "pegdown" % "1.1.0",
  "com.frugalmechanic" %% "scala-optparse" % "1.1.1",
  "sqlitejdbc" % "sqlitejdbc" % "0.5.6"
)

