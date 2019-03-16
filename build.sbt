name := "epl"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies ++= Seq(
  "org.antlr" % "antlr4" % "4.7.2",
  "org.antlr" % "antlr4-runtime" % "4.7.2",
  "org.antlr" % "stringtemplate" % "3.2"
)

enablePlugins(Antlr4Plugin)

compileOrder := CompileOrder.JavaThenScala

antlr4PackageName in Antlr4 := Some("edu.nus.comp.pli.epl.parser")