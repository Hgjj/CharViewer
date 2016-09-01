enablePlugins(ScalaJSPlugin)


name := "charecter viewer root project"

lazy val root = project.in(file(".")).
  aggregate(cVJS, cVJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val cV = crossProject.in(file(".")).
  settings(
    name := "charecter viewer",
    scalaVersion := "2.11.7"
  ).
  jvmSettings().
  jsSettings(

  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2",
	libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0",

	jsDependencies += RuntimeDOM,

	skip in packageJSDependencies := false,
	persistLauncher in Compile := true,
	persistLauncher in Test := true,
  
	scalaJSSemantics ~= { _.withAsInstanceOfs(
  org.scalajs.core.tools.sem.CheckedBehavior.Compliant) }
  )
 persistLauncher in Compile := true
 persistLauncher in Test := true

lazy val cVJVM = cV.jvm
lazy val cVJS = cV.js


