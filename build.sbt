name := "joos1w"

version := "1.0"

scalaVersion := "2.12.8"

// don't compile Java
unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "." + artifact.extension
}

crossTarget := baseDirectory.value

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"

