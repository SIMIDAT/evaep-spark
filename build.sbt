lazy val root = (project in file(".")).
  settings(
    name := "evaep-spark",
    version := "1.0",
    scalaVersion := "2.10.6",
    mainClass in Compile := Some("evaep.Main")
  )

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "2.0.2" % "provided"
)

// META-INF discarding
