
inThisBuild(
	List(
        //name := "projectName",
        version := "0.1.0",
		scalaVersion := "3.4.1",
        cancelable in Global := true,

        libraryDependencies ++= Seq(
            "org.scalatest" %% "scalatest" % "3.2.9" % Test,
        ),
	)
)
