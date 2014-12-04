import sbt._
import Keys._

import java.io.File

object ExerciseBuild extends Build {

  val exerciseSettings = Defaults.defaultSettings ++ Seq(
    organization := "fos",
    name         := "fos-project6",
    version      := "1.0",
    scalaVersion := "2.10.4",
    scalacOptions ++= List("-unchecked", "-deprecation"))

  val filesToInclude = Seq("FJ", "Tree", "Type").map("src/main/scala/fos/" + _ + ".scala")

  val packageToMoodle = InputKey[Unit]("package-for-submission", "Package all files necessary for submission, given your surnames. For example 'Plociniczak Jovanovic'")

  val packageToMoodleTask: Setting[InputTask[Unit]] = packageToMoodle <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
    (argTask, baseDirectory, target, packageOptions, cacheDirectory, streams, compile in Compile) map { (args: Seq[String], base, outDir, po, cDir, s, comp) =>

      if (args.length > 3 || args.length < 1)
        println("Invalid number of students in a group. Please provide your last names separated by a whitespace")
      else {
        val suffix = args.map(name => name.slice(0,3)).mkString
        val out = new File(outDir, "submission" + suffix + ".jar")
        gatherSources(out, po, cDir, s.log)
      }

    }
  }

  lazy val root = Project("fos-project6", file(".")).settings((exerciseSettings ++ Seq(packageToMoodleTask)):_*)


  private def gatherSources(out: File, po: Seq[PackageOption], cacheDir: File, log: Logger): File = {
    val packagePrefix = "src/fos"
    val mappings = filesToInclude.flatMap{src =>
      val f = new File(src)

      if (f.exists()) {
        Some((f, packagePrefix + "/" + f.getName))
      }  else {
        println("Required file " + src + " does not exist!")
        None
      }
    }

    if (filesToInclude.length != mappings.length) {
      out
    } else {
      // println info for students
      println("Files to be included in the submission: '" + filesToInclude.mkString(",") + "'")
      val config = new Package.Configuration(mappings, out, po)
      Package(config, cacheDir, log)
      out
    }

  }



}
