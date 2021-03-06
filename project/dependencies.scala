import sbt._

object Dependencies {

  val scalaGitVersion = "4.0"

  val jgitVersionOverride = Option(System.getProperty("jgit.version"))

  val jgitVersion = jgitVersionOverride.getOrElse("5.3.2.201906051522-r")

  val jgit = "org.eclipse.jgit" % "org.eclipse.jgit" % jgitVersion

  // the 1.7.2 here matches slf4j-api in jgit's dependencies
  val slf4jSimple = "org.slf4j" % "slf4j-simple" % "1.7.2"

  val scalaGit = "com.madgag.scala-git" %% "scala-git" % scalaGitVersion exclude("org.eclipse.jgit", "org.eclipse.jgit")

  val scalaGitTest = "com.madgag.scala-git" %% "scala-git-test" % scalaGitVersion

  val scalatest = "org.scalatest" %% "scalatest" % "3.0.4"

  val apache_commons = "org.apache.commons" % "commons-io" % "1.3.2"

  val madgagCompress = "com.madgag" % "util-compress" % "1.33"

  val textmatching = "com.madgag" %% "scala-textmatching" % "2.3"

  val scopt = "com.github.scopt" %% "scopt" % "3.5.0"

  val guava = Seq("com.google.guava" % "guava" % "19.0", "com.google.code.findbugs" % "jsr305" % "2.0.3")

  val scalaIoFile = "com.madgag" %% "scala-io-file" % "0.4.9"

  val useNewerJava =  "com.madgag" % "use-newer-java" % "0.1"

}
