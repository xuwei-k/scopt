name := "scopt"

version := "3.0.0-SNAPSHOT"

organization := "com.github.scopt"

homepage := Some(url("https://github.com/scopt/scopt"))

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

description := """a command line options parsing library"""

scalaVersion := "2.10.1"

crossScalaVersions := Seq("2.10.1", "2.9.2", "2.9.3")

crossVersion <<= scalaVersion { sv =>
  ("-(M|RC)".r findFirstIn sv) map {_ => CrossVersion.full} getOrElse CrossVersion.binary
}

// scalatest
libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
	val versionMap = Map("2.9.3" -> "1.12.5-SNAPSHOT", "2.9.2" -> "1.12.3", "2.10.1" -> "2.0-RC1")
	val testVersion = versionMap.getOrElse(sv, error("Unsupported Scala version " + sv))
	deps :+ ("org.specs2" %% "specs2" % testVersion % "test")
}

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/repositories/public"

seq(lsSettings :_*)

LsKeys.tags in LsKeys.lsync := Seq("cli", "command-line", "parsing", "parser")

externalResolvers in LsKeys.lsync := Seq(
  "sonatype-public" at "https://oss.sonatype.org/content/repositories/public")

// scaladoc fix
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

pomExtra :=
  (<scm>
    <url>git@github.com:scopt/scopt.git</url>
    <connection>scm:git:git@github.com:scopt/scopt.git</connection>
  </scm>
  <developers>
    <developer>
      <id>eed3si9n</id>
      <name>Eugene Yokota</name>
      <url>http://eed3si9n.com</url>
    </developer>
  </developers>)

// --- Sonatype settings ---

publishMavenStyle := true

publishArtifact in (Compile, packageBin) := true

publishArtifact in Test := false

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { x => false }

// site

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:scopt/scopt.git"
