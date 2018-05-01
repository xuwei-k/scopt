import Dependencies._
import com.typesafe.sbt.pgp.PgpKeys._

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

def v: String = "3.7.0"

lazy val root = project.in(file(".")).
  aggregate(scoptJS, scoptJVM, scoptNative).
  settings(
    publish := {},
    publishLocal := {},
    skip in publish := true)

lazy val scopt = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file(".")).
  settings(
    inThisBuild(Seq(
      version := v,
      organization := "com.github.scopt",
      scalaVersion := scala212,
      crossScalaVersions := Seq(scala211, scala210, scala212, scala213),
      homepage := Some(url("https://github.com/scopt/scopt")),
      licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
    )),
    name := "scopt",
    // site
    // to preview, preview-site
    // to push, ghpages-push-site
    siteSubdirName in SiteScaladoc := "$v/api",
    git.remoteRepo := "git@github.com:scopt/scopt.git",
    description := """a command line options parsing library""",
    scalacOptions ++= Seq("-language:existentials", "-Xfuture", "-deprecation"),
    resolvers += "sonatype-public" at "https://oss.sonatype.org/content/repositories/public",
    // scaladoc fix
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies += specs2.value % Test,
    libraryDependencies ++= parserCombinators.value
  ).
  jsSettings(
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    scalacOptions += {
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/scopt/scopt/" + sys.process.Process("git rev-parse HEAD").lineStream_!.head
      s"-P:scalajs:mapSourceURI:$a->$g/"
    },
    sources in Test := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) =>
          // specs 4.x does not support scala 2.10
          // specs 3.x does not support scala-js
          Nil
        case _ =>
          (sources in Test).value
      }
    }
  ).
  nativeSettings(
    sources in Test := Nil, // https://github.com/etorreborre/specs2/issues/591
    scalaVersion := scala211,
    crossScalaVersions := Nil
  )

lazy val scoptJS = scopt.js
lazy val scoptJVM = scopt.jvm.enablePlugins(SiteScaladocPlugin)
lazy val scoptNative = scopt.native

lazy val nativeTest = project.in(file("nativeTest")).
  dependsOn(scoptNative).
  enablePlugins(ScalaNativePlugin).
  settings(scalaVersion := scala211)
