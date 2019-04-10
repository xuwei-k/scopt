import Dependencies._
import com.typesafe.sbt.pgp.PgpKeys._

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{ crossProject, CrossType }

def v: String = "4.0.0-SNAPSHOT"

ThisBuild / version := v
ThisBuild / scalaVersion := scala212
ThisBuild / crossScalaVersions := Seq(scala211, scala210, scala212, scala213)
ThisBuild / scalafmtOnCompile := true

lazy val root = (project in file("."))
  .aggregate(scoptJS, scoptJVM, scoptNative)
  .settings(
    name := "scopt root",
    publish / skip := true
  )

lazy val scopt = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("."))
  .settings(
    name := "scopt",
    // site
    // to preview, preview-site
    // to push, ghpages-push-site
    siteSubdirName in SiteScaladoc := "$v/api",
    git.remoteRepo := "git@github.com:scopt/scopt.git",
    scalacOptions ++= Seq("-language:existentials", "-Xfuture", "-deprecation"),
    resolvers += "sonatype-public" at "https://oss.sonatype.org/content/repositories/public",
    libraryDependencies ++= Seq(
      "com.eed3si9n.expecty" %%% "expecty" % "0.11.0" % Test,
    ),
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    // scaladoc fix
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
  )
  .platformsSettings(JVMPlatform, JSPlatform)(
    Seq(Compile, Test).map { x =>
      unmanagedSourceDirectories in x += {
        baseDirectory.value.getParentFile / s"jvm_js/src/${Defaults.nameForSrc(x.name)}/scala/"
      }
    }
  )
  .jsSettings(
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    scalacOptions += {
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/scopt/scopt/" + sys.process
        .Process("git rev-parse HEAD")
        .lineStream_!
        .head
      s"-P:scalajs:mapSourceURI:$a->$g/"
    },
  )
  .nativeSettings(
    scalaVersion := scala211,
    crossScalaVersions := Nil
  )

lazy val scoptJS = scopt.js

lazy val scoptJVM = scopt.jvm
  .enablePlugins(SiteScaladocPlugin)

lazy val scoptNative = scopt.native
