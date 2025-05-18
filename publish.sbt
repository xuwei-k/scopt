ThisBuild / organization := "com.github.scopt"
ThisBuild / organizationName := "com.github.scopt"

ThisBuild / description := "a command line options parsing library"

ThisBuild / licenses := List("MIT" -> url("http://www.opensource.org/licenses/mit-license.php"))

val repo = "https://github.com/scopt/scopt"
ThisBuild / scmInfo := Option(ScmInfo(url(repo), s"$repo.git"))
ThisBuild / organizationHomepage := Option(url(repo))
ThisBuild / homepage := Option(url(repo))

ThisBuild / developers ++= List(
  Developer("eed3si9n", "Eugene Yokota", "@eed3si9n", url("https://github.com/eed3si9n")),
)

ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { x =>
  false
}
ThisBuild / publishTo := localStaging.value
