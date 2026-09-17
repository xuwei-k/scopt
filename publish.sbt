ThisBuild / organization := "com.github.scopt"
ThisBuild / organizationName := "com.github.scopt"

ThisBuild / description := "a command line options parsing library"

ThisBuild / licenses := List("MIT" -> uri("http://www.opensource.org/licenses/mit-license.php"))

val repo = "https://github.com/scopt/scopt"
ThisBuild / scmInfo := Option(ScmInfo(uri(repo), s"$repo.git"))
ThisBuild / organizationHomepage := Option(uri(repo))
ThisBuild / homepage := Option(uri(repo))

ThisBuild / developers ++= List(
  Developer("eed3si9n", "Eugene Yokota", "@eed3si9n", uri("https://github.com/eed3si9n")),
)

ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { x =>
  false
}
ThisBuild / publishTo := localStaging.value
