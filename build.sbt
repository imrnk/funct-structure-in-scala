name := "funtionalstructure"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-encoding", "UTF-8", // source files are in UTF-8
  "-deprecation", // warn about use of deprecated APIs
  "-unchecked", // warn about unchecked type parameters
  "-feature", // warn about misused language features
  "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
  "-language:implicitConversions",
  "-Xlint", // enable handy linter warnings
  "-Xfatal-warnings", // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)


resolvers ++= Seq("Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
                "bintray/non" at "https://dl.bintray.com/non/maven")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" % "kind-projector_2.12" % "0.9.7")

libraryDependencies ++= Seq("com.github.mpilquist" %% "simulacrum" % "0.16.0", "org.scalatest" %% "scalatest" % "3.0.5" % "test")