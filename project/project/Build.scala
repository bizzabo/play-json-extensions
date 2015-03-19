import sbt._
import Keys._
object BuildBuild extends Build{
  override lazy val settings = super.settings ++ Seq(
    scalacOptions ++= Seq(
      "-feature", "-deprecation", "-unchecked"
    )
  )
}
