name := "SureStartup"

organization := "com.kdkvsk"

version := "0.0.10"

scalaVersion in ThisBuild := "2.11.5"

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

seq(webSettings :_*)

libraryDependencies ++= {
  val liftVersion = "2.6"
  Seq( "net.liftweb"             %% "lift-webkit"             % liftVersion       % "compile"
     , "net.liftweb"             %% "lift-json"               % liftVersion       % "compile"
     , "net.liftmodules"         %% ("omniauth_"+liftVersion) % "0.17"            % "compile"
     , "ch.qos.logback"          % "logback-classic"          % "1.1.2"           % "compile"
     , "org.postgresql"          % "postgresql"               % "9.3-1102-jdbc41" % "compile"
     , "com.amazonaws"           % "aws-java-sdk"             % "1.9.16"          % "compile"
     , "joda-time"               % "joda-time"                % "2.7"             % "compile"
     , "com.typesafe.slick"      %% "slick"                   % "2.1.0"           % "compile"
     , "com.typesafe.slick"      %% "slick-codegen"           % "2.1.0"           % "compile"
     , "org.flywaydb"            % "flyway-core"              % "3.1"             % "compile"
     , "com.zaxxer"              % "HikariCP-java6"           % "2.3.1"           % "compile"
     , "org.imgscalr"            % "imgscalr-lib"             % "4.2"             % "compile"
     , "org.eclipse.jetty"       % "jetty-webapp"             % "8.1.7.v20120910" % "container,test"
     , "org.eclipse.jetty"       % "jetty-plus"               % "8.1.7.v20120910" % "container,test"
     , "org.scalatest"           %% "scalatest"               % "2.2.1"           % "test"
     )
}
