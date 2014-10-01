name := "DicomStack"

organization := "net.liftweb"

version := "0.0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "dcm4che"   at "http://www.dcm4che.org/maven2")

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

//javaOptions ++= Seq("-javaagent:/Users/anton/dev/jrebel/jrebel.jar")

jetty(port = 8001)

libraryDependencies ++= {
  val liftVersion = "2.6-RC1"
  Seq( "net.liftweb"       %% "lift-webkit"         % liftVersion        % "compile"
     , "net.liftweb"       %% "lift-json"           % liftVersion
     , "org.eclipse.jetty" % "jetty-webapp"         % "8.1.7.v20120910"  % "container,test"
     , "org.eclipse.jetty.orbit" % "javax.servlet"  % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
     , "ch.qos.logback"    % "logback-classic"      % "1.0.6"
     , "org.specs2"        %% "specs2"              % "2.3.12"           % "test"
     , "org.dcm4che"       % "dcm4che-core"         % "3.3.3"
     , "org.dcm4che"       % "dcm4che-image"        % "3.3.3"
     , "org.dcm4che"       % "dcm4che-imageio"      % "3.3.3"
     , "org.dcm4che"       % "dcm4che-imageio-rle"  % "3.3.3"
     , "com.sun.media"     % "jai_imageio"          % "1.2-pre-dr-b04"
     , "org.postgresql"    % "postgresql"           % "9.3-1100-jdbc41"
     , "com.amazonaws"     % "aws-java-sdk"         % "1.7.1"
     , "joda-time"         % "joda-time"            % "2.3"
     , "com.typesafe.slick" %% "slick"              % "2.1.0-M2"
     , "org.flywaydb"      % "flyway-core"          % "3.0"
     , "com.zaxxer"        % "HikariCP-java6"       % "2.0.1"
  )
}
