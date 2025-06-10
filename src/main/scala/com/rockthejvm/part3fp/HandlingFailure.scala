package com.rockthejvm.part3fp

import scala.util.{Random, Try}

class HandlingFailure

/**
 * Exercise:
 * obtain a connection,
 * then fetch the url,
 * then print the resulting HTML
 */
val host = "localhost"
val port = "8081"
val myDesiredURL = "rockthejvm.com/home"

class Connection {
  val random = new Random()

  def get(url: String): String = {
    if (random.nextBoolean()) "<html>Success</html>"
    else throw new RuntimeException("Cannot fetch page right now.")
  }

  def getSafe(url: String): Try[String] =
    Try(get(url))
}

object HttpService {
  val random = new Random()

  def getConnection(host: String, port: String): Connection =
    if (random.nextBoolean()) new Connection
    else throw new RuntimeException("Cannot access host/port combination.")

  def getConnectionSafe(host: String, port: String): Try[Connection] =
    Try(getConnection(host, port))
}

// defensive style
val finalHtml = try {
  val conn = HttpService.getConnection(host, port)
  val html = try {
    conn.get(myDesiredURL)
  } catch {
    case e: RuntimeException => s"<html>${e.getMessage}</html>"
  }
} catch {
  case e: RuntimeException => s"<html>${e.getMessage}</html>"
}

val finalHtmlBetter = for{
  httpSvc <- HttpService.getConnectionSafe(host, port)
  content <- httpSvc.getSafe(myDesiredURL)
}yield content

object HandlingFailure {
  def main(args: Array[String]): Unit = {
    println(finalHtmlBetter.orElse(Try[String]("<html>Failure</html>")).get)
  }
}
