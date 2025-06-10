package com.rockthejvm.part3fp

import scala.util.Random

object Options {

  /**
   * Exercise:
   * Get the host and port from the config map,
   * try to open a connection,
   * print "Conn successful"
   * or "Conn failed"
   */

  val config: Map[String, String] = Map(
    // comes from elsewhere
    "host" -> "176.45.32.1",
    "port" -> "8081"
  )

  class Connection {
    def connect(): String = "Connection successful"
  }

  object Connection {
    val random = new Random()

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }
  // defensive style (in an imperative language e.g. Java)
  /*
    String host = config("host")
    String port = config("port")
    if (host != null)
      if (port != null)
        Connection conn = Connection.apply(host, port)
        if (conn != null)
          return conn.connect()
        // ... that's just the happy path, we need to add the rest of the branches
   */

  // options style
  val host = config.get("host")
  val port = config.get("port")
  val connection = host.flatMap(h => port.flatMap(p => Connection(h, p)))
  val connStatus = connection.map(_.connect())

  // compact
  val connStatus_v2 =
    config.get("host").flatMap(h =>
      config.get("port").flatMap(p =>
        Connection(h, p).map(_.connect())
      )
    )

  // for-comprehension
  val connStatus_v3 = for {
    h <- config.get("host")
    p <- config.get("port")
    conn <- Connection(h, p)
  } yield conn.connect()

  def main(args: Array[String]): Unit = {
    println(connStatus.getOrElse("Failed to establish connection"))
  }

}
