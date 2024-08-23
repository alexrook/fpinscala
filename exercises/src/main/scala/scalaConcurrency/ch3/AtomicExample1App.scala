package scalaConcurrency.ch3

import java.util.concurrent.atomic._

object AtomicExample1App extends App {

  def log(v: String) = println(v)

  def execute(body: => Unit) =
    scala.concurrent.ExecutionContext.global.execute(new Runnable {
      override def run() = body
    })

  private val uid = new AtomicLong(0L)
  def getUniqueId(): Long = uid.incrementAndGet()
  execute { log(s"Uid asynchronously: ${getUniqueId()}") }
  log(s"Got a unique id: ${getUniqueId()}")
}
