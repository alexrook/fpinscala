package scalaConcurrency.ch3

object ExecutirExample1 extends App {

  def log(v: String) = println(v)

  def execute(body: => Unit) =
    scala.concurrent.ExecutionContext.global.execute(new Runnable {
      override def run() = body
    })

  for (i <- 0 until 32) execute {
    Thread.sleep(2000)
    log(s"Task $i completed." + Thread.currentThread().getName())
  }

  Thread.sleep(10000)
}
