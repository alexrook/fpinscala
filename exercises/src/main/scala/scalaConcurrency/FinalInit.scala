package scalaConcurrency

object FinalInit extends App {

  def thread(body: => Unit): Thread = {
    val t = new Thread() {
      override def run(): Unit = body
    }
    t.setDaemon(true)
    t.start()
    t
  }

  var inc: () => Unit = null

  val t = thread {
    if (inc != null) {
      println("Doing increment")
      inc()
    } else {
      println("steel null")
    }
  }

  private var number = 1

  inc = () => { number += 1 }

  Thread.sleep(300)

  println(number)

}
