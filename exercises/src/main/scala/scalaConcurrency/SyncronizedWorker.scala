package scalaConcurrency

import scala.collection.mutable

object SynchronizedPool extends App {

  def log(v: String): Unit = {
    println(v)
  }

  private val tasks = mutable.Queue[() => Unit]()
  object Worker extends Thread {
    setDaemon(true)

    /** poll захватывает монитор для task, проверяет, что очередь пуста и
      * переходит в состояние ожидания освобождая монитор. Цикл while нужен для
      * избежания спонтанных пробуждений, когда JVM может сделать условный
      * tasks.notify, и при этом, очередь все также пуста.
      */
    def poll() =
      tasks.synchronized {
        while (tasks.isEmpty) {
          tasks.wait()
        }
        tasks.dequeue()
      }
    override def run() = while (true) {
      val task = poll()
      task()
    }
  }

  Worker.start()

  /** для вызова notify/wait на объекте нужно владеть его монитором
    */
  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify() // извещаем worker о новых данных
  }
  asynchronous { log("Hello ") }
  asynchronous { log("World!") }
  Thread.sleep(500)
}
