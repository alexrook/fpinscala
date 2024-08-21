package scalaConcurrency

import scala.collection.mutable

object ExerciseSyncQueueApp extends App {

  def thread(body: => Unit): Thread = {
    val t = new Thread() {
      override def run(): Unit = body
    }
    t.start()
    t
  }

  /** Объект SyncVar может хранить только одно значение. Реализуйте класс
    * SyncQueue, имеющий такой же интерфейс, как класс SyncVar, но способный
    * хранить до n значений. Параметр n определяется в конструкторе класса
    * SyncQueue.
    */
  class SyncQueue[T](size: Int) {

    val buffer = mutable.Queue.empty[T]

    def getWait(): T =
      buffer.synchronized {
        if (buffer.nonEmpty) {
          val a = buffer.dequeue()
          buffer.notifyAll
          a
        } else {
          buffer.wait
          getWait()
        }
      }

    def putWait(v: T): Unit =
      buffer.synchronized {
        if (buffer.size < size) {
          buffer.enqueue(v)
          buffer.notifyAll
        } else {
          buffer.wait
          putWait(v)
        }
      }
  }

  val shared = new SyncQueue[Int](10)

  val producer =
    thread {
      for (x <- 0 to 15) {
        shared.putWait(x)
      }
    }

  val consumer =
    thread {
      var c = 0
      while (c < 15) {
        c = shared.getWait()
        println(s"Consumer get[$c]")
      }
    }

  consumer.join()

}
