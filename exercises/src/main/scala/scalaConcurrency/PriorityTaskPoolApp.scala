package scalaConcurrency

import scala.collection.mutable.PriorityQueue
import scalaConcurrency.FinalInit.t
import scala.annotation.tailrec

object PriorityTaskPoolApp extends App {

  def log(v: String) = println(v)

  def thread(body: => Unit)(name: String): Thread = {
    val t = new Thread() {
      override def run(): Unit = body
    }
    t.setName(name)
    t.start()
    t
  }

  /** Реализуйте класс PriorityTaskPool с методом asynchronous. Единственный
    * рабочий поток выбирает задания из очереди и выполняет их. Извлекаемое
    * задание должно иметь высший приоритет из имеющихся в очереди
    *
    * Дополните класс PriorityTaskPool из предыдущего упражнения так, чтобы он
    * поддерживал любое количество p рабочих потоков. Параметр p определяет- ся
    * в конструкторе класса PriorityTaskPool
    *
    * Дополните класс PriorityTaskPool из предыдущего упражнения так, чтобы он
    * поддерживал метод shutdown. Метод должен дождаться выполнения всех заданий
    * с приоритетом выше important, а остальные задания должны быть удалены.
    * Целочисленный параметр important определяется в конструкторе класса
    * PriorityTaskPool. //TODO
    */

  class PriorityTaskPool(threads: Int) {

    case class Entry(priority: Int, runTask: () => Unit)

    class Counter(var c: Int)

    implicit val entryOrd: Ordering[Entry] = Ordering.by { entry: Entry =>
      entry.priority
    }

    val tasks = PriorityQueue.empty[Entry]

    val counter = new Counter(threads)

    def asynchronous(priority: Int)(task: => Unit): Unit =
      tasks.synchronized {
        tasks.enqueue(Entry(priority, () => task))
        tasks.notifyAll
      }

    def mkThread(entry: Entry) =
      counter.synchronized {
        if (counter.c > 0) {
          val id = counter.c
          counter.c -= 1
          thread {
            log(s"run task with priority[${entry.priority}]")
            entry.runTask()
            counter.synchronized {
              counter.c += 1
              counter.notifyAll
            }
          }(s"task-runner-$id")
        } else {
          counter.wait
        }
      }

    final def run: Unit =
      while (true) {
        tasks.synchronized {
          if (tasks.nonEmpty) {
            val next = tasks.dequeue()
            mkThread(entry = next)
          } else {
            tasks.wait
          }
        }
      }

  }

  val pool = new PriorityTaskPool(10)

  val t1: Thread =
    (for (x <- 0 until 500) yield {
      thread {
        pool.asynchronous(1000) {
          println(s"Priority 1000, $x " + Thread.currentThread().getName())
        }

        pool.asynchronous(1) {
          println(s"Priority 1 $x" + Thread.currentThread().getName())
        }

        pool.asynchronous(100) {
          println(s"Priority 100 $x" + Thread.currentThread().getName())
        }

        log("All tasks enqueued")
      }(s"task-producer-$x")
    }).fold(thread(println("Done"))("zero")) {
      case (left: Thread, right: Thread) =>
        left.join
        right.join
        left
    }

  t1.join

  println("Producer joined")

  pool.run

  println("Done")

}
