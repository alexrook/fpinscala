package scalaConcurrency

import java.util.concurrent.atomic.AtomicReference
import java.sql.Time
import scala.runtime.BoxedUnit

object ParApp extends App {

  def thread(body: => Unit)(isDaemon: Boolean, name: String) = {
    val t = new Thread() {
      override def run(): Unit = body
    }
    t.setDaemon(isDaemon)
    t.setName(name)
    t.start()
    t
  }

  /** Реализуйте метод parallel, принимающий два блока кода, a и b, и запускаю-
    * щий их в отдельных потоках. Метод должен вернуть кортеж со значениями,
    * вычисленными обоими блоками, и иметь следующую сигнатуру:
    */
  def parallel[A, B](a: => A, b: => B): (A, B) = {

    var aRef = new AtomicReference[A]
    var bRef = new AtomicReference[B]

    val t1 = thread {
      aRef.set(a)
    }(true, "ex1-t1")

    val t2 = thread {
      bRef.set(b)
    }(true, "ex1-t2")

    t1.join()
    t2.join()

    aRef.get() -> bRef.get()

  }

  // Test
  // val ret =
  //   parallel(
  //     a = {
  //       Thread.sleep(100)
  //       12
  //     },
  //     b = {
  //       Thread.sleep(100)
  //       12
  //     }
  //   )
  // println(ret)

  /** Реализуйте метод periodically, принимающий интервал времени duration в
    * миллисекундах и вычисляющий блок b. Метод должен запускать поток,
    * выполняющий блок b каждые duration миллисекунд
    * @note:
    *   Это намереренная попытка реализовать функционал используя только базовые
    *   примитивы
    */
  def periodically(duration: Long)(b: => Unit): () => Unit = {

    class Box(var v: Long)
    var timer = new Box(0)
    @volatile var isDone = false

    def runner: Unit =
      thread {
        timer.synchronized {
          while (true && !isDone) {
            b
            timer.v = duration
            while (timer.v > 0 && !isDone) {
              println(s"timer;${timer.v}, isDone[$isDone]")
              timer.wait(duration)
            }
          }
        }
      }(false, "ex2-runner")

    def tick: Thread =
      thread {
        timer.synchronized {
          while (true && !isDone) {
            timer.v = timer.v - 1
            if (timer.v <= 0) {
              timer.notify
            }
            timer.wait(1)
          }
        }
      }(false, "ex2-tick")

    def done = {
      isDone = true
      timer.synchronized {
        timer.notifyAll
      }
    }

    runner
    tick

    () => done
  }

  // Test
  thread {
    val f: () => Unit = periodically(1000)(println(System.currentTimeMillis()))
    Thread.sleep(10000)
    f()
  }(false, "periodically-tester")

  // Exit
  println("Done")

}
