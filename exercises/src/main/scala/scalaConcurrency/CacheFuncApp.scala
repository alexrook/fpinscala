package scalaConcurrency

import scala.util.Random

import scala.collection.mutable.Map

object CacheFuncApp extends App {

  def thread(body: => Unit)(name: String): Thread = {
    val t = new Thread() {
      override def run(): Unit = body
    }
    t.setName(name)
    t.start()
    t
  }

  /** Реализуйте метод cache, преобразующий любые функции в их мемоизованные.
    * Когда полученная функция вызывается первый раз, она действует в точности
    * как оригинальная версия, но запоминает результат, и последующий вызов
    * функции с тем же аргументом должен вернуть предыдущее значение из кэша:
    * Реализация должна корректно работать при использовании мемоизованной
    * версии функции из нескольких потоков одновременно.
    */
  def cache[K, V](f: K => V): K => V = {

    val rets = Map.empty[K, V]

    (k: K) => {
      rets.synchronized {
        if (rets.contains(k)) {
          val v = rets(k)
          println(
            s"Ret[$v] is non empty for k[$k], class[${k.getClass().getName()}]"
          )
          v
        } else {
          val v = f(k)
          rets.put(k, v)
          v
        }
      }

    }

  }

  val f1: Int => String = cache((x: Int) => x + " one")

  val f2: String => String = cache((x: String) => x + " two")

  val f3: Long => String =
    cache((x: Long) => System.currentTimeMillis + " three")

  def mkThreads: Thread = {
    val id = Random.nextInt()
    Random
      .shuffle((0 until 10).toList)
      .map {
        case x: Int if x % 2 == 0 =>
          thread(println(f1(x)))(s"thread-$x-$id")
        case x: Int if x % 3 == 0 =>
          thread(println(f2(x.toString())))(s"thread-$x-$id")
        case x: Int =>
          thread(println(f3(x.toLong)))(s"thread-$x-$id")
      }
      .fold(thread(())(s"zero-$id")) { case (left, right) =>
        left.join()
        right.join()
        left
      }
  }

  val threads1 = mkThreads
  val threads2 = mkThreads

  threads1.join()
  threads2.join()

  println("Done")
}
