package scalaConcurrency

object ExerciseSyncVarApp extends App {

  def log(v: String): Unit = println(v)

  /** Реализуйте класс SyncVar. Объекты SyncVar используются для обмена
    * значениями между несколькими потоками. Сразу после создания объект SyncVar
    * пуст:  вызов get возбуждает исключение;  вызов put добавляет значение
    * в объект SyncVar. После добавления значения в объект SyncVar мы говорим,
    * что он не пуст:  вызов get возвращает текущее значение и переводит
    * объект в состояние «пустой»;  вызов put возбуждает исключение.
    */
  class SyncVar[T] {

    class Holder[T](var value: Option[T])
    var boxed = new Holder[T](None)

    def isEmpty = {

      boxed.synchronized {
        boxed.value.isEmpty
      }
    }

    def nonEmpty = {
      !isEmpty
    }

    def get(): T =
      boxed.synchronized {
        boxed.value match {
          case None => throw new IllegalAccessException("The SyncVar is empty")
          case Some(v) =>
            boxed.value = None
            v
        }
      }

    def put(x: T): Unit =
      boxed.synchronized {
        boxed.value match {
          case None =>
            log(s"Doing put:$x")
            boxed.value = Some(x)
          case Some(v) =>
            throw new IllegalAccessException("The SyncVar is non empty")
        }
      }

    def getWait(): T =
      boxed.synchronized {
        boxed.value match {
          case None =>
            boxed.wait()
            getWait()

          case Some(v) =>
            boxed.value = None
            boxed.notifyAll
            v
        }
      }

    def putWait(x: T): Unit =
      boxed.synchronized {
        boxed.value match {
          case None =>
            boxed.value = Some(x)
            boxed.notifyAll

          case Some(v) =>
            boxed.wait
            putWait(x)
        }
      }
  }

}
