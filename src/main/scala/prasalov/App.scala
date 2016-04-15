package prasalov

/**
 * Created by kirillprasalov on 07.04.16.
 */
object App {

  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5)

    println(stream.toList)
    println(stream.take(3))
    println(stream.take(10))
    println(stream.take(0))
    println(stream.drop(0).toList)
    println(stream.drop(3).toList)
    println(stream.drop(10))
    println(stream.takeWhile(_ < 3).toList)
    println(stream.takeWhile(_ < 10).toList)
    println(stream.takeWhile(_ < 0))
  }
}
