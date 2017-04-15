/**
  * Created by gilcu2 on 15/04/2017.
  */

import org.scalatest._
import com.gilcu2.fpbook.chap5._

class Chap5Test extends FlatSpec with Matchers {

  "Stream" should "get first elements" in {
    val r = Stream(1, 2, 3).take(2).toList
    r should be(List(1, 2))
  }

  "Stream" should "generate fibonaccy" in {
    val r = Stream.fibs.take(5).toList
    r should be(List(0, 1, 1, 2, 3))
  }

}