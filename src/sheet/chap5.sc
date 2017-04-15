import com.gilcu2.fpbook.chap5._

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList

Stream.fibs.take(4).toList