
import math.Ordering

  def msort1(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge1(xs: List[Int], ys: List[Int]): List[Int] =
        xs match {
          case Nil =>
            ys
          case x :: xs1 =>
            ys match {
              case Nil =>
                xs
              case y :: ys1 =>
                if (x < y) x :: merge1(xs1, ys)
                else y :: merge1(xs, ys1)
            }
        }
      val (fst, snd) = xs splitAt n
      merge1(msort1(fst), msort1(snd))
    }
  }



  def msort2(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge2(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
          case (Nil, `ys`) => ys // backticks for suspicious
          case (`xs`, Nil) => xs // shadowing
          case (x :: xs1, y :: ys1) =>
            if (x < y) x :: merge2(xs1, ys)
            else y :: merge2(xs, ys1)
        }
      val (fst, snd) = xs splitAt n
      merge2(msort2(fst), msort2(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  msort2(nums)


def msort3[T](xs: List[T])(lt: (T,T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge3(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, `ys`) => ys
      case (`xs`, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x,y)) x :: merge3(xs1,ys)
        else y :: merge3(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge3(msort3(fst)(lt), msort3(snd)(lt))
  }
}

  msort3(nums)((x: Int, y: Int) => x < y)

  val fruits = List("apple", "pineapple", "orange","banana")
  msort3(fruits)((x: String, y: String) => x.compareTo(y) < 0)

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, `ys`) => ys
        case (`xs`, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(ord), msort(snd)(ord))
    }
  }

msort(nums)(Ordering.Int)
msort(fruits)(Ordering.String)
msort(fruits)
