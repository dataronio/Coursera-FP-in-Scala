def scaleList1(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList1(ys, factor)
}

def scaleList(xs: List[Double], factor: Double) =
  xs map (x => x * factor)

def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList1(ys)
}

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)

val nums = List(2, -4, 5, 7, 1)
squareList1(nums)
squareList(nums)

def posElems1(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems1(ys) else posElems1(ys)
}

def posElems(xs: List[Int]): List[Int] =
  xs filter (x => x > 0)

posElems1(nums)
posElems(nums)
