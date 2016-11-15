

val xs = Array(1,2,3,44)
xs map (x => x * x)

val s = "Hello World"
s filter (c => c.isUpper)

val ra: Range = 1 until 5
val st: Range = 1 to 5
1 to 10 by 3
6 to 1 by -2

s exists ( c => c.isUpper)
s forall ( c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List(' ', c))
s flatMap (c => List('.', c))

ra.sum
ra.product
ra.max
ra.min

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

def Combo(m: Int, n: Int) =
  (1 to m) flatMap (x => (1 to n) map (y => (x,y)))

def isPrime(n: Int): Boolean = (2 to n/2) forall (x => n % x != 0)

isPrime(7)

isPrime(10)

val n = 7
(1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (pair =>
  isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct1(xs: List[Double],ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum

val fruit = Set("apple", "banana", "pear", "pineapple")
val t = (1 to 6).toSet

t map (_ + 2)
//fruit filter ( _.startsWith == "app")
t.nonEmpty

t map(_ / 2)

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if(k == 0) Set(List())
  else
    for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens
    placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col-c) != row - r //row is greater than r-don't need abs
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) map show

(queens(8) take 3 map show) mkString "\n"

// now lets do maps

val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry get "andorra"
capitalOfCountry get "US"

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

val fruits = List("apple", "banana", "pear", "pineapple")
fruits sortWith (_.length < _.length)
fruits.sorted

fruits groupBy (_.head)

class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ other.terms map adjust)
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }
  override def toString =
    (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" +exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1("andorra")
