trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

// add week4 stuff on List object

object List {
  // List(1,2) = List.apply(1,2)
  // List() = new Nil
  // List(1) = List.apply(1)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](): List[T] = new Nil
  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil)

singleton[Int](1)
singleton[Boolean](true)
singleton(1)

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list)
nth(-1, list)