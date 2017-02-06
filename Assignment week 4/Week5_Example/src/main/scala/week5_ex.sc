def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

squareList(List(1, 2, 3, 4))

def squareListMap(xs: List[Int]): List[Int] = {
  xs map(x => x * x)
}

squareListMap(List(1, 2, 3, 4))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(l => (l.head, l.size))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))