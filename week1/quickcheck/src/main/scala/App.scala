// Playground for Week 1
object App extends App {
  // Recap: Collections
  val ys = for {
    x <- 2 to 20
    y <- 2 to x
    if (x % y == 0)
  } print((x, y))
  println()
  (2 to 20) flatMap (x => (2 to x) withFilter (y => x % y == 0) map (y => (x, y))) foreach (print)
  println()

  // Queries with For
  case class Book(title: String, authors: List[String]) {
    def apply(title: String, authors: List[String]): Book = this(title, authors)
  }

  val books = List(Book("XYZ", List("Oscar", "Michal")), Book("BEKA z ciebie", List("Paulina", "Maciek")), Book("AKKA", List("Lukasz")))

  for {
    x <- books
    a <- x.authors
    if a == "Oscar"
  } println(x.title)

  def checkContaining(input: String, regex: String): Boolean =
    input.toLowerCase contains regex.toLowerCase

  for {
    x <- books
    if checkContaining(x.title, "beka")
  } print(x.title)
  println()

  for {
    x <- books
    y <- books
    if x != y
    a1 <- x.authors
    a2 <- y.authors
  } println(a1 + " - " + a2)

  // Translation of For
  for {
    b <- books
    a <- b.authors
    if a == "Oscar"
  } yield b.title
      .foreach(print)
  println()

  books withFilter (b => b.authors contains "Oscar") map (b => b.title) foreach print
  println()
  books flatMap (b => for (a <- b.authors if a == "Oscar") yield b.title)
  books flatMap (b => for (a <- b.authors withFilter (a => a == "Oscar")) yield b.title)
  books flatMap (b => b.authors withFilter (a => a == "Oscar") map (_ => b.title)) foreach print
  println()

  // Functional Random Generators
  trait Generator[+T] {
    self =>
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate: S = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate: Int = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate: Boolean = integers.generate > 0
  }

  val pairs = new Generator[(Int, Int)] {
    def generate: (Int, Int) = (integers.generate, integers.generate)
  }

  val booleans2 = for {
    x <- integers
  } yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]) =
    t flatMap (x => u map (y => (x, y)))

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans2
    tree <- if (isLeaf) leafs else inners
  } yield tree

  def leafs: Generator[Leaf] = new Generator[Leaf] {
    def generate: Leaf = Leaf(integers.generate)
  }

  def inners:Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ++ l2).size
  }


}
