package examples

object Persons {
  /**
    * A list of persons. To create alist, we use Predef.List
    * which takes a variable number of argumentes and constructs
    * a list out of them
    */
  val persons = List(
    new Person("Bob", 17),
    new Person("Json", 40),
    new Person("Richard", 68)
  )

  /**
    * A Person class. 'val' constructor parameters become
    * public members of the class.
    */
  class Person(val name: String, val age: Int)

  /**
    * Return on iterator ober persons that are older than 20
    */
//  def olderThan20(xs: Seq[Person]): Iterator[String] = {
//    olderThan20(xs.elements)
//  }

//  def olderThan20(xs: Seq[Person]): Iterator[String] = {
//    olderThan20(xs.toIterator)
//  }
  def olderThan20(xs: Seq[Person]): Seq[String] =
    for (p <- xs if p.age > 20) yield p.name

  /**
    * Return on iterator over persons loder than 20,
    * given on iterator over persons.
    */
//  def olderThan20(xs: Iterator[Person]): Iterator[String] = {
//    for (p <- xs if p.age > 20) yield p.name
//  }

}

object Numeric {
  /**
    * Return the divisors of n.
    */
  def divisors(n: Int): List[Int] =
    for (i <- List.range(1, n+1) if n % i == 0) yield i

  /**
    * Is 'n' a prime number?
    */
  def isPrime(n: Int) = divisors(n).length == 2

  /**
    * Return pairs of numbers whose sum is prime.
    */
  def findNums(n: Int): Iterable[(Int, Int)] = {
    // a for comprehension usein two generators
    for (i <- 1 until n;
         j <- 1 until (i-1);
         if isPrime(i + j)) yield (i, j)
  }

  /**
    * Return the sum of the elements of xs
    */
  def sum(xs: List[Double]): Double =
    xs.foldLeft(0.0) { (x, y) => x + y }

  /**
    * Retunr the sum of pairwise product of the two lists
    */
  def scalProd(xs: List[Double], ys: List[Double]) =
    sum(for((x, y) <- xs zip ys) yield x * y);

  /**
    * Remove dupulicate elements in xs
    */
  def removeDuplicates[A](xs: List[A]): List[A] =
    if (xs.isEmpty)
      xs
    else
      xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)
}

/**
  * The main class, the entry point of this program.
  */
object Fors {
  def main(args: Array[String]): Unit = {
    // import all members of object 'persons' in the current scope
    import Persons._

    print("Persons over 20:")
    olderThan20(persons) foreach { x => print(" " + x) }
    println

    import Numeric._

    println("divisors(34) = " + divisors(34))

    print("findNums(15) = ")
    findNums(15) foreach { x => print(" " + x) }
    println

    val xs = List(3.5, 5.0, 4.5)
    println("average(" + xs + ") = " + sum(xs) / xs.length)

    val ys = List(2.0, 1.0, 3.0)
    println("scalProd(" + xs + ", " + ys + ") = " + scalProd(xs, ys))
  }
}

























