object Main {
  def divisors(n: Int): List[Int] =
    for (i <- List.range(1, n+1) if n % i == 0) yield i

  def isPrime(n: Int) = divisors(n).lengthCompare(2) == 0

  def findPrimeUntil(n: Int): Iterable[Int] = {
    for (i <- (1 until n); if isPrime(i)) yield i
  }

  def main(args: Array[String]) {
    val l = findPrimeUntil(1000)
    println(l)
  }
}