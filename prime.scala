object PrimeNumber {
  // 約数を求める
  def divisors(n: Int): List[Int] =
    for (i <- (1 to n).toList if n % i == 0) yield i
//    for (i <- List.range(1, n+1) if n % i == 0) yield i

  // 約数が2のときは素数
  def isPrime(n: Int): Boolean = divisors(n).lengthCompare(2) == 0

  // 与えられた整数までの素数のリストを返す
  def findPrimeUntil(n: Int): Iterable[Int] = {
    for (i <- 1 until n if isPrime(i)) yield i
  }

  def main(args: Array[String]) {
    val l = findPrimeUntil(97)
    println(l)
  }
}