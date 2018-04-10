package examples

/** Quick sort, imperative style */
object sort {

  /** Nested methods can use and even update everything
    *  visible in their scope (including local variables or
    *  arguments of enclosing methods).
    */
  def sort(a: Array[Int]) {

    def swap(i: Int, j: Int) {
      val t = a(i); a(i) = a(j); a(j) = t
    }

    def sort1(l: Int, r: Int) {
      print("r = " + r + ";")
//print(a(((l + r) / 2)) + ",")
      val pivot = a((l + r) / 2)
      var i = l // 0
      var j = r // 5
      while (i <= j) {
        // a(0) = 6 < 8  = 0 += 1 = i は 1
        while (a(i) < pivot) i += 1
        // a(5) = 1 < 8  = 5 -= 1 = j は 4
        while (a(j) > pivot) j -= 1
        if (i <= j) {
        // 1 <= 5
          swap(i, j)
          // i は4 -> 5
          i += 1
          // j は1 -> 0
          j -= 1
        }
      }
      // 0 < 0
      if (l < j) sort1(l, j)
      // 0 < 5
      // sprt1(5, 5)
      //print("i = " + i + ";")
//      print("r = " + r + ";")
      if (j < r) sort1(i, r)
    }

    if (a.length > 0)
      sort1(0, a.length - 1)
  }

  def println(ar: Array[Int]) {
    def print1 = {
      def iter(i: Int): String =
        ar(i) + (if (i < ar.length-1) "," + iter(i+1) else "")
      if (ar.length == 0) "" else iter(0)
    }
    Console.println("[" + print1 + "]")
  }

  def main(args: Array[String]) {
    val ar = Array(6, 2, 8, 5, 1)
    println(ar)
    sort(ar)
    println(ar)
  }

}