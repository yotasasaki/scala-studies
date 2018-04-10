package examples

/** Quick sort, functional style */
object sort1 {
  def sort(a: List[Int]): List[Int] = {
    // 配列の中身が一つになったら、そのまま帰す(List(2)とか)
    if (a.length < 2)
      a
    // それ以外のケースは、
    else {
      val pivot = a(a.length / 2)
        sort(a.filter(_ < pivot)) ::: //(2,1), // ()
          a.filter(_ == pivot) ::: // (3), (1)
          sort(a.filter(_ > pivot)) // () , (2)
    }
  }

  def main(args: Array[String]) {
    //val xs = List(6, 2, 8, 5, 1)
//    val xs = List(9,2,4,7,1,8,5,3,6)
//    val xs = List(2,3,1)
    val xs = List(3,10001,44,1)
    println(xs)
    println(sort(xs))
  }
}