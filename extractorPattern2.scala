object extractorObject {

//  object Sample {
//
//    def apply(str: String): String = {
//      str + " (applyが呼ばれた)"
//    }
//
//    def unapply(str: String): Option[String] = {
//      if (str.nonEmpty) Some(str + (" (unapplyも呼ばれた)")) else None
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    val p = Sample("本日は晴天なり") // applyを呼ぶ
//    println(p)
//
//    p match {
//      case Sample(str) => println(str) // unapplyを呼ぶ
//      case _ => println("")
//    }
//  }
//}

  abstract class Tree

//  case class Branch(left: Tree, right: Tree) extends Tree
//  case class Leaf(x: Int) extends Tree

  class Branch(val left: Tree, val right: Tree) extends Tree

  object Branch {

    def apply(left: Tree, right: Tree): Tree = {
      println("Branch#apply 呼ばれたよ")
      new Branch(left, right)
    }

    def unapply(x: Tree): Option[(Tree, Tree)] = {
      println("Branch#unapply 呼ばれたよ")
      println(x)
      x match {
        case y: Branch => Some(y.left, y.right)
        case _ => None
      }
    }
  }

  class Leaf(val x: Int) extends Tree

  object Leaf {

    def apply(x: Int): Tree = {
      println("Leaf#apply 呼ばれたよ" + "x = " + x.toString)
      new Leaf(x)
    }

    def unapply(x: Tree): Option[Int] = {
      println("Leaf#unapply 呼ばれたよ")
      println(x)
      x match {
        case y: Leaf => Some(y.x)
        case _ => None
      }
    }
  }

  // 木A
  val treeA = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  def sumLeaves(t: Tree): Int = t match {
    case Branch(l, r) => sumLeaves(l) + sumLeaves(r)
    case Leaf(x) => x
  }

  // 実行
  def main(args: Array[String]): Unit = {
    println("sum of leaves = " + sumLeaves(treeA))
  }
}
