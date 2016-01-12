package fpinscala.state

object FoldDemo extends App {

  println("foldLeft")
  println((0 to 10).foldLeft(List[Int]())(evalListLeft))

  println("foldRight")
  println((0 to 10).foldRight(List[Int]())(evalListRight))

  def evalListRight(i: Int, acc: List[Int]): List[Int] = {
    println("eval " + i)
    i :: acc
  }

  def evalListLeft(acc: List[Int], i: Int): List[Int] = {
    println("eval " + i)
    i :: acc
  }

}
