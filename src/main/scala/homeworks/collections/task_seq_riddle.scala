package homeworks.collections

//import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = encode(currentLine).flatMap(decode)
  //    task"Реализуйте функцию генерирующую след последовательность из текущей"()

  def encode[A](list: List[A]): List[(Int, A)] = {
    @tailrec
    def loop(rest: List[A], acc: List[(Int, A)]): List[(Int, A)] = {
      rest match {
        case Nil => acc
        case h :: _ =>
          val (head, tail) = rest.span(_ == h)
          val add = (head.length, head.head)
          loop(tail, acc.appended(add))
      }
    }

    loop(list, List.empty[(Int, A)])
  }

  private def decode = {
    elem: (Int, Int) => List(elem._1, elem._2)
  }


  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */


  lazy val funSeq: LazyList[List[Int]] = ???
//  task"Реализуйте ленивый список, который генерирует данную последовательность"()


  def generator(first: List[Int] = List(1)): LazyList[List[Int]] = {
    LazyList.cons(first, generator(nextLine(first)))
  }

  def select(n: Int): List[Int] = generator().take(n + 1).last
}
