package homeworks.futures

//import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

object task_futures_sequence {

  /**
   * @param eithers list of eithers
   * @return swapped tuple of lists: (rights, lefts)
   */
  def partitionEithers[B, A](eithers: List[Either[B, A]]): (List[A], List[B]) =
    eithers.partitionMap(identity).swap

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
//    task"Реализуйте метод `fullSequence`"()
  {
    val tryToEither: Try[A] => Success[Either[Throwable, A]] =
      (x: Try[A]) => Success(x.toEither)

    val eitherFutures: List[Future[Either[Throwable, A]]] =
      futures.map(_ transform tryToEither)

    Future.sequence(eitherFutures) map partitionEithers
  }
}
