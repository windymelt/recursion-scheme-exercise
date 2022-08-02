import cats.Functor
import cats.implicits._

type ListF[A] = Option[(Int, A)]

def cata[F[_]: Functor, A, B](
    algebra: F[A] => A,
    project: B => F[B]
): B => A = {
  def loop(state: B): A = algebra(project(state).map(loop))
  loop
}

val project: List[Int] => ListF[List[Int]] = {
  case Nil => None
  case lis => Some((lis.head, lis.tail))
}

implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
  def map[A, B](fa: ListF[A])(f: A => B): ListF[B] = fa match {
    case Some((head, tail)) => Some((head, f(tail)))
    case None               => None
  }
}

val productAlgebra: ListF[Int] => Int = {
  case Some((head, tailProd)) => head * tailProd
  case None                   => 1
}

val product = cata(productAlgebra, project)

product(List(1, 2, 3, 4, 5))

val mkStringAlgebra: ListF[String] => String = {
  case Some((head, tailProd)) => s"${head} :: ${tailProd}"
  case None                   => "Nil"
}

val mkString = cata(mkStringAlgebra, project)

mkString(List(1, 2, 3, 4, 5))

val lengthAlgebra: ListF[Int] => Int = {
  case Some((_, tailLength)) => 1 + tailLength
  case None                  => 0
}

val length = cata(lengthAlgebra, project)

length(List(1, 2, 3, 4, 5))
