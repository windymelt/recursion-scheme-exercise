import higherkindness.droste.data.list.{ListF, NilF, ConsF}
import higherkindness.droste._
import higherkindness.droste.data.:<

val x = 1

// 基本的な1次元累積和をまず実装する
val l = List(0, 1, 0, 0, 1, 0, -1, 0, -1, 0)
// 今まで見てきた値を参照したいので paramorphism を使う
val cusum1RA = RAlgebra[List[Int], ListF[Int, *], List[Int]] {
  case NilF               => Nil
  case ConsF(h, (lis, t)) => h + lis.sum :: t
}
val cusum1 = scheme.zoo.para(cusum1RA)
// 再帰は順番が逆になるのでreverseする
cusum1(l.reverse)

// sumが結局重たい。直前のsumの値は一度計算すれば確定して使い回せるので histomorphism を使う
// histoが使うのはCVAlgebraなので、定義する
val cusum1CVA = CVAlgebra[ListF[Int, *], List[Int]] {
  // base case
  case NilF                            => Nil
  // recursion case, first step
  case ConsF(h, tail :< NilF)          => h :: tail
  // recursuon case, second and more step
  case ConsF(h, tail :< ConsF(hist, _)) => hist + h :: tail
}
// できた。
val cusum1Histo = scheme.zoo.histo(cusum1CVA)
cusum1Histo(l.reverse)

// 直前の値はConsFから取り出せることがわかったのでCataでよかった:
val cusum1A = Algebra[ListF[Int, *], List[Int]] {
  case NilF => Nil
  case ConsF(h, Nil) => h :: Nil
  case ConsF(h, t :: ts) => h + t :: t :: ts
}
val cusum12 = scheme.cata(cusum1A)
cusum12(l.reverse)

// 結局cataも過去の計算結果にアクセスできるんじゃん・・・
// imosの場合、返り値としてListとして展開しながら返しているので、cata中から参照できる
// 最終的に返したい値はスカラーだが、計算過程で過去の計算結果がほしいときなどにhistoを使うことになりそう。
// 例えfibを計算するときのmemoiseに使ったりできそう。
import higherkindness.droste.data.Coattr
scheme.zoo.futu(CVCoalgebra[ListF[Int, *], Int] {
  case n => ConsF(Coattr(Right(n + 1)), Coattr(Right(NilF)))
})
