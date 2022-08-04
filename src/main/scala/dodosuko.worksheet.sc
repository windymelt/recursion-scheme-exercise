// ドドスコチャレンジ
// https://twitter.com/Sheeeeepla/status/1554028833942441984
// 【問題】配列{"ドド","スコ"}からランダムに要素を標準出力し続け、
// 『その並びが「ドドスコスコスコ」を3回繰り返したもの』に一致したときに
// 「ラブ注入♡」と標準出力して終了するプログラムを作成せよ(配点:5点)

// ドド、またはスコのいずれかを出力する無限リストを考える。
// これはanaで作成できる:
import higherkindness.droste._
import higherkindness.droste.data.list._

import scala.util.Random

val dd = "ドド"
val sk = "スコ"

val dodoOrSuko: () => String = () =>
  Random.nextBoolean() match {
    case true  => dd
    case false => sk
  }

val example = dodoOrSuko()

// Rangeを作る
val lif = ListF.fromScalaList(List(1, 2, 3, 4, 5))
val sumAlg = Algebra[ListF[Int, _], Int] {
  case ConsF(h, t) => h + t
  case NilF        => 0
}

val sumA = scheme.cata(sum)
scheme.cata[ListF[Int, _], Int, Int](sum)
// def sumA: Algebra[ListF[Int], Int] = Algebra {
//   case Some(n) => n + 1
//   case None    => 0
// }
// val sum = scheme.cata(sumA)

// 指定された個数出力できるようにする。
// 残り個数と出力する文字列は違う型なのでanaをそのまま使うことはできない

val ddskCoalgebra = Coalgebra[List, (Int, String)](state =>
  state match {
    case (0, _)    => Nil
    case (n, rest) => (n, rest) :: List((n - 1, dodoOrSuko()))
  }
)

//val ddskCoalgebra = Coalgebra[Option, String](_ => Some(dodoOrSuko()))

val infiniteDdSk = scheme.ana(ddskCoalgebra)
// 停止しない
//infiniteDdSk((10, ""))

// 過去の出力をもとに
