// ドドスコチャレンジ
// https://twitter.com/Sheeeeepla/status/1554028833942441984
// 【問題】配列{"ドド","スコ"}からランダムに要素を標準出力し続け、
// 『その並びが「ドドスコスコスコ」を3回繰り返したもの』に一致したときに
// 「ラブ注入♡」と標準出力して終了するプログラムを作成せよ(配点:5点)
import higherkindness.droste.data.Nu
import higherkindness.droste.data.Nu.Default
import scala.collection.immutable
import higherkindness.droste.data.Attr

// ドド、またはスコのいずれかを出力する無限リストを考える。
// これはanaで作成できる:
import higherkindness.droste._
import higherkindness.droste.data.:<
import higherkindness.droste.data.list._

import scala.util.Random

val d = "ドド"
val s = "スコ"

val ds = () => Random.nextInt(2)
val dsa = "スコ" :: "ドド" :: Nil

// test: スコが出たら停止するana
// val sukoCoalgebra = Coalgebra[ListF[String, *], String] {
//   case "スコ" => NilF
//   case "ドド" => ConsF("ドド", ds())
// }
// val sukoAna = scheme.ana(sukoCoalgebra)

// sukoAna("ドド")

// apoの練習
// 型パラメータRは再帰しないので、直接List[Int]を与える
val rco = RCoalgebra[List[Int], ListF[Int, *], Int] {
  case n if n > 10 => NilF
  case n if n < 0  => ConsF(0, Left(Nil))
  case n           => ConsF(n, Right(n + 1))

}
val rcoApo = scheme.zoo.apo(rco)

rcoApo(0)
rcoApo(100)
rcoApo(-10)

// paraの練習
// paraは今まで見てきたサブツリーを得られる
val ral = RAlgebra[List[Int], ListF[Int, *], List[List[Int]]] {
  case ConsF(head, (orig, tail)) => head +: orig :: tail
  case NilF                      => List(Nil)
}
val tails = scheme.zoo.para(ral)
tails(List(1, 2, 3, 4))

// histoの練習
// course-of-value algebra, abbreviated to a CV-algebra
// https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html
val cusumCVA = CVAlgebra[ListF[Int, *], Int] {
  case ConsF(head, tail :< attr) => head + tail
  case NilF                      => 0
}

val cusum = scheme.zoo.histo(cusumCVA)
cusum(List(1, 2, 3, 4, 5))

val natCoalgebra: Coalgebra[Option, BigDecimal] =
  Coalgebra(n => if (n > 0) Some(n - 1) else None)
val natNat = scheme.ana(natCoalgebra)
natNat(3)

val fibCVA = CVAlgebra[Option, BigDecimal] {
  case None                    => 0
  case Some(_ :< None)         => 1
  case Some(x :< Some(y :< _)) => x + y
}

val fib = scheme.zoo.histo(fibCVA)
fib(natNat(10))

// 素朴なhyloじゃなくてhistoなどのバリエーションによるhyloをやりたいときはghyloをつかうっぽい
// gなんとかシリーズは合成可能なgenericなバージョン
// genericなバージョンを使う場合は、それぞれにどのschemeを使うかをgather/scatterで指定するっぽい
val fib2 =
  scheme.ghylo(fibCVA.gather(Gather.histo), natCoalgebra.scatter(Scatter.ana))
fib2(10)

// 実はhisto;anaはdynaなのでこう書ける
val fib3 = scheme.zoo.dyna(fibCVA, natCoalgebra)
fib3(10)

val dodoStream =
  data.stream.Stream.fromIterator(LazyList.iterate(ds())(_ => ds()).iterator)
val ddskRATest =
  Algebra[ListF[String, *], data.stream.Stream[String]] {
    case NilF              => data.stream.Stream.empty
    case ConsF(head, tail) => data.stream.Stream.cons(tail)(head)
  }
val ddskParaTest = scheme.cata(ddskRATest)

val dodosukoCoalgebra = Coalgebra[ListF[Int, *], (Int, Int)] {
  case (_, st) if st == 2184 => NilF
  case (word, st) =>
    print(dsa(word)); ConsF(word, (ds(), ((st << 1) | word) & 4095))
}
val dodosukoAnamorphism = scheme.ana(dodosukoCoalgebra)
def injectLove() = println("ラブ注入♡")

dodosukoAnamorphism(ds() -> 0)
injectLove()
