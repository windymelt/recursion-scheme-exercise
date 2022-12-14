import higherkindness.droste.Algebra
import higherkindness.droste.CVAlgebra
import higherkindness.droste.Coalgebra
import higherkindness.droste.RAlgebra
import higherkindness.droste.data.:<
import higherkindness.droste.data.Attr
import higherkindness.droste.data.list.ConsF
import higherkindness.droste.data.list.ListF
import higherkindness.droste.data.list.NilF
import higherkindness.droste.scheme
import higherkindness.droste.prelude

object Main extends App {
  println("factorial")
  println(Recursive.cataFactorial(List(1, 2, 3, 4, 5)))
  println("iota")
  println(Recursive.anaReverseIota(10))
  println("tails")
  println(Recursive.tails(List(1, 2, 3, 4, 5)))
  println("fibonacci -- we never see duplicated calculation")
  println(Recursive.fib(Recursive.natnat(20)))
  println("fibonacci via dyna")
  println(Recursive.fib2(20))
  // println("dodosuko problem")
  // Recursive.dodosukoAnamorphism(Recursive.ds() -> 0)
  // Recursive.injectLove()
  println("imos1")
  println(Recursive.cusum12(Recursive.l.reverse))
}

object Recursive {
  val factorialAlg = Algebra[ListF[Int, *], Int] {
    case ConsF(h, t) => h * t
    case NilF        => 1
  }
  val cataFactorial = scheme.cata(factorialAlg)

  val reverseIotaCoalg = Coalgebra[ListF[Int, *], Int] {
    case n if n < 0 => NilF
    case n          => ConsF(n, n - 1)
  }
  val anaReverseIota = scheme.ana(reverseIotaCoalg)

  val ral = RAlgebra[List[Int], ListF[Int, *], List[List[Int]]] {
    case ConsF(head, (orig, tail)) => head +: orig :: tail
    case NilF                      => List(Nil)
  }
  val tails = scheme.zoo.para(ral)

  val natCoalgebra: Coalgebra[Option, BigDecimal] =
    Coalgebra(n => if (n > 0) Some(n - 1) else None)

  val fibCVA = CVAlgebra[Option, BigDecimal] {
    // 入力された数値自体は興味がないのでOptionでよい
    case None            => { println("zero"); 0 }
    case Some(_ :< None) => { println("one"); 1 }
    case Some(x :< Some(y :< _)) =>
      println(s"$x + $y = ${x + y}")
      x + y
  }

  val fib = scheme.zoo.histo(fibCVA)
  val natnat = scheme.ana(natCoalgebra)

  val fib2 = scheme.zoo.dyna(fibCVA, natCoalgebra)

  val ds = () => scala.util.Random.nextInt(2)
  val dss = "スコ" :: "ドド" :: Nil
  val dodosukoCoalgebra = Coalgebra[ListF[Int, *], (Int, Int)] {
    case (_, 2184) => NilF
    case (b, st)   => print(dss(b)); ConsF(b, (ds(), st << 1 & 4095 | b))
  }
  val dodosukoAnamorphism = scheme.ana(dodosukoCoalgebra)
  def injectLove() = println("ラブ注入♡")

// できた。
val l = List(1, 0, 0, 1, 0, -1, 0, -1, 0)

val cusum1A = Algebra[ListF[Int, *], List[Int]] {
  case NilF => println("base case: Nil");Nil
  case ConsF(h, Nil) => println(s"boobie case: head is $h");h :: Nil
  case ConsF(h, t :: ts) => println(s"case: head is $h and prev.value is $t and posttail is $ts");h + t :: t :: ts
}
val cusum12 = scheme.cata(cusum1A)
}
