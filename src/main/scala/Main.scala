import higherkindness.droste.Algebra
import higherkindness.droste.Coalgebra
import higherkindness.droste.data.list.ConsF
import higherkindness.droste.data.list.ListF
import higherkindness.droste.data.list.NilF
import higherkindness.droste.scheme
import higherkindness.droste.CVAlgebra

object Main extends App {
  println("factorial")
  println(Recursive.cataFactorial(List(1, 2, 3, 4, 5)))
  println("iota")
  println(Recursive.anaReverseIota(10))
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

  // CVAlgebra -- histomorphismで使うっぽい。 Attrと関係している。
}
