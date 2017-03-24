package calculator

import Math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    b() * b() - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (delta() < 0) Set()
    else {
      val solution1 = (-b() + sqrt(delta())) / 2 * a()
      val solution2 = (-b() - sqrt(delta())) / 2 * a()
      Set(solution1, solution2)
    }
  }
}
