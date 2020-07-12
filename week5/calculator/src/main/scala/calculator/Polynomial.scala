package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Signal({
      if (delta() < 0.0) Set.empty
      else
        Set(
          (Math.sqrt(delta()) - b())/(2*a()),
          (-Math.sqrt(delta()) - b())/(2*a())
        )
    })
  }
}
