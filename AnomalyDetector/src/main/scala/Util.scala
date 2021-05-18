object Util {
  def max[A](myList: List[A], function: (A, A) => Double): A = myList.reduce((x, y) => if (function(x, y) >= 0) x else y)

  def map[A, B, C](myList: List[A], AtoB: A => B, BtoC: B => C): List[C] = {
    myList.map(AtoB).map(BtoC)
  }

  def isSorted[A](myList: List[A], function: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = {
      if (i >= myList.length - 1) true
      else function(myList(i), myList(i + 1)) && go(i + 1)
    }

    go(0)
  }

  def entropy(doubleArray: Array[Double]): Double =
    -doubleArray.zip(probes(doubleArray)).distinct.map(x => x._2).map(x => x * math.log10(x) / math.log10(2)).sum

  def probes(doubleArray: Array[Double]):
  Array[Double] = doubleArray.map(x => doubleArray.count(y => y == x) / doubleArray.length.toDouble)

  def zscore(xs: Array[Double], d: Double): Double = (d - mu(xs)) / math.sqrt(variance(xs))

  def variance(doubleArray: Array[Double]): Double =
    doubleArray.zip(probes(doubleArray)).distinct.map(x => x._2 * math.pow(x._1 - mu(doubleArray), 2)).sum

  def mu(doubleArray: Array[Double]): Double = doubleArray.zip(probes(doubleArray)).distinct.map(x => x._1 * x._2).sum

  def cov(xs: Array[Double], ys: Array[Double]): Double = mu((xs zip ys).map(x => x._1 * x._2)) - mu(xs) * mu(ys)

  def pearson(xs: Array[Double], ys: Array[Double]): Double = cov(xs, ys) / (math.sqrt(variance(xs)) * math.sqrt(variance(ys)))

  // Additional functions
  def getMaxCorrelation(base_element: (String, Array[Double]))(restOfFeatures: List[(String, Array[Double])]): List[(String, String, Double)] = restOfFeatures match {
    case Nil => List()
    case _ => (base_element._1,
      restOfFeatures.head._1,
      Math.abs(pearson(base_element._2,
        restOfFeatures.head._2))) :: getMaxCorrelation(base_element)(restOfFeatures.tail)
  }

  def rec(base: List[(String, Array[Double])]): List[List[(String, String, Double)]] = base match {
    case Nil => List()
    case _ => getMaxCorrelation(base.head)(base.tail) :: rec(base.tail)
  }

  def publicGetTopCorrelation(data: TimeSeries): Array[(String, String, Double)] = {
    rec(data.features.map(x => (x, data.getValues(x).get.toArray)).toList).toArray.
      map(_.toArray).
      dropRight(1).
      map(x => x max Ordering[Double].on[(_, _, Double)](_._3))
  }

  def sqrSum(p: Point, points: Array[Point]): Double = points.filter(x => !((x.x == p.x) && (x.y == p.y)))
    .map(x => Math.pow(sqrDist(p, x), 2)).sum

  def sqrDist(p1: Point, p2: Point): Double = Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))

  def getPointsFromTwoDoubleArray(xs: Array[Double], ys: Array[Double]): Array[Point] = xs.zip(ys).map(x => new Point(x._1, x._2))
}
