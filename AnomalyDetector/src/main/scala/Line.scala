class Line() {
  var a = Double.NaN
  var b = Double.NaN

  def this(ps: Array[Point]) {
    this()
    this.a = Util.cov(ps.map(_.x), ps.map(_.y)) / Util.variance(ps.map(_.x))
    this.b = ps.map(_.y).sum / ps.length - this.a * (ps.map(_.x).sum / ps.length)
  }

  def this(n: Double, m: Double) {
    this()
    this.a = n
    this.b = m
  }

  def dist(point: Point): Double = Math.abs(this.f(point.x) - point.y)

  //  val a: Double = Util.cov(ps.map(_.x),ps.map(_.y)) / Util.variance(ps.map(_.x))
  //  val b: Double = ps.map(_.y).sum/ps.length - a* (ps.map(_.x).sum/ps.length)
  def f(x: Double): Double = this.a * x + this.b

  override def toString: String = s"$a * x + $b"
}