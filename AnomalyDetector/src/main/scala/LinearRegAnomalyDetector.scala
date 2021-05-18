import Util.{getPointsFromTwoDoubleArray, publicGetTopCorrelation}

object LinearRegAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    publicGetTopCorrelation(normal).
      filter(x => x._3 >= 0.9).
      map(x => (x._1, x._2)).
      map(x => (x._1, x._2, getPointsFromTwoDoubleArray(normal.hashMapValues(x._1), normal.hashMapValues(x._2)))).
      map(x => (x._1, x._2, x._3, new Line(x._3))).
      map(x => (x._1, x._2, x._3, x._4, getMaxDistanceFromLine(x._4, x._3))).
      map(x => (new StringBuilder(x._1).append("<=>").append(x._2).toString, new StringBuilder(x._4.toString).append(",").append(x._5.toString).toString)).toMap
  }

  def getMaxDistanceFromLine(line: Line, points: Array[Point]): Double = points.map(line.dist).max

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    model.map(x => (x._1.split("<=>"), x._2.split(","))).
      map(x => (x._1.head, x._1.last, x._2.head.split(" "), x._2.last.toDouble)).
      map(x =>
        (x._1,
          x._2,
          new Line(x._3.head.toDouble, x._3.last.toDouble),
          x._4,
          getPointsFromTwoDoubleArray(test.hashMapValues(x._1), test.hashMapValues(x._2)).zipWithIndex)).
      flatMap(x => x._5.filter(p => x._3.dist(p._1) > x._4).map(p => (x._1, x._2, p._2))).
      map(x => (new StringBuilder(x._1).append(",").append(x._2).toString, x._3)).toVector
  }
}
