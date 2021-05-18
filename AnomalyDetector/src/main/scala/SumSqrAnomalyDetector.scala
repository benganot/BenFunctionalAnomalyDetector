import Util.{getPointsFromTwoDoubleArray, publicGetTopCorrelation, sqrSum}

object SumSqrAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    publicGetTopCorrelation(normal).
      filter(x => x._3 >= 0.9).
      map(x => (x._1, x._2)).
      map(x => (x._1, x._2, getPointsFromTwoDoubleArray(normal.hashMapValues(x._1), normal.hashMapValues(x._2)))).
      map(x => (x._1, x._2, x._3.map(p => sqrSum(p, x._3)).max)).
      map(x => (new StringBuilder(x._1).append("<=>").append(x._2).toString, x._3.toString)).toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    model.map(x => (x._1.split("<=>"), x._2.toDouble)).
      map(x => (x._1.head, x._1.last, x._2)).
      map(x =>
        (x._1,
          x._2,
          x._3,
          getPointsFromTwoDoubleArray(test.hashMapValues(x._1), test.hashMapValues(x._2)).zipWithIndex)).
      flatMap(x => x._4.filter(p => sqrSum(p._1, x._4.map(_._1)) > x._3).map(p => (x._1, x._2, p._2))).
      map(x => (new StringBuilder(x._1).append(",").append(x._2).toString, x._3)).toVector
  }
}
