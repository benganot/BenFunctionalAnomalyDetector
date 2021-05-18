import Util.zscore

object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] =
    normal.hashMapValues.map(x => (x._1, x._2.map(y => Math.abs(zscore(x._2, y))).max.toString))

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    test.hashMapValues.toArray.map(x => (x._1, x._2.zipWithIndex.map(y => (x._1, y._2, y._1)))).
      flatMap(_._2).
      filter(x => Math.abs(zscore(test.getValues(x._1).get.toArray, x._3)) > model(x._1).toDouble).
      map(x => (x._1, x._2)).toVector
  }
}
