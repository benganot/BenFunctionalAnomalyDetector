import LinearRegAnomalyDetector.getMaxDistanceFromLine
import Util._

object HybridAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    val a = publicGetTopCorrelation(normal).map(doLearnLogic(normal)).toMap
    val last_feature = (normal.features.last, normal.hashMapValues(normal.features.last))
    a + ((last_feature._1, last_feature._2.map(y => Math.abs(zscore(last_feature._2, y))).max.toString))
  }

  def doLearnLogic(ts: TimeSeries)(correlation: (String, String, Double)): (String, String) = {
    if (correlation._3 >= 0.9) {
      val correlation2 = (correlation._1, correlation._2, getPointsFromTwoDoubleArray(ts.hashMapValues(correlation._1), ts.hashMapValues(correlation._2)))
      val correlation3 = (correlation2._1, correlation2._2, correlation2._3, new Line(correlation2._3))
      val correlation4 = (correlation3._1, correlation3._2, correlation3._3, correlation3._4, getMaxDistanceFromLine(correlation3._4, correlation3._3))
      (new StringBuilder(correlation4._1).append("<=>").append(correlation4._2).append("<=>L").toString, new StringBuilder(correlation4._4.toString).append(",").append(correlation4._5.toString).toString)
    }
    else if (correlation._3 > 0.5) {
      val correlation2 = (correlation._1, correlation._2, getPointsFromTwoDoubleArray(ts.hashMapValues(correlation._1), ts.hashMapValues(correlation._2)))
      val correlation3 = (correlation2._1, correlation2._2, correlation2._3.map(p => (p, sqrSum(p, correlation2._3))).minBy(_._2))
      val correlation4 = (correlation3._1, correlation3._2, correlation2._3.map(p => sqrDist(correlation3._3._1, p)).max)
      (new StringBuilder(correlation4._1).append("<=>").append(correlation4._2).append("<=>S").toString(), correlation4._3.toString)
    }
    else {
      val correlation2 = (correlation._1, ts.hashMapValues(correlation._1))
      (correlation2._1, correlation2._2.map(y => Math.abs(zscore(correlation2._2, y))).max.toString)
    }
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    model.map(doDetectLogic(test)).flatten.toVector
  }

  def doDetectLogic(ts: TimeSeries)(correlation: (String, String)): Vector[(String, Int)] = {
    val correlation2 = (correlation._1.split("<=>"), correlation._2)
    // Low correlation detected
    if (correlation2._1.length == 1) {
      val correlation3 = (correlation._1, ts.hashMapValues(correlation._1))
      correlation3._2.zipWithIndex.
        map(y => (correlation3._1, y._2, y._1)).
        filter(x => Math.abs(zscore(correlation3._2, x._3)) > correlation2._2.toDouble).
        map(v => (v._1, v._2)).toVector
    }
    else if (correlation2._1.length == 3) {
      val featureX = correlation2._1(0)
      val featureY = correlation2._1(1)
      val featuresPoints = getPointsFromTwoDoubleArray(ts.hashMapValues(featureX), ts.hashMapValues(featureY))
      correlation2._1(2) match {
        // Medium correlation detected
        case "S" => {
          val minPoint = featuresPoints.map(p => (p, sqrSum(p, featuresPoints))).minBy(_._2)._1
          featuresPoints.zipWithIndex.filter(p => sqrDist(minPoint, p._1) > correlation2._2.toDouble).
            map(x => (new StringBuilder(featureX).append(",").append(featureY).toString, x._2)).toVector
        }
        // High correlation detected
        case "L" => {
          val (lineStr, maxDistFromLine) = (correlation2._2.split(",")(0), correlation2._2.split(",")(1).toDouble)
          val regressionLine = new Line(lineStr.split(" ").head.toDouble, lineStr.split(" ").last.toDouble)
          featuresPoints.zipWithIndex.filter(p => regressionLine.dist(p._1) > maxDistFromLine).
            map(x => (featureX, featureY, x._2)).
            map(x => (new StringBuilder(x._1).append(",").append(x._2).toString, x._3)).toVector
        }
      }
    }
    else {
      Vector.empty
    }
  }
}
