import scala.io.Source

class TimeSeries(csvFileName: String) {
  val features = {
    val src = Source.fromFile(csvFileName)
    val firstLine = src.getLines.take(1).toList
    src.close
    firstLine.head.split(",")
  }

  val hashMapValues = {
    val src = Source.fromFile(csvFileName)
    val mapColIndexToFeatureNamesToRowValues = src.getLines.drop(1).
      flatMap(line => line.split(",").map(i => i.toDouble).zipWithIndex).
      toArray.map(i => (i._1, features(i._2))).groupBy(_._2).map(i => (i._1, i._2.map(j => j._1)))
    src.close
    mapColIndexToFeatureNamesToRowValues
  }

  // given name of a feature return in O(1) its value series
  def getValues(feature: String): Option[Vector[Double]] = if (hashMapValues.contains(feature))
    Some(hashMapValues(feature).toVector) else None

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    hashMapValues.get(feature) match {
      case None => None
      case Some(value) => value.lift(timeStep)
    }
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature: String, r: Range): Option[Vector[Double]] = {
    hashMapValues.get(feature) match {
      case None => None
      case Some(value) => if (r.forall(x => (0 <= x) && (x <= value.length - 1))) Some(r.map(i => value(i)).toVector) else None
    }
  }
}