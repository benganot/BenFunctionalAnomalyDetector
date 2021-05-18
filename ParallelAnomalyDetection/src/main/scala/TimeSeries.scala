import scala.::
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class TimeSeries() {
  var hashMapValues = Map[String,Array[Double]]()
  var features = Array[String]()

  def this(csvFileName: String) {
    this()
    this.features = {
      val src = Source.fromFile(csvFileName)
      val firstLine = src.getLines.take(1).toList
      src.close
      firstLine.head.split(",")
    }

    this.hashMapValues = {
      val src = Source.fromFile(csvFileName)
      val mapColIndexToFeatureNamesToRowValues = src.getLines.drop(1).
        flatMap(line => line.split(",").map(i => i.toDouble).zipWithIndex).
        toArray.map(i => (i._1, this.features(i._2))).groupBy(_._2).map(i => (i._1, i._2.map(j => j._1)))
      src.close
      mapColIndexToFeatureNamesToRowValues
    }
  }

  def this(v:  Map[String, Array[Double]]){
    this()
    this.features = v.keys.toArray
    this.hashMapValues = v
  }

  def length(): Int = hashMapValues.head._2.length

  def split(n:Int):List[TimeSeries] = {
    if (n == 1) List(this)
    else {
      var needRev = false
      val chunkSize = length / n
      val a = this.hashMapValues.map(h => (h._1, h._2.grouped(chunkSize).toArray))
      val b2 = a.map(
        x => if ((x._2.length >= 2) && (x._2.last.length < chunkSize)) {
          needRev = true
          (x._1,x._2.dropRight(2) :+ (x._2.dropRight(1).last ++ x._2.last))
        } else
          (x._1,x._2)
      )
//      val b1 = a.collect {
//          case x => if ((x._2.length >= 2) && (x._2.last.length < chunkSize))
            //(x._1,x._2.dropRight(2).appended(x._2.dropRight(1).last.appendedAll(x._2.last)))
//        }
        val c = b2.flatMap(x => x._2.zipWithIndex.map(y => (y._2, x._1, y._1))).groupBy(_._1)
        val d = c.map(x => (x._1, x._2.map(y => (y._2, y._3)).groupBy(_._1).map(g=> (g._1,g._2.map(_._2))))).
          map(i => (i._1,i._2.map(y => (y._1,y._2.head)))).values.map(x => new TimeSeries(x)).toList
      if (needRev) d.reverse else d
    }
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