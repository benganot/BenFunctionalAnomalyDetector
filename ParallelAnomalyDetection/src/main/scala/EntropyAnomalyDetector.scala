import scala.collection.mutable.ListBuffer
import scala.util.Sorting

object EntropyAnomalyDetector extends ParAnomalyDetector{
  override def map(ts: TimeSeries):Reports= {
    val a = ts.hashMapValues.map(x =>
      (x._1,
      x._2.zipWithIndex.map(y => (y._2,
                                  Math.abs(Util.entropy(x._2) - Util.entropy(x._2.zipWithIndex.
                                                                filter(_._2 != y._2).
                                                                map(_._1))))).
        maxBy(_._2))).
      map(x => Report(x._1,x._2._1,x._2._2))
    var new_reports = ListBuffer[Report]()
    new_reports.appendAll(a)
    new_reports
  }
  override def reduce(r1:Reports,r2:Reports):Reports= {
    r1.sortBy(_.feature).
      zip(r2.sortBy(_.feature)).
      map(x => Array(x._1,x._2).maxBy(_.anomalyScore))
  }
}