import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)

// sort by anomalyScore
object AnomalyScoreOrdering extends Ordering[Report] {
  def compare(a:Report, b:Report): Int = a.anomalyScore compare b.anomalyScore
}

trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]
  def map(ts: TimeSeries): Reports
  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val a = ts.split(chunks).zipWithIndex
//    val b = a.map(t => (t._2,es.submit(() => map(t._1))))
    val b2 = a.map(t => (t._2,es.submit(new Callable[Reports] {
      override def call(): Reports = map(t._1)
    })))
    val c = b2.map(x => (x._1,x._2.get()))
    val d = c.map(x => x._2.map(r => Report(r.feature,x._1*a.head._1.length()+r.timeStep,r.anomalyScore))).
      reduce(reduce).
      sortBy(_.feature).toVector
    d
//    ts.split(chunks).map(t => es.submit(() => map(t))).map(x => x.get()).reduce(reduce).sortBy(_.feature).toVector
  }
}

