package com.leeriggins.location.model

// Warning: assumes an infinite flat earth.
case class Polygon(vertices: Seq[GeoPoint]) {
  import Polygon._

  val sides = {
    val s = vertices.sliding(2) ++ Iterator(Seq(vertices.last, vertices.head))
    s.map { seq => (seq(0), seq(1)) }
  } toIndexedSeq

  def contains(point: GeoPoint): Boolean = {
    val sidesIntersectingVerticalLine = sides filter { side =>
      side._1.lng <= point.lng && point.lng < side._2.lng ||
        side._1.lng >= point.lng && point.lng > side._2.lng
    } toSeq

    if (sidesIntersectingVerticalLine.exists(pointIsOn(point))) return true
    
    val sidesBelow = sidesIntersectingVerticalLine.filter(pointIsAbove(point))

    sidesBelow.size % 2 == 1
  }

}

object Polygon {
  private def evalLine(lng: Double)(side: (GeoPoint, GeoPoint)): Double = {
    if (side._1.lng == side._2.lng) throw new RuntimeException("Vertical line.")

    val slope = (side._2.lat - side._1.lat) / (side._2.lng - side._1.lng)

    (slope * (lng - side._1.lng) + side._1.lat)
  }

  private def pointIsOn(point: GeoPoint)(side: (GeoPoint, GeoPoint)): Boolean = {
    if (side._1.lng == side._2.lng) {
      return (point.lng == side._1.lng) &&
        ((side._1.lat <= point.lat && point.lat < side._2.lat)
          || (side._1.lat >= point.lat && point.lat > side._2.lat))
    }
    val correspondinglat = evalLine(point.lng)(side)
    correspondinglat == point.lat
  }

  // returns whether a point is above the infinite line defined by the given side
  private def pointIsAbove(point: GeoPoint)(side: (GeoPoint, GeoPoint)): Boolean = {
    if (side._1.lng == side._2.lng) return (point.lng == side._1.lng)
    val correspondinglat = evalLine(point.lng)(side)
    correspondinglat < point.lat
  }
}
