package com.leeriggins.location.parser

import scala.xml._
import com.leeriggins.location.model._

class MapmakerKmlParser {

  def apply(kml: Elem): Map[String, Polygon] = {
    val placemarks = kml \ "Document" \ "Folder" \ "Placemark"

    placemarks.map { placemark =>
      val name = placemark \ "name" text

      val verticesText = placemark \ "Polygon" \\ "coordinates" text

      val vertices = verticesText.split("\\s+").map { pointText =>
        val Array(lng, lat, _) = pointText.split(",") map (_.toDouble)
        GeoPoint(lng, lat)
      }.toSeq

      name -> Polygon(vertices)
    }.toMap
  }
}
