package com.leeriggins.location.parser

import scala.xml._
import com.leeriggins.location.model._
import org.threeten.bp._

class LocationHistoryParser {

  def apply(kml: Elem): Seq[Track] = {
    val trackNode = kml \ "Document" \ "Placemark" \ "Track"

    val childPairs = trackNode(0).child.filter { node =>
      node.label == "when" || node.label == "coord"
    }.grouped(2)
    
    childPairs map {
      case Seq(whenNode, coordNode) => 
        val time = ZonedDateTime.parse(whenNode.text)
        
        // note that you get lng, lat and not lat, lng as you might expect...
        val Array(lng, lat, altitude) = coordNode.text.split("\\s+").map(_.toDouble)

        Track(time, GeoPoint(lng, lat))
    } toIndexedSeq
  }

}
