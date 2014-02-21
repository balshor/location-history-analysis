package com.leeriggins.location

import scala.xml._
import com.leeriggins.location.GeoMath._
import com.leeriggins.location.model._
import com.leeriggins.location.parser._
import org.threeten.bp._

object Run extends App {

  // from Google location history
  val historyXML = XML.loadFile("history-12-02-2013.kml")
  
  // from Google map engine
  val polygonsXML = XML.loadFile("eb-all.kml")

  val parsedPolygons = new MapmakerKmlParser().apply(polygonsXML)
  val polygon = parsedPolygons("East Bay")
  
  val tracks = new LocationHistoryParser().apply(historyXML)

  def sumDistance(tracks: Seq[Track]): Double = {
    tracks.sliding(2).map {
      case Seq(from, to) =>
        haversine(from, to)
    }.sum
  }

  val totalDistance = sumDistance(tracks)
  
  val distancesByDay = tracks.groupBy { track =>
    (track.time.getMonthValue, track.time.getDayOfMonth)
  }.mapValues { tracks =>
    sumDistance(tracks)
  }.toIndexedSeq.sorted

  // simple geo filter
  val treasureIslandlng = -122.3702611
  val tracksInSF = tracks.filter(_.location.lng < treasureIslandlng)
  val daysInSF = tracksInSF.map(_.toMonthDay).toSet.toIndexedSeq.sorted
  
  // polygon geo filter
  val notInPolygon = tracks.filter { track =>
    !polygon.contains(track.location)
  }
  println(notInPolygon.size)
  val daysOutOfEastBay = notInPolygon.map(_.toMonthDay).toSet.toIndexedSeq.sorted
  
  val highTravelDays = distancesByDay.filter(_._2 > 100)

  // output
  println(s"found ${tracks.size} points")
  println(s"total distance is ${totalDistance} km")
  
  println(s"${daysInSF.size} days in SF:")
  daysInSF.map { case (month, day) => s"${month}/${day}" }.foreach { dayStr => println("  " + dayStr) }
  
  println(s"${daysOutOfEastBay.size} days out of East Bay:")
  daysOutOfEastBay.map { case (month, day) => s"${month}/${day}" }.foreach { dayStr => println("  " + dayStr) }
  
  println(s"average distance per day is ${distancesByDay.map(_._2).sum / distancesByDay.size} km")
  println(s"${distancesByDay.filter(_._2 < 3).size} lazy days (did not leave the house)")
  
  println(s"${highTravelDays.size} high travel days (>100km)")
  highTravelDays.foreach { case ((month, day), distance) =>
    println(s"  ${month}/${day}: ${distance} km")
  }

}
