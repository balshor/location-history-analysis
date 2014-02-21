package com.leeriggins.location.model

import org.threeten.bp._

case class Track(time: ZonedDateTime, location: GeoPoint) {
  def long = location.lng
  def lat = location.lat
  def this(time: ZonedDateTime, lng: Double, lat: Double) = this(time, GeoPoint(lng, lat))
  
  def toMonthDay(): (Int, Int) = (time.getMonthValue, time.getDayOfMonth)
}


object Track {
  def apply(time: ZonedDateTime, lng: Double, lat: Double): Track = Track(time, lng, lat)
}
