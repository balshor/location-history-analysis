package com.leeriggins.location

import scala.math._
import com.leeriggins.location.model._

object GeoMath {
  private val R = 6372.8 //radius in km

  // from http://rosettacode.org/wiki/Haversine_formula#Scala
  // result is in km
  def haversine(lat1: Double, lng1: Double, lat2: Double, lng2: Double): Double = {
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lng2 - lng1).toRadians

    val a = pow(sin(dLat / 2), 2) +
      pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(lat2.toRadians)
    val c = 2 * asin(sqrt(a))
    R * c
  }

  // result is in km
  def haversine(track1: Track, track2: Track): Double =
    haversine(track1.location.lat, track1.location.lng, track2.location.lat, track2.location.lng)
}  
