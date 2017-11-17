package geo.model

case class Geometry(
                     `type`:String,
                     coordinates: Coordinates
                   )

sealed trait Coordinates

case class LatLng(lat: Float,
                  lng: Float) {
  def coordinates = Seq(lng,lat)
}


case class SingleCoordinate(coordinates:Seq[Float]) extends Coordinates {
  def latLng: Option[LatLng] = for{
    lng <- coordinates.lift(0)
    lat <- coordinates.lift(1)
  } yield LatLng(lat,lng)
}
case class MultiCoordinate(coordinates:Seq[Seq[Float]]) extends Coordinates {
  def positions: Seq[LatLng] = coordinates.map(SingleCoordinate).flatMap(_.latLng)
}
case class TripleCoordinate(coordinates:Seq[Seq[Seq[Float]]]) extends Coordinates {
  def positions: Seq[Seq[LatLng]] = coordinates.map(_.map(SingleCoordinate).flatMap(_.latLng))
}

case class QuadrupleCoordinate(coordinates:Seq[Seq[Seq[Seq[Float]]]]) extends Coordinates {
  def positions: Seq[Seq[Seq[LatLng]]] = coordinates.map(_.map(_.map(SingleCoordinate).flatMap(_.latLng)))
}


object Geometry{
  def point(latLng: LatLng) = Geometry(Types.POINT,SingleCoordinate(latLng.coordinates))
  def polygon(latLngs: Seq[LatLng]) = Geometry(Types.POLYGON,MultiCoordinate(latLngs.map(_.coordinates)))

  object Types{
    val POINT = "Point"
    val MULTIPOINT = "MultiPoint"
    val LINESTRING = "LineString"
    val MULTILINESTRING = "MultiLineString"
    val POLYGON = "Polygon"
    val MULTIPOLYGON = "MultiPolygon"
    val GEOMETRYCOLLECTION = "GeometryCollection"
  }
}
