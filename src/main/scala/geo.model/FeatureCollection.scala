package geo.model

case class FeatureCollection(`type`:String, crs:Crs, features:Seq[Feature])

case class Crs(`type`:String, properties:CrsProperties)

case class CrsProperties(name:String)

case class Feature(`type`:String, properties:Map[String,Any], geometry: Geometry)

object Feature{
  def fromGeom(name:String,geometry: Geometry):Feature = {
    Feature("Feature",Map("name" -> name),geometry)
  }
}

object FeatureCollection{
  def fromGeom(geometries:Seq[(String,Geometry)]):FeatureCollection = {
    FeatureCollection(
      "FeatureCollection",
      Crs("name",CrsProperties("urn:ogc:def:crs:OGC:1.3:CRS84")),
      geometries.map{ case (name,geom) => Feature.fromGeom(name,geom)}
    )

  }
}
