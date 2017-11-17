package geo.utils

import com.vividsolutions.jts.geom._
import geo.model.LatLng
import geo.model.SingleCoordinate
import geo.model.MultiCoordinate
import geo.model.TripleCoordinate
import geo.model.QuadrupleCoordinate

/**
  * https://tools.ietf.org/html/rfc7946#section-1.4
  */
object JTSGeoJson {
  private val factory = new GeometryFactory()

  import geo.model.Geometry.Types._

  def latLng(latLng:LatLng):Coordinate = new Coordinate(latLng.lng,latLng.lat)

  def multigeometry(geoms: Seq[geo.model.Geometry]): Option[GeometryCollection] = {
    val geometries = geoms.map(geometry).flatten
    if(geometries.length > 0) Some(factory.createGeometryCollection(geometries.toArray)) else None
  }

  def geometry(geom: geo.model.Geometry):Option[Geometry] = {

    geom.coordinates match {
      case sc:SingleCoordinate => position(sc.latLng,geom.`type`)
      case mc:MultiCoordinate => positions(mc.positions,geom.`type`)
      case tc:TripleCoordinate => multipositions(tc.positions,geom.`type`)
      case tc:QuadrupleCoordinate => multimultipositions(tc.positions,geom.`type`)
    }
  }

  private def position(latLngOpt:Option[LatLng],`type`:String):Option[Geometry] = `type` match {
    case POINT => latLngOpt.map( ll => factory.createPoint(latLng(ll)))
    case _ => None
  }

  private def positions(positions:Seq[LatLng],`type`:String):Option[Geometry] = `type` match {
    case MULTIPOINT if positions.length > 0 => Some{
      factory.createMultiPoint(positions.map(latLng).toArray)
    }
    case LINESTRING if positions.length > 1 => Some{
      factory.createLineString(positions.map(latLng).toArray)
    }
    case _ => None
  }

  private def multipositions(multipositions:Seq[Seq[LatLng]],`type`:String):Option[Geometry] = `type` match {
    case MULTILINESTRING => Some{
      factory.createMultiLineString(multipositions.map(positions(_,LINESTRING).asInstanceOf[LineString]).toArray)
    }
    case POLYGON => multipositions.toList match {
      case x :: tail => Some{
        val linearRing = factory.createLinearRing(x.map(latLng).toArray)
        val holes = tail.map(x => factory.createLinearRing(x.map(latLng).toArray)).toArray
        factory.createPolygon(linearRing,holes)
      }
      case Nil => None
    }
  }

  private def multimultipositions(multimultipositions:Seq[Seq[Seq[LatLng]]],`type`:String):Option[Geometry] = `type` match {
    case MULTIPOLYGON => Some{
      val polygons = multimultipositions.flatMap(multipositions(_,POLYGON)).map(_.asInstanceOf[Polygon]).toArray
      factory.createMultiPolygon(polygons)
    }
    case _ => None
  }

  def toLatLng(c:Coordinate):LatLng = LatLng(c.y.toFloat,c.x.toFloat)

  def geoJson(geoms:Seq[Geometry]):Seq[geo.model.Geometry] = geoms.map(geoJson)

  def geoJson(geom:Geometry):geo.model.Geometry = {
    geom match {
      case g:Point => geo.model.Geometry(POINT,SingleCoordinate(toLatLng(g.getCoordinate).coordinates))
      case g:MultiPoint => geo.model.Geometry(MULTIPOINT,MultiCoordinate(g.getCoordinates.map(toLatLng).map(_.coordinates)))
      case g:LineString => geo.model.Geometry(LINESTRING,MultiCoordinate(g.getCoordinates.map(toLatLng).map(_.coordinates)))
      case g:MultiLineString => {
        val coords =for( i <- 0 to g.getNumGeometries) yield {
          g.getGeometryN(i).getCoordinates.map(toLatLng).map(_.coordinates).toSeq
        }
        geo.model.Geometry(MULTILINESTRING,TripleCoordinate(coords))
      }
      case g:Polygon => {

        val holes = for( i <- 0 to g.getNumInteriorRing) yield {
          g.getInteriorRingN(i).getCoordinates
        }
        val rings = Seq(g.getExteriorRing.getCoordinates) ++ holes
        val tripleCoordinate = rings.map(_.toSeq.map(toLatLng).map(_.coordinates))
        geo.model.Geometry(POLYGON,TripleCoordinate(tripleCoordinate))
      }
      case g:MultiPolygon => {
        val polygons = for(i <- 0 to g.getNumGeometries) yield g.getGeometryN(i)
        val quadCords = geoJson(polygons).map(_.coordinates match {
          case TripleCoordinate(cords) => cords
          case _ => Seq(Seq(Seq[Float]()))
        })
        geo.model.Geometry(MULTIPOLYGON,QuadrupleCoordinate(quadCords))
      }
      case g:GeometryCollection => ???
    }
  }

}
