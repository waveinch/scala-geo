package geo.utils

import geo.model._
import play.api.libs.json._

object PlayFormatters {


  val coordinateReads = new Reads[Coordinates] {
    override def reads(json: JsValue) = {
      (json.asOpt[Seq[Float]],json.asOpt[Seq[Seq[Float]]],json.asOpt[Seq[Seq[Seq[Float]]]]) match {
        case (Some(sc),_,_) => JsSuccess(SingleCoordinate(sc))
        case (_,Some(mc),_) => JsSuccess(MultiCoordinate(mc))
        case (_,_,Some(tc)) => JsSuccess(TripleCoordinate(tc))
        case (_,_,_) => JsError("not found coordinate deserializer")
      }
    }
  }

  val coordinateWrites: Writes[Coordinates] = new Writes[Coordinates] {
    override def writes(c: Coordinates) = c match {
      case mc:MultiCoordinate => (Json.format[MultiCoordinate].writes(mc) \ "coordinates").as[JsValue]
      case sc:SingleCoordinate => (Json.format[SingleCoordinate].writes(sc) \ "coordinates").as[JsValue]
      case sc:TripleCoordinate => (Json.format[TripleCoordinate].writes(sc) \ "coordinates").as[JsValue]
    }
  }

  implicit val coordinateFormat: Format[Coordinates] = Format(coordinateReads, coordinateWrites)

  implicit val mapStringAnyFormatter = new Format[Map[String,Any]] {
    override def reads(json: JsValue): JsResult[Map[String,Any]] = json.asOpt[JsObject].map{ jsobj =>
      val map:Map[String,Any] = jsobj.value.map{ case (k,v) => k -> v.toString().asInstanceOf[Any]}.toMap
      JsSuccess(map)
    }.getOrElse(JsError())
    override def writes(o: Map[String,Any]): JsValue = Json.toJson(o.toString)
  }

  implicit val geometry = Json.format[Geometry]
  implicit val crsPropertiesF = Json.format[CrsProperties]
  implicit val crsF = Json.format[Crs]
  implicit val featureF = Json.format[Feature]
  implicit val featureCollectionF = Json.format[FeatureCollection]

}