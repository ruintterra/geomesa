package org.locationtech.geomesa.fs.storage.orc.utils

import org.locationtech.jts.geom.{Envelope, Geometry}
import org.locationtech.sfcurve.zorder.ZCurve2D

case class GeometryIndexer(res:Int) {
  private val curve = new ZCurve2D(res)

  def indices(geom:Geometry): Seq[Long] = {
    val env = geom.getEnvelopeInternal
    curve.toRanges(env.getMinX, env.getMinY, env.getMaxX, env.getMaxY)
      .flatMap(r => r.lower to r.upper)
      .filter(i => curve.intersects(i, geom))
  }

  private implicit class ZCurve2DImplicits(curve: ZCurve2D) {
    def intersects(i: Long, geom: Geometry): Boolean = {
      val (minx, miny, maxx, maxy) = curve.bound(i)
      val env = new Envelope(minx, maxx, miny, maxy)
      geom.intersects(geom.getFactory.toGeometry(env))
    }
  }
}
