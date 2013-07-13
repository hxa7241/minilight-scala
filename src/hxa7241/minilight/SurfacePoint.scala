/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * Surface point at a ray-object intersection.
 *
 * All direction parameters are away from surface.
 *
 * @constant
 *
 * @param triangle_c  surface's object
 * @param position_c  position of point on surface
 */
class SurfacePoint( triangle_c:Triangle, position_c:Vector3f ) extends NotNull
{
/// queries --------------------------------------------------------------------

   /**
    * Emission from surface element to point.
    *
    * @param toPosition    point being illuminated
    * @param outDirection  direction (unitized) from emitting point
    * @param isSolidAngle  use solid angle
    *
    * @return  emitted radiance
    */
   def emission( toPosition:Vector3f, outDirection:Vector3f,
      isSolidAngle:Boolean ):Vector3f =
   {
      val distance2 = { val ray = toPosition - position;  ray dot ray }
      val cosArea   = (outDirection dot triangle_m.normal) * triangle_m.area

      // with infinity clamped out
      val solidAngle = if( isSolidAngle )
         cosArea / (distance2 max 1e-6) else 1.0

      // emit from front face of surface only
      if( cosArea > 0.0 ) triangle_m.emitivity * solidAngle else Vector3f.ZERO
   }


   /**
    * Light reflection from ray to ray by surface.
    *
    * @param inDirection   negative of inward ray direction
    * @param inRadiance    inward radiance
    * @param outDirection  outward (eyeward) ray direction
    *
    * @return  reflected radiance
    */
   def reflection( inDirection:Vector3f, inRadiance:Vector3f,
      outDirection:Vector3f ):Vector3f =
   {
      val inDot  = inDirection  dot triangle_m.normal
      val outDot = outDirection dot triangle_m.normal

      // directions must be on same side of surface (no transmission)
      if( (inDot < 0.0) ^ (outDot < 0.0) )
         Vector3f.ZERO
      else
         // ideal diffuse BRDF:
         // radiance scaled by reflectivity, cosine, and 1/pi
         (inRadiance * triangle_m.reflectivity) * (inDot.abs / math.Pi)
   }


   /**
    * Monte-carlo direction of reflection from surface.
    *
    * @param inDirection  eyeward ray direction
    * @param random       random number generator
    *
    * @return sceneward ray direction (unitized) and light scaling of
    *         interaction point
    */
   def nextDirection( inDirection:Vector3f, random:hxa7241.general.Random )
      :Option[(Vector3f,Vector3f)] =
   {
      val reflectivityMean = (triangle_m.reflectivity dot Vector3f.ONE) / 3.0

      // russian-roulette for reflectance 'magnitude'
      if( random.real64 < reflectivityMean )
      {
         // cosine-weighted importance sample hemisphere

         val _2pr1 = math.Pi * 2.0 * random.real64
         val sr2   = math.sqrt( random.real64 )

         // make coord frame coefficients (z in normal direction)
         val x = math.cos( _2pr1 ) * sr2
         val y = math.sin( _2pr1 ) * sr2
         val z = math.sqrt( 1.0 - (sr2 * sr2) )

         // make coord frame
         val tangent = triangle_m.tangent
         val normal  =
         {
            // put normal on inward ray side of surface
            // (preventing transmission)
            val n = triangle_m.normal
            if( (n dot inDirection) >= 0.0 ) n else -n
         }

         // make vector from frame scaled by coefficients
         val outDirection = (tangent * x) + ((normal cross tangent) * y) +
            (normal * z)

         // make color by dividing-out mean from reflectivity
         val color = triangle_m.reflectivity * (1.0 / reflectivityMean)

         if( !outDirection.isZero ) Some( (outDirection, color) ) else None
      }
      else
         None
   }


   def hitObject:AnyRef = triangle_m


/// fields ---------------------------------------------------------------------

   private val triangle_m = triangle_c
           val position   = position_c
}
