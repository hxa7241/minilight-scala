/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * Ray tracer for general light transport.
 *
 * Traces a path with emitter sampling each step: A single chain of ray-steps
 * advances from the eye into the scene with one sampling of emitters at each
 * node.
 *
 * @constant
 *
 * @param scene_c  collection of objects
 */
class RayTracer( scene_c:Scene ) extends NotNull
{
/// queries --------------------------------------------------------------------

   /**
    * Radiance returned from a trace.
    *
    * @param rayOrigin     ray start point
    * @param rayDirection  ray direction (unitized)
    * @param random        random number generator
    * @param lastHit       previous intersected object, or null
    *
    * @return  radiance
    */
   def radiance( rayOrigin:Vector3f, rayDirection:Vector3f,
      random:hxa7241.general.Random, lastHit:AnyRef = null ):Vector3f =
   {
      // intersect ray with scene
      scene_m.intersection( rayOrigin, rayDirection, lastHit ) match
      {
         // hit
         case Some( (hitObject, hitPosition) ) =>
         {
            // make SurfacePoint of intersection
            val surfacePoint = new SurfacePoint( hitObject, hitPosition )

            // local emission (only for first-hit)
            val localEmission = if( lastHit != null ) Vector3f.ZERO else
               surfacePoint.emission( rayOrigin, -rayDirection, false )

            // emitter sample
            val illumination = emitterSample(rayDirection, surfacePoint, random)

            // recursive reflection:
            // single hemisphere sample, ideal diffuse BRDF:
            //    reflected = (inradiance * pi) * (cos(in) / pi * color) *
            //       reflectance
            // -- reflectance magnitude is 'scaled' by the russian roulette,
            // cos is importance sampled (both done by SurfacePoint),
            // and the pi and 1/pi cancel out -- leaving just:
            //    inradiance * reflectance color
            val reflection =
               // check surface reflects ray
               surfacePoint.nextDirection( -rayDirection, random ) match
               {
                  // recurse
                  case Some( (nextDirection, color) ) =>
                     color * radiance( surfacePoint.position, nextDirection,
                        random, surfacePoint.hitObject )
                  // end
                  case None => Vector3f.ZERO
               }

            // total radiance returned
            reflection + illumination + localEmission
         }
         // no hit: default/background scene emission
         case None => scene_m.defaultEmission( -rayDirection )
      }
   }


/// implementation -------------------------------------------------------------

   /**
    * Radiance from an emitter sample.
    *
    * @param rayDirection  previous ray direction (unitized)
    * @param surfacePoint  surface point receiving emission
    * @param random        random number generator
    *
    * @return  radiance
    */
   private def emitterSample( rayDirection:Vector3f, surfacePoint:SurfacePoint,
      random:hxa7241.general.Random ):Vector3f =
   {
      // single emitter sample, ideal diffuse BRDF:
      //    reflected = (emitivity * solidangle) * (emitterscount) *
      //       (cos(emitdirection) / pi * reflectivity)
      // -- SurfacePoint does the first and last parts (in separate methods)

      // check an emitter was found
      scene_m.emitter( random ) match
      {
         // emitter
         case Some( (emitter, emitPosition) ) =>
         {
            // make direction to emit point
            val emitDirection = (emitPosition - surfacePoint.position).unitized

            // send shadow ray to get light
            scene_m.intersection( surfacePoint.position, emitDirection,
               surfacePoint.hitObject ) match
            {
               // unshadowed
               case None | Some( (`emitter`, _) ) =>
               {
                  // get inward emission value
                  val emissionIn = new SurfacePoint( emitter, emitPosition
                     ).emission(surfacePoint.position, -emitDirection, true)

                  // get amount reflected by surface
                  surfacePoint.reflection( emitDirection,
                     (emissionIn * scene_m.emittersCount), -rayDirection )
               }
               // shadowed
               case _ => Vector3f.ZERO
            }
         }
         // no emitter
         case None => Vector3f.ZERO
      }
   }


/// fields ---------------------------------------------------------------------

   private val scene_m = scene_c
}
