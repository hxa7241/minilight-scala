/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * Simple, explicit/non-vertex-shared, triangle.
 *
 * Includes geometry and quality.
 *
 * @implementation
 * Adapts ray intersection code from:
 * 'Fast, Minimum Storage Ray-Triangle Intersection'; Moller, Trumbore; 1997.
 * (Journal of Graphics Tools, v2 n1 p21)
 * [[http://www.acm.org/jgt/papers/MollerTrumbore97/]]
 *
 * @constant
 *
 * @invariants
 *  - vertexs_m.length == 3
 *  - reflectivity_m >= 0 and <= 1
 *  - emitivity_m    >= 0
 *
 * @param modelFile_c  to read from
 */
final class Triangle( modelFile_c:hxa7241.general.TokenStream ) extends NotNull
{
/// queries --------------------------------------------------------------------

   /**
    * Axis-aligned bounding box of triangle.
    *
    * @return  lower corner and upper corner
    */
   def bound:(Vector3f,Vector3f) =
   {
      // calculate max and min across all vertexs
      val lower = (vertexs_m(0) clampedMax vertexs_m(1)) clampedMax vertexs_m(2)
      val upper = (vertexs_m(0) clampedMin vertexs_m(1)) clampedMin vertexs_m(2)

      // enlarge with some tolerance
      val tolerance = Vector3f( Triangle.TOLERANCE )
      ( lower - tolerance, upper + tolerance )
   }


   /**
    * Intersection point of ray with triangle.
    *
    * (Vector operations are manually inlined (is faster).)
    *
    * @param rayOrigin     ray origin
    * @param rayDirection  ray direction (unitized)
    *
    * @return  distance along ray
    */
   def intersection( rayOrigin:Vector3f, rayDirection:Vector3f ):Option[Double]
      =
   {
      // make vectors for two edges sharing vert0
      val e3x = vertexs_m(2).x - vertexs_m(0).x
      val e3y = vertexs_m(2).y - vertexs_m(0).y
      val e3z = vertexs_m(2).z - vertexs_m(0).z

      val e0x = vertexs_m(1).x - vertexs_m(0).x
      val e0y = vertexs_m(1).y - vertexs_m(0).y
      val e0z = vertexs_m(1).z - vertexs_m(0).z

      // begin calculating determinant -- also used to calculate U parameter
      //val pvec = rayDirection cross edge3
      val pvx = (rayDirection.y * e3z) - (rayDirection.z * e3y)
      val pvy = (rayDirection.z * e3x) - (rayDirection.x * e3z)
      val pvz = (rayDirection.x * e3y) - (rayDirection.y * e3x)

      // if determinant is near zero, ray lies in plane of triangle
      //val det = edge0 dot pvec
      val det = (e0x * pvx) + (e0y * pvy) + (e0z * pvz)
      if( (det > -Triangle.EPSILON) & (det < Triangle.EPSILON) )
         None
      else
      {
         val inv_det = 1.0 / det

         // calculate distance from vertex 0 to ray origin
         //val tvec = rayOrigin - vertexs_m(0)
         val tvx = rayOrigin.x - vertexs_m(0).x
         val tvy = rayOrigin.y - vertexs_m(0).y
         val tvz = rayOrigin.z - vertexs_m(0).z

         // calculate U parameter and test bounds
         //val u = (tvec dot pvec) * inv_det
         val u = ((tvx * pvx) + (tvy * pvy) + (tvz * pvz)) * inv_det
         if( (u < 0.0) | (u > 1.0) )
            None
         else
         {
            // prepare to test V parameter
            //val qvec = tvec cross edge0
            val qvx = (tvy * e0z) - (tvz * e0y)
            val qvy = (tvz * e0x) - (tvx * e0z)
            val qvz = (tvx * e0y) - (tvy * e0x)

            // calculate V parameter and test bounds
            //val v = (rayDirection dot qvec) * inv_det
            val v = ((rayDirection.x * qvx) + (rayDirection.y * qvy) +
               (rayDirection.z * qvz)) * inv_det
            if( (v < 0.0) | (u + v > 1.0) )
               None
            else
            {
               // calculate t, ray intersects triangle
               //val hitDistance = (edge3 dot qvec) * inv_det
               val hitDistance = ((e3x * qvx) + (e3y * qvy) + (e3z * qvz)) *
                  inv_det

               // only allow intersections in the forward ray direction
               if(hitDistance >= 0.0) Some( hitDistance ) else None
            }
         }
      }
   }


   /**
    * Monte-carlo sample point on triangle.
    *
    * @param random  random number generator
    *
    * @return  sample point
    */
   def samplePoint( random:hxa7241.general.Random ):Vector3f =
   {
      // get two randoms
      val (sqr1, r2) = (math.sqrt(random.real64), random.real64)

      // make barycentric coords
      val (a, b) = ( (1.0 - sqr1), ((1.0 - r2) * sqr1) )

      // interpolate position by scaling edges by barycentric coords
      (edge0 * a) + (edge3 * b) + vertexs_m(0)
   }


   def normal  = (edge0 cross edge1).unitized

   def tangent = edge0.unitized

   // (half area of parallelogram)
   def area    = 0.5 * (edge0 cross edge1).length


/// implementation -------------------------------------------------------------

   private def edge0 = vertexs_m(1) - vertexs_m(0)
   private def edge1 = vertexs_m(2) - vertexs_m(1)
   private def edge3 = vertexs_m(2) - vertexs_m(0)


/// fields ---------------------------------------------------------------------

   private val vertexs_m = Array.fill(3)( Vector3f(modelFile_c) )

   val reflectivity = Vector3f(modelFile_c).clamped01
   val emitivity    = Vector3f(modelFile_c).clampedMin( Vector3f.ZERO )
}




object Triangle
{
/// constants ------------------------------------------------------------------

   // General tolerance of 1 mm seems reasonable.
   private[minilight] final val TOLERANCE = 1.0 / 1024.0

   // Epsilon suitable for at least single precision.
   private final val EPSILON = 1.0 / 1048576.0
}
