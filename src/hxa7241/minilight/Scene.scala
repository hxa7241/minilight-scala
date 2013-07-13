/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * Collection of objects in the environment.
 *
 * Makes a sub-grouping of emitting objects.
 *
 * @constant
 *
 * @invariants
 *  - skyEmission_m      >= 0
 *  - groundReflection_m >= 0
 *  - triangles_m length <= TRIANGLES_MAX
 *  - emitters_m length  <= TRIANGLES_MAX
 *
 * @param modelFile_c    to read from
 * @param eyePosition_c  eye position
 */
class Scene( modelFile_c:hxa7241.general.TokenStream, eyePosition_c:Vector3f )
   extends NotNull
{
/// queries --------------------------------------------------------------------

   /**
    * Nearest intersection of ray with object.
    *
    * @param rayOrigin     ray origin
    * @param rayDirection  ray direction (unitized)
    * @param lastHit       previous intersected object, or null
    *
    * @return  triangle and position
    */
   def intersection( rayOrigin:Vector3f, rayDirection:Vector3f,
      lastHit:AnyRef ):Option[(Triangle,Vector3f)] =
   {
      spatialIndex_m.intersection( rayOrigin, rayDirection, lastHit )
   }


   /**
    * Monte-carlo sample point on monte-carlo selected emitting object.
    *
    * @param random  random number generator
    *
    * @return  triangle and position
    */
   def emitter( random:hxa7241.general.Random ):Option[(Triangle,Vector3f)] =
   {
      if( emittersCount > 0 )
      {
         // select emitter
         val emitter = emitters_m( math.floor(random.real64 *
            emittersCount.toDouble).toInt min (emittersCount - 1) )

         // get position on triangle
         Some( (emitter, emitter.samplePoint( random )) )
      }
      else
         None
   }


   /**
    * Number of emitters in scene.
    *
    * @return  number of emitters
    */
   def emittersCount = emitters_m.length


   /**
    * Default/'background' light of scene universe.
    *
    * @param eyeDirection  direction to eye
    *
    * @return  emitted radiance
    */
   def defaultEmission( eyeDirection:Vector3f ):Vector3f =
   {
      // sky for downward ray, ground for upward ray
      if( eyeDirection.y < 0.0f ) skyEmission_m else groundReflection_m
   }


/// fields ---------------------------------------------------------------------

   // default scene background
   private val skyEmission_m = Vector3f(modelFile_c).clampedMin( Vector3f.ZERO )
   private val groundReflection_m = Vector3f(modelFile_c).clamped01 *
      skyEmission_m

   // main objects
   private val triangles_m =
   {
      def readTriangle( ts:List[Triangle], i:Int ):List[Triangle] =
      {
         try
         {
            if( i != 0 )
               readTriangle( new Triangle(modelFile_c) :: ts, i - 1 ) else ts
         }
         catch
         {
            // EOF is not really exceptional here, but the code is simpler
            case _:java.io.EOFException => ts
         }
      }
      // read up to maximum
      readTriangle( Nil, Scene.TRIANGLES_MAX ).toArray
   }
   // find emitting triangles
   private val emitters_m = triangles_m.filter(
      // has non-zero emission and area
      t => (!t.emitivity.isZero) && (t.area > 0.0f) )

   // acceleration index
   private val spatialIndex_m = new SpatialIndex( eyePosition_c, triangles_m )
}




object Scene
{
/// constants ------------------------------------------------------------------

   // 2^24 ~= 16 million
   final val TRIANGLES_MAX = 0x1000000
}
