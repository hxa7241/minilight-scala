/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * View definition and rasterizer.
 *
 * @constant
 *
 * @invariants
 *  - viewAngle_m is >= VIEW_ANGLE_MIN and <= VIEW_ANGLE_MAX degrees in radians
 *  - viewDirection_m is unitized
 *  - right_m is unitized
 *  - up_m is unitized
 *  - above three form a coordinate frame
 *
 * @param modelFile_c  to read from
 */
class Camera( modelFile_c:hxa7241.general.TokenStream ) extends NotNull
{
/// queries --------------------------------------------------------------------

   def eyePoint = viewPosition_m


   /**
    * Accumulate a frame of samples to the image.
    *
    * @param scene   scene to render
    * @param random  random number generator
    * @param image   image to add to
    */
   def getFrame( scene:Scene, random:hxa7241.general.Random, image:Image )
   {
      val rayTracer = new RayTracer( scene )

      // step through image pixels, sampling them
      for( y <- 0 until image.height;  x <- 0 until image.width )
      {
         // make sample ray direction, stratified by pixels
         val sampleDirection =
         {
            // make image plane XY displacement vector [-1,+1) coefficients,
            // with sub-pixel jitter
            val xF = ((x + random.real64) * 2.0 / image.width ) - 1.0
            val yF = ((y + random.real64) * 2.0 / image.height) - 1.0

            // make image plane offset vector,
            // by scaling the view definition by the coefficients
            val offset = (right_m * xF) + (up_m * (yF *
               (image.height.toDouble / image.width)))

            // add image offset vector to view direction
            (viewDirection_m + (offset * math.tan(viewAngle_m * 0.5))).unitized
         }

         // get radiance from RayTracer
         val radiance = rayTracer.radiance( viewPosition_m, sampleDirection,
            random )

         // add radiance to pixel
         image.addToPixel( x, y, radiance )
      }
   }


/// fields ---------------------------------------------------------------------

   // view definition
   private val viewPosition_m  = Vector3f( modelFile_c )
   private val viewDirection_m =
   {
      val vd = Vector3f( modelFile_c ).unitized
      if( !vd.isZero ) vd else Vector3f(0, 0, 1)
   }

   private val viewAngle_m = math.toRadians( (modelFile_c.next.toDouble
      max Camera.VIEW_ANGLE_MIN) min Camera.VIEW_ANGLE_MAX )

   // other directions of view coord frame
   private val (right_m, up_m) =
   {
      // make trial 'right', using viewDirection and assuming 'up' is Y
      val right = (Vector3f(0, 1, 0) cross viewDirection_m).unitized
      // check 'right' is valid
      // -- i.e. viewDirection was not co-linear with 'up'
      if( !right.isZero )
         // use 'right', and make 'up' properly orthogonal
         ( right, (viewDirection_m cross right).unitized )
      // else, assume a different 'up' and redo
      else
      {
         // 'up' is Z if viewDirection is down, otherwise -Z
         val up = Vector3f( 0, 0, if(viewDirection_m.y < 0.0) +1 else -1 )
         // remake 'right'
         ( (up cross viewDirection_m).unitized, up )
      }
   }
}




object Camera
{
/// constants ------------------------------------------------------------------

   // View angle max and min.
   final val VIEW_ANGLE_MIN =  10.0
   final val VIEW_ANGLE_MAX = 160.0
}
