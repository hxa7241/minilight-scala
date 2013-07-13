/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f



/**
 * Pixel sheet with simple tone-mapping and file formatting.
 *
 * @implementation
 * Uses PPM image format:
 * [[http://netpbm.sourceforge.net/doc/ppm.html]]
 *
 * @implementation
 * Uses Ward simple tonemapper:
 * 'A Contrast Based Scalefactor For Luminance Display'; Ward; 1994.
 * (Graphics Gems 4, AP)
 *
 * @mutable
 *
 * @invariants
 *  - width  >= 1 and <= DIMENSION_MAX
 *  - height >= 1 and <= DIMENSION_MAX
 *  - pixels_m.length == (width * height)
 *
 * @param modelFile_c  to read from
 */
class Image( modelFile_c:hxa7241.general.TokenStream ) extends NotNull
{
/// commands -------------------------------------------------------------------

   /**
    * Accumulate (add, not just assign) a value to the image.
    *
    * @param x         x coord
    * @param y         y coord
    * @param radiance  light value
    */
   def addToPixel( x:Int, y:Int, radiance:Vector3f )
   {
      // only inside image bounds
      if( (x >= 0) & (x < width) & (y >= 0) & (y < height) )
         pixels_m(x + ((height - 1 - y) * width)) += radiance
   }


/// queries --------------------------------------------------------------------

   /**
    * Format the image.
    *
    * @param out        to receive the image
    * @param iteration  number of accumulations made to the image
    */
   def formatted( out:java.io.PrintStream, iteration:Int )
   {
      // make pixel value accumulation divider, and tonemap scaling
      val divider        = 1.0 / (iteration max 1)
      val tonemapScaling = toneMapping( divider )

      // write PPM P6 format:

      // write ID and comment
      out.print( "P6\n# " + Image.MINILIGHT_URI + "\n\n" )

      // write width, height, maxval
      out.print( width + " " + height + "\n" + 255 + "\n" )

      // write pixels
      for( pixel <- pixels_m;  c <- 0 to 2;  channel = pixel(c) )
      {
         // tonemap and gamma encode
         val mapped  = (channel * divider * tonemapScaling) max 0.0
         val gammaed = math.pow( mapped, Image.GAMMA_ENCODE )

         // quantize and output as byte
         out.write( (((gammaed * 255.0) + 0.5) min 255.0).toByte )
      }
   }


/// implementation -------------------------------------------------------------

   /**
    * Calculate tone-mapping scaling factor.
    *
    * @param divider  pixel scaling
    *
    * @return  scaling factor
    */
   private def toneMapping( divider:Double ):Double =
   {
      // calculate estimate of world-adaptation luminance
      // as log mean luminance of scene
      val adaptLuminance =
      {
         // sum
         val sumOfLogs = pixels_m.foldLeft( 0.0 )( (sum, pixel) =>
            {
               val y = (pixel dot Image.RGB_LUMINANCE) * divider
               // clamp luminance to a perceptual minimum
               sum + math.log10( y max 1e-4 )
            } )
         // mean
         math.pow( 10.0, (sumOfLogs / pixels_m.length) )
      }

      // make scale-factor from:
      // ratio of minimum visible differences in luminance, in display-adapted
      // and world-adapted perception (discluding the constant that cancelled),
      // divided by display max to yield a [0,1] range
      val a = 1.219 + math.pow( (Image.DISPLAY_LUMINANCE_MAX * 0.25), 0.4 )
      val b = 1.219 + math.pow( adaptLuminance,                       0.4 )

      math.pow( (a / b), 2.5 ) / Image.DISPLAY_LUMINANCE_MAX
   }


/// fields ---------------------------------------------------------------------

   val width  = (modelFile_c.next.toInt max 1) min Image.DIMENSION_MAX
   val height = (modelFile_c.next.toInt max 1) min Image.DIMENSION_MAX

   private val pixels_m = Array.fill( width * height )( Vector3f() )
}




object Image
{
/// constants ------------------------------------------------------------------

   // image dimension max
   final val DIMENSION_MAX = 4000

   // image file comment
   private final val MINILIGHT_URI = "http://www.hxa.name/minilight"

   // guess of average screen maximum brightness
   private final val DISPLAY_LUMINANCE_MAX = 200.0
   // ITU-R BT.709 standard RGB luminance weighting
   private       val RGB_LUMINANCE         = Vector3f( 0.2126, 0.7152, 0.0722 )
   // ITU-R BT.709 standard gamma
   private final val GAMMA_ENCODE          = 0.45
}
