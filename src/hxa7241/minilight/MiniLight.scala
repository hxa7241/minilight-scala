/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.general.{TokenStream, Random}




/**
 * Control-module and entry point.
 *
 * Handles command-line UI, and runs the main progressive-refinement render
 * loop.
 *
 * Supply a model file pathname as the command-line argument. Or -? for help.
 */


object MiniLight
{
/// user messages --------------------------------------------------------------

   private val TITLE  = "MiniLight 1.6 Scala"
   private val AUTHOR = "Harrison Ainsworth / HXA7241 : 2008-2013"
   private val URI    = "http://www.hxa.name/minilight"
   private val DATE   = "2013-05-04"

   private val BANNER_MESSAGE = "\n  " + TITLE + " - " + URI + "\n"

   private val HELP_MESSAGE   = """
----------------------------------------------------------------------
  """ + TITLE + "\n\n  " + AUTHOR + "\n  " + URI + "\n\n  " + DATE + """
----------------------------------------------------------------------

MiniLight is a minimal global illumination renderer.

usage:
  minilight modelFilePathName

The model text file format is:
  #MiniLight

  iterations

  imagewidth imageheight
  viewposition viewdirection viewangle

  skyemission groundreflection

  vertex0 vertex1 vertex2 reflectivity emitivity
  vertex0 vertex1 vertex2 reflectivity emitivity
  ...

-- where iterations and image values are integers, viewangle is a real,
and all other values are three parenthised reals. The file must end
with a newline. Eg.:
  #MiniLight

  100

  200 150
  (0 0.75 -2) (0 0 1) 45

  (3626 5572 5802) (0.1 0.09 0.07)

  (0 0 0) (0 1 0) (1 1 0)  (0.7 0.7 0.7) (0 0 0)
"""


/// other declarations ---------------------------------------------------------

   private val MODEL_FORMAT_ID = "#MiniLight"




/// entry point ----------------------------------------------------------------

   def main( args:Array[String] )
   {
      // catch everything
      try
      {
         // check for help request
         if( args.isEmpty || ("-?" == args(0)) || ("--help"== args(0)) )
         {
            println( HELP_MESSAGE )
         }
         // execute
         else
         {
            println( BANNER_MESSAGE )

            // make random generator
            val random = new Random

            // get/make file names
            val modelFilePathname = args(0)
            val imageFilePathname = modelFilePathname + ".ppm"

            // open model file
            val modelFile = new TokenStream( modelFilePathname )

            // check model file format identifier at start of first line
            if( MODEL_FORMAT_ID != (modelFile.next + modelFile.next) )
               throw new Exception( "unrecognised model format" )

            // read frame iterations
            val iterations = modelFile.next.toInt

            // create main rendering objects, from model file
            val image  = new Image ( modelFile )
            val camera = new Camera( modelFile )
            val scene  = new Scene ( modelFile, camera.eyePoint )

            modelFile.close()

            // setup interruption/ctrl-c handler
            // (will also run at successful exit)
            val sigintHandler = new Thread()
               {
                  override def run() { println( "\ninterrupted" ) }
               }
            Runtime.getRuntime().addShutdownHook( sigintHandler )

            // do progressive refinement render loop
            for( frameNumber <- 1 to iterations )
            {
               // display latest frame number
               Console.out.append( "\riteration: " + frameNumber ).flush

               // render a frame
               camera.getFrame( scene, random, image )

               // save image at twice error-halving rate, and at start and end
               if( ((frameNumber & (frameNumber - 1)) == 0) |
                  (iterations == frameNumber) )
               {
                  // open file, write, close
                  val imageFile = new java.io.PrintStream( imageFilePathname )
                  image.formatted( imageFile, frameNumber )
                  imageFile.close()
               }
            }

            Runtime.getRuntime().removeShutdownHook( sigintHandler )
            println( "\nfinished" )
        }
      }
      // print any exception message
      catch
      {
         case e:Throwable => println( "\n*** execution failed: " + e )
      }
   }
}
