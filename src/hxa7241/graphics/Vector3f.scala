/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.graphics




/**
 * Yes, its the 3D vector class!.
 *
 * ...mostly the usual sort of stuff.
 * (Unused methods are commented out. They do work fine though.)
 *
 * @constant
 */
final class Vector3f( val x:Double, val y:Double, val z:Double ) extends NotNull
{
/// standard overrides----------------------------------------------------------

   // must enable this method if == or != is used
   //override def equals( v:Any ) = v match
   //{
   //   case v:Vector3f => (x == v.x) & (y == v.y) & (z == v.z)
   //   case _          => false
   //}
   //override def hashCode = (x, y, z).hashCode

   //override def toString = "(" + x + " " + y + " " + z + ")"
   //def toArray = Array( x, y, z )


/// queries --------------------------------------------------------------------

/// elements
   def apply( i:Int ):Double = i match { case 2 => z; case 1 => y; case 0 => x }

/// => Double
   //def sum      =  x + y + z
   //def average  = (x + y + z) * (1.0 / 3.0)
   //def smallest = x min (y min z)
   //def largest  = x max (y max z)

   def length            = math.sqrt( this dot this )
   def dot( v:Vector3f ) = (x * v.x) + (y * v.y) + (z * v.z)
   //def distance( v:Vector3f ) = (this - v).length

/// => vector3f
   def unary_-  = new Vector3f( -x, -y, -z )
   def abs      = new Vector3f( x.abs, y.abs, z.abs )
   // Zero vectors, and vectors of near zero magnitude, return zero vectors;
   // Vectors of extremely large magnitude return zero vectors:
   def unitized = if( length != 0.0 ) this * (1.0 / length) else Vector3f.ZERO
   def cross( v:Vector3f ) = new Vector3f(
      (y * v.z) - (z * v.y),
      (z * v.x) - (x * v.z),
      (x * v.y) - (y * v.x) )

   def +( v:Vector3f ) = new Vector3f( (x + v.x), (y + v.y), (z + v.z) )
   def -( v:Vector3f ) = new Vector3f( (x - v.x), (y - v.y), (z - v.z) )
   def *( v:Vector3f ) = new Vector3f( (x * v.x), (y * v.y), (z * v.z) )
   //def /( v:Vector3f ) = new Vector3f( (x / v.x), (y / v.y), (z / v.z) )
   def *( f:Double )   = new Vector3f( (x * f),   (y * f),   (z * f)   )
   //def /( f:Double )   = this * (1.0 / f)

/// logical
   def isZero = (x == 0.0) & (y == 0.0) & (z == 0.0)

/// clamp
   def clampedMin( lower:Vector3f )           = zip( math.max, lower )
   def clampedMax( upper:Vector3f )           = zip( math.min, upper )
   //def clamped   ( lo:Vector3f, up:Vector3f ) = clampedMin(lo).clampedMax(up)
   def clamped01 = clampedMin(Vector3f.ZERO).clampedMax(Vector3f.ONE)

/// comprehensions
   def fold( f:(Double,Double)=>Double ):Double = f( f(x, y), z )
   def zip ( f:(Double,Double)=>Double, v:Vector3f ):Vector3f =
      new Vector3f( f(x, v.x), f(y, v.y), f(z, v.z) )
}




object Vector3f
{
/// constructors ---------------------------------------------------------------

   def apply()                               = new Vector3f( 0.0, 0.0, 0.0 )
   def apply( n:Double )                     = new Vector3f( n, n, n )
   def apply( x:Double, y:Double, z:Double ) = new Vector3f( x, y, z )

   def apply( modelFile:hxa7241.general.TokenStream ) =
   {
      val a = Array.fill(5)( modelFile.next ).slice(1,4).map( _.toDouble )
      new Vector3f( a(0), a(1), a(2) )
   }


/// constants ------------------------------------------------------------------

   val ZERO       = Vector3f( 0.0 )
   //val HALF       = Vector3f( 0.5 )
   val ONE        = Vector3f( 1.0 )
   //val EPSILON    = Vector3f( Double.MinPositiveValue )
   //val ALMOST_ONE = Vector3f( 1.0 - Double.MinPositiveValue )
   //val MINIMUM    = Vector3f( -Double.MaxValue )
   //val MAXIMUM    = Vector3f( Double.MaxValue )
   //val SMALL      = Vector3f( 1e-12 )
   //val LARGE      = Vector3f( 1e+12 )
   //val SMALL_48   = Vector3f( 1.0 / (65536.0 * 65536.0 * 65536.0) )
   //val LARGE_48   = Vector3f( 65536.0 * 65536.0 * 65536.0 )
   //val X          = Vector3f( 1.0, 0.0, 0.0 )
   //val Y          = Vector3f( 0.0, 1.0, 0.0 )
   //val Z          = Vector3f( 0.0, 0.0, 1.0 )
}
