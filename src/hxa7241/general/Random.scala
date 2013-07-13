/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.general




/**
 * Simple, fast, good random number generator.
 *
 * @implementation
 *
 * 'Maximally Equidistributed Combined Tausworthe Generators'; L'Ecuyer; 1996.
 * http://www.iro.umontreal.ca/~lecuyer/myftp/papers/tausme2.ps
 * http://www.iro.umontreal.ca/~simardr/rng/lfsr113.c
 *
 * 'Conversion of High-Period Random Numbers to Floating Point'; Doornik; 2006.
 * http://www.doornik.com/research/randomdouble.pdf
 */
final class Random() extends NotNull
{
/// queries --------------------------------------------------------------------

   def int32 :Int =
   {
      z_m(0) = ((z_m(0) &   -2) << 18) ^ (((z_m(0) <<  6) ^ z_m(0)) >>> 13)
      z_m(1) = ((z_m(1) &   -8) <<  2) ^ (((z_m(1) <<  2) ^ z_m(1)) >>> 27)
      z_m(2) = ((z_m(2) &  -16) <<  7) ^ (((z_m(2) << 13) ^ z_m(2)) >>> 21)
      z_m(3) = ((z_m(3) & -128) << 13) ^ (((z_m(3) <<  3) ^ z_m(3)) >>> 12)

      z_m(0) ^ z_m(1) ^ z_m(2) ^ z_m(3)
   }


   /**
    * Single precision, [0,1) interval (never returns 1).
    */
   //def real32 =
   //   (int32 & 0xFFFFFF00).toFloat * (1.0f / 4294967296.0f) + 0.5f


   /**
    * Double precision, [0,1) interval (never returns 1).
    */
   def real64 =
      int32.toDouble * (1.0 / 4294967296.0) + 0.5 +
         (int32 & 0x001FFFFF).toDouble * (1.0 / 9007199254740992.0)


   /**
    * Double precision, (0,1) interval (never returns 0 or 1).
    */
   //def real64_ =
   //   int32.toDouble * (1.0 / 4294967296.0) +
   //      (0.5 + (1.0  / 4503599627370496.0) * 0.5) +
   //      (int32 & 0x000FFFFF).toDouble * (1.0  / 4503599627370496.0)


/// fields ---------------------------------------------------------------------

   /*private val z_m :Array[Int] =
   {
      // get UUID in 32-bit chunks
      val u32s :Array[Long] =
      {
         val u  = java.util.UUID.randomUUID
         val ul = Array( u.getLeastSignificantBits, u.getMostSignificantBits )
         Array.tabulate( 4 )( i => (ul(i >> 1) >> (32 * (i & 1))) & 0xFFFFFFFFL)
      }
      // *** VERY IMPORTANT ***
      // The initial seeds z1, z2, z3, z4  MUST be larger
      // than 1, 7, 15, and 127 respectively.
      Array.tabulate( 4 )( i =>
         if (u32s(i) >= Random.SEED_MINS(i)) u32s(i).toInt else Random.SEED )
   }*/

   /* *** VERY IMPORTANT ***
      The initial seeds z1, z2, z3, z4  MUST be larger
      than 1, 7, 15, and 127 respectively. */
   private val z_m :Array[Int] = Array.fill( 4 )( Random.SEED )

   //val id = "%08X".format( z_m(3) )
}




object Random
{
/// constants ------------------------------------------------------------------

   // default seed and seed minimums
   private final val SEED :Int = 987654321
   //private final val SEED_MINS = Array( 2, 8, 16, 128 )
}
