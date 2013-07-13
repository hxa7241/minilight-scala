/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.general


import java.io.{FileReader, BufferedReader, StreamTokenizer, EOFException}




/**
 * Make some dismal Java file-reading things half-usable.
 */
class TokenStream( pathname_c:String ) extends NotNull
{
/// commands -------------------------------------------------------------------

   def close() { file_m.close() }


/// queries --------------------------------------------------------------------

   def next:String =
   {
      tokens_m.nextToken()
      if( tokens_m.ttype == StreamTokenizer.TT_EOF ) throw new EOFException
      if( tokens_m.ttype < 0 ) tokens_m.sval else "" + tokens_m.ttype.toChar
   }


/// fields ---------------------------------------------------------------------

   private val file_m   = new BufferedReader( new FileReader( pathname_c ) )
   private val tokens_m = new StreamTokenizer( file_m )

   tokens_m.ordinaryChars( 42, 122 );
   tokens_m.wordChars( 42, 122 )
}
