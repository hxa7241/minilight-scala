/*------------------------------------------------------------------------------

   MiniLight Scala : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2008-2013

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*/


package hxa7241.minilight


import hxa7241.graphics.Vector3f




/**
 * A minimal spatial index for ray tracing.
 *
 * Suitable for a scale of 1 metre == 1 numerical unit, and has a resolution of
 * 1 millimetre. (Implementation uses fixed tolerances)
 *
 * @implementation
 * Octree: axis-aligned, cubical. Subcells are numbered thusly (in binary):
 * {{{
 *            110---111
 *            /|    /|
 *         010---011 |
 *    y z   | 100-|-101
 *    |/    | /   | /
 *    .-x  000---001
 * }}}
 *
 * Each cell stores its bound: fatter data, but simpler code.
 *
 * Calculations for building and tracing are absolute rather than incremental --
 * so quite numerically solid. Uses tolerances in: bounding items (in
 * Triangle.bound), and checking intersection is inside cell (both effective
 * for axis-aligned items). Also, depth is constrained to an absolute subcell
 * size (easy way to handle overlapping items).
 *
 * Sacrificed some clarity for compactness.
 *
 * @constant
 *
 * @invariants
 *  - bound_m length is 6
 *  - subParts_m is SpatialIndex.SubCells or SpatialIndex.Items
 */
final class SpatialIndex private (
   bound_c:Array[Double], items_c:Array[Triangle], level_c:Int ) extends NotNull
{
/// standard object services ---------------------------------------------------

   /**
    * Construct (publicly).
    *
    * @param eyePosition  position of eye
    * @param items        collection of all items
    */
   def this( eyePosition:Vector3f, items:Array[Triangle] ) = this(
      {
         // make overall bound
         val bound =
         {
            // accommodate all items, and eye position
            // (makes tracing algorithm simpler)
            val rectBound = items.foldLeft( (eyePosition, eyePosition) )(
               (rb, item) =>
               {
                  // accommodate item
                  val ib = item.bound
                  ( (rb._1 clampedMax ib._1), (rb._2 clampedMin ib._2) )
               } )
            // make cubical
            val cube = Vector3f( (rectBound._2 - rectBound._1).fold(math.max) )
            ( rectBound._1, (rectBound._2 clampedMin (rectBound._1 + cube)) )
         }

         // convert to array
         Array.range(0,6).map( i => (if(i < 3) bound._1 else bound._2)(i % 3) )

      // delegate to main (recursive) constructor
      }, items, 0 )


/// queries --------------------------------------------------------------------

   /**
    * Find nearest intersection of ray with item.
    *
    * @param rayOrigin     ray origin
    * @param rayDirection  ray direction (unitized)
    * @param lastHit       previous intersected item, or null
    * @param _start        current traversal point, or None
    *
    * @return  item and position
    */
   def intersection( rayOrigin:Vector3f, rayDirection:Vector3f, lastHit:AnyRef,
      _start:Option[Vector3f] = None ):Option[(Triangle,Vector3f)] =
   {
      subParts_m match
      {
         // branch: step through subcells and recurse
         case SpatialIndex.SubCells( subCells ) =>
         {
            val start = _start.getOrElse( rayOrigin )

            // find which subcell holds ray origin (ray origin is inside cell)
            val subCell =
               // compare dimensions with center
               (if( start.x >= ((bound_m(0) + bound_m(3)) * 0.5) ) 1 else 0) |
               (if( start.y >= ((bound_m(1) + bound_m(4)) * 0.5) ) 2 else 0) |
               (if( start.z >= ((bound_m(2) + bound_m(5)) * 0.5) ) 4 else 0)

            // define subcell walker
            def walk( subCell:Int, cellPosition:Vector3f )
               :Option[(Triangle,Vector3f)] =
            {
               // intersect subcell
               // if no hit, continue walking across subcells
               subCells(subCell).flatMap( _.intersection( rayOrigin,
                  rayDirection, lastHit, Some(start) ) ).orElse(
               {
                  // find next subcell ray moves to
                  // (by finding which face of corner ahead is crossed first)
                  val (step, axis) = (0 to 2).foldLeft((Double.MaxValue, 0))( {
                     case ((step, axis), i) =>
                     {
                        val high = (subCell >>> i) & 1
                        val face = if( (rayDirection(i) < 0.0) ^ (high == 1) )
                              bound_m(i + (high * 3))
                           else
                              (bound_m(i) + bound_m(i + 3)) * 0.5
                        // distance to face
                        // (div by zero produces infinity, which gets discarded)
                        val distance = (face - rayOrigin(i)) / rayDirection(i)
                        if( distance < step ) (distance, i) else (step, axis)
                     } } )

                  // leaving branch if: direction is negative and subcell is
                  // low, or direction is positive and subcell is high
                  if( (rayDirection(axis) < 0.0) ^
                     (((subCell >>> axis) & 1) == 1) )
                     None
                  else
                     // move to (outer face of) next subcell
                     walk( subCell ^ (1 << axis),
                        rayOrigin + (rayDirection * step) )
               } )
            }
            // step through intersected subcells
            walk( subCell, start )
         }
         // leaf: exhaustively intersect contained items
         case SpatialIndex.Items( items ) =>
         {
            // apply nearest-finder to items list
            items.foldLeft( (None:Option[(Triangle,Vector3f)],
               Double.PositiveInfinity) )( (nearest, item) =>
               {
                  // intersect item and inspect if nearest so far
                  val distance = item.intersection( rayOrigin, rayDirection
                     ).getOrElse( Double.PositiveInfinity )
                  // avoid spurious intersection with surface just come from
                  if( (distance < nearest._2) && (item != lastHit) )
                  {
                     val hit = rayOrigin + (rayDirection * distance)
                     // check intersection is inside cell bound (with tolerance)
                     val t = Triangle.TOLERANCE
                     if( (bound_m(0) - hit.x > t) | (hit.x - bound_m(3) > t) |
                         (bound_m(1) - hit.y > t) | (hit.y - bound_m(4) > t) |
                         (bound_m(2) - hit.z > t) | (hit.z - bound_m(5) > t) )
                        nearest else (Some((item, hit)), distance)
                  }
                  else
                     nearest
               } )._1
         }
      }
   }


/// fields ---------------------------------------------------------------------

   private val bound_m    = bound_c
   private val subParts_m =
   {
      // if items overflow leaf and tree not too deep,
      // make branch: make subcells, and recurse construction
      if( (items_c.length > SpatialIndex.MAX_ITEMS) &
         (level_c < (SpatialIndex.MAX_LEVELS - 1)) )
      {
         // make subcells
         var q1 = 0
         SpatialIndex.SubCells( for( subcellIndex <- Array.range(0,8) ) yield
         {
            // make subcell bound
            val subBound = for( i <- Array.range(0,6);  m = i % 3 ) yield
               if( (((subcellIndex >> m) & 1) ^ (i / 3)) != 0 )
                  (bound_c(m) + bound_c(m + 3)) * 0.5 else bound_c(i)

            // collect items that overlap subcell
            val subItems = for( i <- items_c;  (itemLo, itemHi) = i.bound;
               // must overlap in all dimensions
               if (0 to 5).foldLeft( true )( (b, j) => b & ((subBound(j) <=
                  (if(j > 2) itemLo else itemHi)(j % 3)) ^ (j > 2)) )
               ) yield i

            // curtail degenerate subdivision by adjusting next level
            // (degenerate if two or more subcells copy entire contents of
            // parent, or if subdivision reaches below approx mm size)
            // (having a model including the sun requires one subcell copying
            // entire contents of parent to be allowed)
            q1 += (if( subItems.length == items_c.length ) 1 else 0)
            val q2  = (subBound(3) - subBound(0)) < (Triangle.TOLERANCE * 4.0)

            // maybe recurse
            if( !subItems.isEmpty )
               new Some( new SpatialIndex( subBound, subItems,
               (if((q1 > 1) | q2) SpatialIndex.MAX_LEVELS else level_c + 1) ) )
            else
               None
         } )
      }
      // otherwise (size limit reached),
      // make leaf: store items, and end recursion
      else
         // (try to trim any reserve capacity)
         SpatialIndex.Items( items_c.slice( 0, items_c.length ) )
   }
}





object SpatialIndex
{
/// constants ------------------------------------------------------------------

   // accommodates scene including sun and earth, down to cm cells
   // (use 47 for mm)
   private final val MAX_LEVELS = 44

   // 8 seemed reasonably optimal in casual testing
   private final val MAX_ITEMS  =  8


/// types ----------------------------------------------------------------------

   private case class SubCells( subCells:Array[Option[SpatialIndex]] )
   private case class Items   (    items:Array[Triangle]     )
}
