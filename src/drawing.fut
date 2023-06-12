----------------------------------------------------------------------
--
-- Flattened drawing of Bezier curves (linear; e.g. lines, quadratic,
-- and cubic) and triangles
--
----------------------------------------------------------------------

import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/athas/matte/colour"

type color = argb.colour
type point0 = (i32,i32)
type point = {p:point0,z:i32,color:color}
type line = {p0:point0,p1:point0,z:i32,color:color}

--
-- Functions for drawing points on a grid
--
let mk_grid [n] (h:i64) (w:i64) (xs:[n]i32) (ys:[n]i32)
                (vs:[n]{z:i32,color:color}) : [h][w]argb.colour =
  let is = map2 (\x y -> w * (i64.i32 y) + (i64.i32 x)) xs ys
  let grid = replicate (h*w) (0,argb.white)
  let f (z1,c1) (z2,c2) = if z1 > z2 || (z1 == z2 && c2 > c1)
			  then (z1,c1) -- choose darker color
			  else (z2,c2)
  let cs = map (\v -> (v.z,v.color)) vs
  in reduce_by_index grid f (0,argb.white) is cs
     |> map (.1)
     |> unflatten

let drawpoints [n] (h:i64) (w:i64)
                   (ps:[n]point) :[h][w]argb.colour =
  let xs = map (.p.0) ps
  let ys = map (.p.1) ps
  let vs = map (\p -> {z=p.z,color=p.color}) ps
  in mk_grid h w xs ys vs

let scalei2d [h][w] (s:i64) (grid: [h][w]color) : [][]color =
  tabulate_2d (s*h)(s*w) (\r c -> #[unsafe] (grid[r/s])[c/s])

let half [h][w] (grid: [h][w]color) : [][]color =
  tabulate_2d (h/2)(w/2) (\r c -> #[unsafe] (grid[r])[c])

----------------------------------------------------------------------
--
-- Parallel flattened algorithm for turning triangles into
-- lines, using expansion.
--
----------------------------------------------------------------------

type triangle = (point0,point0,point0,i32,color)

let bubble (a:point0) (b:point0) =
  if b.1 < a.1 then (b,a) else (a,b)

let normalize ((p,q,r,z,c): triangle) : triangle =
  let (p,q) = bubble p q
  let (q,r) = bubble q r
  let (p,q) = bubble p q
  in (p,q,r,z,c)

let lines_in_triangle ((p,_,r,_,_):triangle) : i64 =
  i64.i32(r.1 - p.1 + 1)

let dxdy (a:point0) (b:point0) : f32 =
  let dx = b.0 - a.0
  let dy = b.1 - a.1
  in if dy == 0 then f32.i32 0
     else f32.i32 dx / f32.i32 dy

let get_line_in_triangle ((p,q,r,z,c):triangle) (i:i64) =
  let y = p.1 + i32.i64 i
  in if i32.i64 i <= q.1 - p.1 then     -- upper half
       let sl1 = dxdy p q
       let sl2 = dxdy p r
       let x1 = p.0 + i32.f32(f32.round(sl1 * f32.i64 i))
       let x2 = p.0 + i32.f32(f32.round(sl2 * f32.i64 i))
       in {p0=(x1,y),p1=(x2,y),z,color=c}
     else                       -- lower half
       let sl1 = dxdy r p
       let sl2 = dxdy r q
       let dy = (r.1 - p.1) - i32.i64 i
       let x1 = r.0 - i32.f32(f32.round(sl1 * f32.i32 dy))
       let x2 = r.0 - i32.f32(f32.round(sl2 * f32.i32 dy))
       in {p0=(x1,y),p1=(x2,y),z,color=c}

let lines_of_triangles (xs:[]triangle) : []line =
  expand lines_in_triangle get_line_in_triangle
         (map normalize xs)

----------------------------------------------------------------------
--
-- DEFINITIONS OF BEZIER CURVES
--
--  Linear (lines), quadratic, and cubic
--
----------------------------------------------------------------------

type cbezier = {p0:point0,p1:point0,p2:point0,p3:point0,
		z:i32,color:color}
type fpoint0 = (f32,f32)

let fpoint0_from_point0 ((x,y):point0) : fpoint0 =
  (r32 x, r32 y)

let point0_from_fpoint0 ((x,y):fpoint0) : point0 =
  (i32.f32(f32.round x), i32.f32(f32.round y))

let (*>) (s:f32) ((x,y):fpoint0) : fpoint0 = (s*x,s*y)

let (<+>) ((x1,y1):fpoint0) ((x2,y2):fpoint0) : fpoint0 =
  (x1+x2,y1+y2)

let (<->) ((x1,y1):fpoint0) ((x2,y2):fpoint0) : fpoint0 =
  (x1-x2,y1-y2)

let zero_fpoint0 : fpoint0 = (0,0)

-- linear, quadratic, and cubic interpolations

-- linear bezier point from linear curve and t in [0;1]
let lb (p0:fpoint0,p1:fpoint0) (t:f32) : fpoint0 =
  (1-t) *> p0 <+> t *> p1

-- quadratic bezier point from quadratic curve and t in [0;1]
let qb (p0:fpoint0,p1:fpoint0,p2:fpoint0) (t:f32) : fpoint0 =
  (1-t) *> lb(p0,p1)t <+> t *> lb(p1,p2)t

-- cubic bezier point from cubic curve and t in [0;1]
let cb (p0:fpoint0,p1:fpoint0,p2:fpoint0,p3:fpoint0) (t:f32) =
  (1-t) *> qb(p0,p1,p2)t <+> t *> qb(p1,p2,p3)t

-- the cubic bezier curve differentiated
let cb' (p0:fpoint0,p1:fpoint0,p2:fpoint0,p3:fpoint0) (t:f32) =
  3*(1-t)*(1-t) *> (p1 <-> p0) <+>
  6*(1-t)*t *> (p2 <-> p1) <+>
  3*t*t *> (p3 <-> p2)

----------------------------------------------------------------------
--
-- ANTIALIASING TOOLS
--
----------------------------------------------------------------------

-- [scale_intensity s i] scales the intensity i [0;1] according to s
-- [0;1]. The result is an intensity [0;1].
--
-- Examples:
--  scale_intensity  1 .8 = .8     scale_intensity  1 .3 =  .3
--  scale_intensity  0 .8 =  1     scale_intensity  0 .3 =   1
--  scale_intensity .5 .8 = .9     scale_intensity .5 .3 = .65
--  scale_intensity  1  0 =  0

let scale_intensity (s:f32) (i:f32) =
  i + (1-s)*(1-i)

-- [colour_split s c] splits, according to s [0;1], the colour c into
-- two colours with maintained intensity.
let colour_split (s:f32) (c: color) : (color,color) =
  let (r,g,b,a) = argb.to_rgba c
  let scale = scale_intensity
  in (argb.from_rgba (scale s r) (scale s g) (scale s b) a,
      argb.from_rgba (scale (1-s) r) (scale (1-s) g) (scale (1-s) b) a)

let antialias_vertical ((x,y):fpoint0) (c:color)
  : (point0,color,point0,color) =
  let hi = f32.ceil y                                -- hi >= y
  let lo = f32.floor y
  let (chi,clo) = if hi == lo then (c,c)
		  else colour_split (1-(hi-y)) c
  let x' = i32.f32(f32.round x)
  in ((x',i32.f32 hi), chi,
      (x',i32.f32 lo), clo)

let swap 'a (x:a,y:a) : (a,a) = (y,x)

let antialias_horizontal (p:fpoint0) (c:color)
  : (point0,color,point0,color) =
  let (q1,c1,q2,c2) = antialias_vertical (swap p) c
  in (swap q1,c1,swap q2,c2)

----------------------------------------------------------------------
--
-- LINEAR BEZIER CURVES - a.k.a. lines
--
--   val points_of_lbeziers             : []line -> []point
--   val points_of_lbeziers_antialiased : []line -> []point
--
----------------------------------------------------------------------

let points_lbezier {p0:point0,p1:point0,z=_:i32,color=_:color} : i64 =
  i64.i32(1 + i32.(max (abs(p1.0-p0.0)) (abs(p1.1-p0.1))))

let fpoint_on_lbezier (curve:line) (i:i64) : fpoint0 =
  let p0 = fpoint0_from_point0 curve.p0
  let p1 = fpoint0_from_point0 curve.p1
  let t = f32.i64 i / f32.i64 (points_lbezier curve - 1)
  in lb (p0,p1) t

let get_point_on_lbezier (bc:line) (i:i64) : point =
  let p = fpoint_on_lbezier bc i
  in {p=point0_from_fpoint0 p, z=bc.z, color=bc.color}

let points_of_lbeziers (xs:[]line) : []point =
  expand points_lbezier get_point_on_lbezier xs

let steep (l:line) : bool =
  i32.(abs(l.p1.1-l.p0.1)>abs(l.p1.0-l.p0.0))

let get_antialiased_points_in_lbezier (bc:line) (i:i64) : [2]point =
  let p = fpoint_on_lbezier bc i
  let (q1,c1,q2,c2) =
    if steep bc then antialias_horizontal p bc.color
    else antialias_vertical p bc.color
  in [{p=q1, z=bc.z, color=c1},
      {p=q2, z=bc.z, color=c2}]

let points_of_lbeziers_antialiased (xs:[]line) : []point =
  expand points_lbezier get_antialiased_points_in_lbezier xs
     |> flatten

----------------------------------------------------------------------
--
-- CUBIC BEZIER CURVES
--
--    val points_of_cbeziers             : []cbezier -> []point
--    val points_of_cbeziers_antialiased : []cbezier -> []point
--
----------------------------------------------------------------------

let points_cbezier {p0:point0,p1:point0,p2:point0,p3:point0,z=_:i32,color=_:color} : i64 =
--  1 + i32.(max (abs(p3.0-p0.0)) (abs(p3.1-p0.1)))
  i64.i32(2 * i32.(abs(p1.0-p0.0) + abs(p1.1-p0.1) +
		   abs(p2.0-p1.0) + abs(p2.1-p1.1) +   -- bad approaximation (also, the
		   abs(p3.0-p2.0) + abs(p3.1-p2.1)))   -- parameterisation is non-linear!

let fpoint_on_cbezier (bc:cbezier) (i:i64) : (fpoint0,f32) =
  let p0 = fpoint0_from_point0 bc.p0
  let p1 = fpoint0_from_point0 bc.p1
  let p2 = fpoint0_from_point0 bc.p2
  let p3 = fpoint0_from_point0 bc.p3
  let t = f32.i64 i / f32.i64 (points_cbezier bc - 1)
  in (cb (p0,p1,p2,p3) t, t)

let get_point_on_cbezier (bc:cbezier) (i:i64) : point =
  let (p,_) = fpoint_on_cbezier bc i
  in {p=point0_from_fpoint0 p,
      z=bc.z,color=bc.color}

let points_of_cbeziers (xs:[]cbezier) : []point =
  expand points_cbezier get_point_on_cbezier xs

let steep_cubic (bc:cbezier) (t:f32) : bool =
  let p0 = fpoint0_from_point0 bc.p0
  let p1 = fpoint0_from_point0 bc.p1
  let p2 = fpoint0_from_point0 bc.p2
  let p3 = fpoint0_from_point0 bc.p3
  let (x,y) = cb' (p0,p1,p2,p3) t   -- the derivative wrt t
  in f32.(abs y > abs x)

let get_antialiased_points_in_cbezier (bc:cbezier) (i:i64) : [2]point =
  let (p,t) = fpoint_on_cbezier bc i
  let (q1,c1,q2,c2) =
    if steep_cubic bc t then antialias_horizontal p bc.color
    else antialias_vertical p bc.color
  in [{p=q1, z=bc.z, color=c1},
      {p=q2, z=bc.z, color=c2}]

let points_of_cbeziers_antialiased (xs:[]cbezier) : []point =
  expand points_cbezier get_antialiased_points_in_cbezier xs
     |> flatten
