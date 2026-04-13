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
                (vs:[n]{z:i32,color:color}) : [h][w]color =
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
                   (ps:[n]point) :[h][w]color =
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

def bubble (a:point0) (b:point0) =
  if b.1 < a.1 then (b,a) else (a,b)

def normalize ((p,q,r,z,c): triangle) : triangle =
  let (p,q) = bubble p q
  let (q,r) = bubble q r
  let (p,q) = bubble p q
  in (p,q,r,z,c)

def lines_in_triangle ((p,_,r,_,_):triangle) : i64 =
  i64.i32(r.1 - p.1 + 1)

def dxdy (a:point0) (b:point0) : f32 =
  let dx = b.0 - a.0
  let dy = b.1 - a.1
  in if dy == 0 then f32.i32 0
     else f32.i32 dx / f32.i32 dy

def get_line_in_triangle ((p,q,r,z,c):triangle) (i:i64) =
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

def lines_of_triangles (xs:[]triangle) : []line =
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

def nullcurve() : cbezier = {p0=(-1,-1),p1=(-1,-1),p2=(-1,-1),p3=(-1,-1),
			     z=0,color=argb.white}

type fpoint0 = (f32,f32)

def fpoint0_from_point0 ((x,y):point0) : fpoint0 =
  (r32 x, r32 y)

def point0_from_fpoint0 ((x,y):fpoint0) : point0 =
  (i32.f32(f32.round x), i32.f32(f32.round y))

def (*>) (s:f32) ((x,y):fpoint0) : fpoint0 = (s*x,s*y)

def (<+>) ((x1,y1):fpoint0) ((x2,y2):fpoint0) : fpoint0 =
  (x1+x2,y1+y2)

def (<->) ((x1,y1):fpoint0) ((x2,y2):fpoint0) : fpoint0 =
  (x1-x2,y1-y2)

def zero_fpoint0 : fpoint0 = (0,0)

-- linear, quadratic, and cubic interpolations

-- linear bezier point from linear curve and t in [0;1]
def lb (p0:fpoint0,p1:fpoint0) (t:f32) : fpoint0 =
  (1-t) *> p0 <+> t *> p1

-- quadratic bezier point from quadratic curve and t in [0;1]
def qb (p0:fpoint0,p1:fpoint0,p2:fpoint0) (t:f32) : fpoint0 =
  (1-t) *> lb(p0,p1)t <+> t *> lb(p1,p2)t

-- cubic bezier point from cubic curve and t in [0;1]
def cb (p0:fpoint0,p1:fpoint0,p2:fpoint0,p3:fpoint0) (t:f32) =
  (1-t) *> qb(p0,p1,p2)t <+> t *> qb(p1,p2,p3)t

-- the cubic bezier curve differentiated
def cb' (p0:fpoint0,p1:fpoint0,p2:fpoint0,p3:fpoint0) (t:f32) =
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

def scale_intensity (s:f32) (i:f32) =
  i + (1-s)*(1-i)

-- [color_split s c] splits, according to s [0;1], the colour c into
-- two colours with maintained intensity.
def color_split (s:f32) (c: color) : (color,color) =
  let (r,g,b,a) = argb.to_rgba c
  let scale = scale_intensity
  in (argb.from_rgba (scale s r) (scale s g) (scale s b) a,
      argb.from_rgba (scale (1-s) r) (scale (1-s) g) (scale (1-s) b) a)

def bl (a:f32) b = (a+b)/2

def blend c1 c2 =
  let (r1,g1,b1,a1) = argb.to_rgba c1
  let (r2,g2,b2,a2) = argb.to_rgba c2
  in argb.from_rgba (bl r1 r2) (bl g1 g2) (bl b1 b2) (bl a1 a2)

def antialias_vertical ((x,y):fpoint0) (c:color)
  : (point0,color,point0,color) =
  let hi = f32.ceil y                                -- hi >= y
  let lo = f32.floor y
  let (chi,clo) = if hi == lo then (c,c)
                  else color_split (1-(hi-y)) c
  let x' = i32.f32(f32.round x)
  in ((x',i32.f32 hi), chi,
      (x',i32.f32 lo), clo)

def swap 'a (x:a,y:a) : (a,a) = (y,x)

def antialias_horizontal (p:fpoint0) (c:color)
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

def points_lbezier {p0:point0,p1:point0,z=_:i32,color=_:color} : i64 =
  i64.i32(1 + i32.(max (abs(p1.0-p0.0)) (abs(p1.1-p0.1))))

def fpoint_on_lbezier (curve:line) (i:i64) : fpoint0 =
  let p0 = fpoint0_from_point0 curve.p0
  let p1 = fpoint0_from_point0 curve.p1
  let t = f32.i64 i / f32.i64 (points_lbezier curve - 1)
  in lb (p0,p1) t

def get_point_on_lbezier (bc:line) (i:i64) : point =
  let p = fpoint_on_lbezier bc i
  in {p=point0_from_fpoint0 p, z=bc.z, color=bc.color}

def points_of_lbeziers (xs:[]line) : []point =
  expand points_lbezier get_point_on_lbezier xs

def steep (l:line) : bool =
  i32.(abs(l.p1.1-l.p0.1)>abs(l.p1.0-l.p0.0))

def get_antialiased_points_in_lbezier (bc:line) (i:i64) : [2]point =
  let p = fpoint_on_lbezier bc i
  let (q1,c1,q2,c2) =
    if steep bc then antialias_horizontal p bc.color
    else antialias_vertical p bc.color
  in [{p=q1, z=bc.z, color=c1},
      {p=q2, z=bc.z, color=c2}]

def points_of_lbeziers_antialiased (xs:[]line) : []point =
  expand points_lbezier get_antialiased_points_in_lbezier xs
     |> flatten


-- Functionality for splitting cubic bezier curves into segments that
-- are x-dominated or y-dominated

module X = {
type t = f32

def round a : i32 = i32.f32(f32.round a)

def lerp i j t = round (f32.(lerp (i32 i) (i32 j) t))

def plerp (p0:point0) (p1:point0) (t:t) : point0 =
  (lerp p0.0 p1.0 t, lerp p0.1 p1.1 t)

def split ({p0=a,p1=b,p2=c,p3=d,z,color}:cbezier) (t:t) : (cbezier, cbezier) =
  let e = plerp a b t
  let f = plerp b c t
  let g = plerp c d t
  let h = plerp e f t
  let j = plerp f g t
  let k = plerp h j t
  in ({p0=a,p1=e,p2=h,p3=k,z=z,color=blend color argb.red},
      {p0=k,p1=j,p2=g,p3=d,z=z,color=blend color argb.blue})

def sqrt a : t = f32.sqrt a
def sq a : t = a * a
def chk a : t = if a < 0 || a >= 1 then -1 else a
def zero4() : [4]t = [-1,-1,-1,-1]

def splits0 {A:i32,B:i32,C:i32,D:i32} : [2]t = -- returns sorted
  if B == 0 || D < 0 then [-1,-1]              -- array
  else if A == 0 then [-1,chk (f32.i32(-C) / f32.i32 B)]
  else if D > 0 then
    let s1 = chk (f32.((-(i32 B)-sqrt(i32 D)) / (2*i32 A)))
    let s2 = chk (f32.((-(i32 B)+sqrt(i32 D)) / (2*i32 A)))
    in if s1 < s2 then [s1,s2] else [s2,s1]
  else -- D == 0
    [-1, chk (f32.(-(i32 B) / (2*i32 A)))]

def maybe_swap [n] (ss:*[n]t) (i:i32) (j:i32) : *[n]t =
  let x = ss[i]
  let y = ss[j]
  in if x < y then ss
     else (ss with [i] = y) with [j] = x

def sort4 (ss : *[4]t) : *[4]t = -- assumes 01 and 23 sorted
  let ss = maybe_swap ss 0 2  -- smallest in slot 0
  let ss = maybe_swap ss 1 3  -- largest in slot 3
  let ss = maybe_swap ss 1 2  -- maybe rearrange 1 and 2
  in ss

def coeffs ({p0=(x0,y0),p1=(x1,y1),p2=(x2,y2),p3=(x3,y3),z=_,color=_}:cbezier) =
  ({a=x1-x0-(y1-y0), b=x2-x1-(y2-y1), c=x3-x2-(y3-y2)},
   {a=x1-x0+(y1-y0), b=x2-x1+(y2-y1), c=x3-x2+(y3-y2)})

def compD {a:i32,b,c} =
  let (A,B,C) = (3*a-6*b+3*c, 6*b-6*a, 3*a)
  in {A, B, C, D=B*B-4*A*C}

def simple {a:i32,b:i32,c:i32} : bool =
  a == 0 && b == 0 && c == 0

def splits (z:cbezier) : [4]t =
  let (coeffs1,coeffs2) = coeffs z
  let X1 = compD coeffs1
  let X2 = compD coeffs2
  in if simple coeffs1 || simple coeffs2 ||
        X1.A == 0 || X2.A == 0
     then zero4()
     else let ss = splits0 X1 ++ splits0 X2
          in sort4 (ss :> [4]t)

def sz (ss:[4]t) : i64 = -- [1;5]
  let maysub i c = if ss[i] == -1 then c - 1 else c
  in maysub 3 (maysub 2 (maysub 1 (maysub 0 5)))

def withColor (i:i64) (c:cbezier) =
  c with color =
  if i == 0 then argb.black
  else if i == 1 then argb.blue
  else if i == 2 then argb.green
  else if i == 3 then argb.brown
  else if i == 4 then argb.orange
  else argb.red

def get (z:cbezier,ss:[4]t,sz:i64) (i:i64) : cbezier =
  (if sz == 1 then z
  else if i == 0 then split z (ss[i+(5-sz)]) |> (.0) -- first segment
  else if i == sz-1 then split z (ss[3]) |> (.1) -- last segment
  else let s1 = ss[i+(5-sz)-1]
       let s2 = ss[i+(5-sz)]
       let z = split z s1 |> (.1)
       let d = 1-s1
       let s2' = (s2-s1)/d
       in if d == 0 || s2' < 0 || s2' > 1 then nullcurve()
          else split z s2' |> (.0)
  ) |> withColor i

def segs (zs:[]cbezier) : []cbezier =
  let xs = map (\z -> let ss = splits z
                      in (z, ss, sz ss)) zs
  in expand (.2) get xs
}


----------------------------------------------------------------------
--
-- CUBIC BEZIER CURVES
--
--    val points_of_cbeziers             : []cbezier -> []point
--    val points_of_cbeziers_antialiased : []cbezier -> []point
--
----------------------------------------------------------------------

def points_cbezier {p0:point0,p1=_:point0,p2=_:point0,p3:point0,z=_:i32,color=_:color} : i64 =
  2*(1 + i64.i32(i32.(max (abs(p3.0-p0.0)) (abs(p3.1-p0.1)))))
--  i64.i32(2 * i32.(abs(p1.0-p0.0) + abs(p1.1-p0.1) +
--                 abs(p2.0-p1.0) + abs(p2.1-p1.1) +   -- bad approaximation (also, the
--                 abs(p3.0-p2.0) + abs(p3.1-p2.1)))   -- parameterisation is non-linear!

def fpoint_on_cbezier (bc:cbezier) (i:i64) : (fpoint0,f32) =
  let p0 = fpoint0_from_point0 bc.p0
  let p1 = fpoint0_from_point0 bc.p1
  let p2 = fpoint0_from_point0 bc.p2
  let p3 = fpoint0_from_point0 bc.p3
  let t = f32.i64 i / f32.i64 (points_cbezier bc - 1)
  in (cb (p0,p1,p2,p3) t, t)

def get_point_on_cbezier (bc:cbezier) (i:i64) : point =
  let (p,_) = fpoint_on_cbezier bc i
  in {p=point0_from_fpoint0 p,
      z=bc.z,color=bc.color}

def points_of_cbeziers (xs:[]cbezier) : []point =
  expand points_cbezier get_point_on_cbezier (X.segs xs)

def steep_cubic (bc:cbezier) (t:f32) : bool =
  let p0 = fpoint0_from_point0 bc.p0
  let p1 = fpoint0_from_point0 bc.p1
  let p2 = fpoint0_from_point0 bc.p2
  let p3 = fpoint0_from_point0 bc.p3
  let (x,y) = cb' (p0,p1,p2,p3) t   -- the derivative wrt t
  in f32.(abs y > abs x)

def get_antialiased_points_in_cbezier (bc:cbezier) (i:i64) : [2]point =
  let (p,t) = fpoint_on_cbezier bc i
  let (q1,c1,q2,c2) =
    if steep_cubic bc t then antialias_horizontal p bc.color
    else antialias_vertical p bc.color
  in [{p=q1, z=bc.z, color=c1},
      {p=q2, z=bc.z, color=c2}]

def points_of_cbeziers_antialiased (xs:[]cbezier) : []point =
  expand points_cbezier get_antialiased_points_in_cbezier (X.segs xs)
     |> flatten
