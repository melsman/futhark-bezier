import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/athas/matte/colour"

-- Flattened drawing of Bezier curves (linear; e.g. lines, quadratic,
-- and cubic) and triangles

type color = argb.colour
type point0 = (i32,i32)
type point = {p:point0,z:i32,color:color}
type line = {p0:point0,p1:point0,z:i32,color:color}

-- Write to grid
let mk_grid [n] (h:i32) (w:i32) (xs:[n]i32) (ys:[n]i32)
                (vs:[n]{z:i32,color:color}) : [h][w]i32 =
  let is = map2 (\x y -> w*y+x) xs ys
  let grid = replicate (h*w) (0,argb.white)
  let f (z1,c1) (z2,c2) = if z1 > z2 then (z1,c1) else (z2,c2)
  let cs = map (\v -> (v.z,v.color)) vs
  in reduce_by_index grid f (0,argb.white) is cs
     |> map (.1)
     |> unflatten h w

-- Parallel flattened algorithm for turning triangles into
-- lines, using expansion.

type triangle = (point0,point0,point0,i32,color)

let bubble (a:point0) (b:point0) =
  if b.1 < a.1 then (b,a) else (a,b)

let normalize ((p,q,r,z,c): triangle) : triangle =
  let (p,q) = bubble p q
  let (q,r) = bubble q r
  let (p,q) = bubble p q
  in (p,q,r,z,c)

let lines_in_triangle ((p,_,r,_,_):triangle) : i32 =
  r.1 - p.1 + 1

let dxdy (a:point0) (b:point0) : f32 =
  let dx = b.0 - a.0
  let dy = b.1 - a.1
  in if dy == 0 then f32.i32 0
     else f32.i32 dx / f32.i32 dy

let get_line_in_triangle ((p,q,r,z,c):triangle) (i:i32) =
  let y = p.1 + i
  in if i <= q.1 - p.1 then     -- upper half
       let sl1 = dxdy p q
       let sl2 = dxdy p r
       let x1 = p.0 + i32.f32(f32.round(sl1 * f32.i32 i))
       let x2 = p.0 + i32.f32(f32.round(sl2 * f32.i32 i))
       in {p0=(x1,y),p1=(x2,y),z,color=c}
     else                       -- lower half
       let sl1 = dxdy r p
       let sl2 = dxdy r q
       let dy = (r.1 - p.1) - i
       let x1 = r.0 - i32.f32(f32.round(sl1 * f32.i32 dy))
       let x2 = r.0 - i32.f32(f32.round(sl2 * f32.i32 dy))
       in {p0=(x1,y),p1=(x2,y),z,color=c}

let lines_of_triangles (xs:[]triangle) : []line =
  expand lines_in_triangle get_line_in_triangle
         (map normalize xs)

type cbezier = {p0:point0,p1:point0,p2:point0,p3:point0,z:i32,color:color}

type fpoint0 = (f32,f32)

let (*>) (s:f32) ((x,y):fpoint0) : fpoint0 = (s*x,s*y)

let (<+>) ((x1,y1):fpoint0) ((x2,y2):fpoint0) : fpoint0 = (x1+x2,y1+y2)

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

let points_cbezier {p0:point0,p1:point0,p2:point0,p3:point0,z=_:i32,color=_:color} : i32 =
  5 * ((p1.0-p0.0) + (p1.1-p0.1) + (p2.0-p1.0) + (p2.1-p1.1) +
       (p3.0-p2.0) + (p3.1-p2.1))

let fpoint0_from_point0 ((x,y):point0) : fpoint0 =
  (r32 x, r32 y)

let point0_from_fpoint0 ((x,y):fpoint0) : point0 =
  (i32.f32 x, i32.f32 y)

let get_point_on_cbezier (curve:cbezier) (i:i32) : point =
  let p0 = fpoint0_from_point0 curve.p0
  let p1 = fpoint0_from_point0 curve.p1
  let p2 = fpoint0_from_point0 curve.p2
  let p3 = fpoint0_from_point0 curve.p3
  let t = r32 i / r32 (points_cbezier curve)
  let p = cb (p0,p1,p2,p3) t
  in {p=point0_from_fpoint0 p,
      z=curve.z,color=curve.color}

let points_of_cbeziers (xs:[]cbezier) : []point =
  expand points_cbezier get_point_on_cbezier xs

let points_lbezier {p0:point0,p1:point0,z=_:i32,color=_:color} : i32 =
  1 + i32.(max (abs(p1.0-p0.0)) (abs(p1.1-p0.1)))

let get_point_on_lbezier (curve:line) (i:i32) : point =
  let p0 = fpoint0_from_point0 curve.p0
  let p1 = fpoint0_from_point0 curve.p1
  let t = r32 i / r32 (points_lbezier curve)
  let p = lb (p0,p1) t
  in {p=point0_from_fpoint0 p,
      z=curve.z,color=curve.color}

let points_of_lbeziers (xs:[]line) : []point =
  expand points_lbezier get_point_on_lbezier xs

let drawpoints [n] (h:i32) (w:i32)
                   (ps:[n]point) :[h][w]i32 =
  let xs = map (.p.0) ps
  let ys = map (.p.1) ps
  let vs = map (\p -> {z=p.z,color=p.color}) ps
  in mk_grid h w xs ys vs
