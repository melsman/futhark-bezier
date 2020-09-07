import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"
import "drawing"
import "font"

module F = OpenBaskerville

type text_content = i32

module lys: lys with text_content = text_content = {
  type~ state = {time: f32, h: i32, w: i32,
                 moving: (i32, i32),
                 mouse: (i32, i32),
                 paused: bool,
		 cbeziers:[]cbezier,
		 resolution:i32
		 }

  let grab_mouse = false

  -- Pick an RNG engine and define random distributions for specific types.
  module rng_engine = minstd_rand
  --module rand_f32 = uniform_real_distribution f32 rng_engine
  module rand_i32 = uniform_int_distribution i32 rng_engine

  let indices [n] 'a (_:[n]a) : [n]i32 = iota n

  let init (seed: u32) (h: i32) (w: i32): state =
    let rng = rng_engine.rng_from_seed [i32.u32 seed]
    let rngs = rng_engine.split_rng 100 rng
    let ts = map2 (\rng z ->
		     let maxsz = 200
		     let (rng,x1) = rand_i32.rand (0,w-maxsz) rng
		     let (rng,y1) = rand_i32.rand (0,h-maxsz) rng
		     let (rng,dx2) = rand_i32.rand (5,maxsz) rng
		     let (rng,dy2) = rand_i32.rand (5,maxsz) rng
		     let (rng,dx3) = rand_i32.rand (5,maxsz) rng
		     let (rng,dy3) = rand_i32.rand (5,maxsz) rng
		     let (rng,dx4) = rand_i32.rand (5,maxsz) rng
		     let (rng,dy4) = rand_i32.rand (5,maxsz) rng
		     let (x2,y2) = (x1+dx2,y1+dy2)
		     let (x3,y3) = (x1+dx3,y1+dy3)
		     let (x4,y4) = (x1+dx4,y1+dy4)
		     let (_rng,c) = rand_i32.rand (0,256*256*256) rng
		     in {p0=(x1,y1),p1=(x2,y2),p2=(x3,y3),p3=(x4,y4),z=z+1,color=u32.i32 c}) rngs
             (indices rngs)
    in {time = 0, w, h,
	moving = (0,0),
	mouse = (0,0),
	paused = false,
	cbeziers = ts,
	resolution = 1
	}

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let keydown (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.1 = 1
    else if key == SDLK_LEFT then s with moving.1 = -1
    else if key == SDLK_UP then s with moving.0 = -1
    else if key == SDLK_DOWN then s with moving.0 = 1
    else if key == SDLK_SPACE then s with paused = !s.paused
    else if key == SDLK_r then s with resolution = s.resolution + 1
    else s

  let keyup (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.1 = 0
    else if key == SDLK_LEFT then s with moving.1 = 0
    else if key == SDLK_UP then s with moving.0 = 0
    else if key == SDLK_DOWN then s with moving.0 = 0
    else if key == SDLK_f then s with resolution = i32.max 1 (s.resolution-1)
    else s

  let event (e: event) (s: state) =
    match e
    case #step td -> s with time = s.time + (if s.paused then 0 else td)
    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
    case #mouse -> s
    case #wheel -> s

  let modify (s:f32) (t:f32) (p:point0) : point0 =
    (s*(f32.cos (t*10.0) * 20), s*(f32.sin t * 10)) <+> (fpoint0_from_point0 p)
    |> point0_from_fpoint0


  let transl ((x,y):point0) (l:line) : line =
    l with p0=(l.p0.0+x,l.p0.1+y)
      with p1=(l.p1.0+x,l.p1.1+y)

  let transl' ((x,y):point0) (p:point) =
    p with p = (p.p.0+x,p.p.1+y)

  let lines : []line = [{color=argb.blue,z=1,p0=(10,15),p1=(300,105)},
			{color=argb.red,z=1,p0=(100,15),p1=(300,105)},
			{color=argb.green,z=1,p0=(10,45),p1=(50,345)},
			{color=argb.yellow,z=1,p0=(100,45),p1=(30,345)}]

  let lines2 : []line = map (transl (0,40)) lines

  let curv1 : []cbezier = [{color=argb.blue,z=1,p0=(50,300),p1=(100,310),p2=(200,310),p3=(300,300)}]
  let curv2 : []cbezier = [{color=argb.blue,z=1,p0=(50,400),p1=(100,410),p2=(200,410),p3=(300,400)}]

  let curv3 : []cbezier = [{color=argb.red,z=1,p0=(60,60),p1=(20,160),p2=(200,300),p3=(60,400)}]

  let halfer (r:i32) (grid: [][]color) : [][]color =
    loop g = grid for _i < r-1 do half g

  let doubler (r:i32) (grid: [][]color) : [][]color =
    loop g = grid for _i < r-1 do scalei2d 2 g

  let scp (s:i32) ((x,y):point0) : point0 = (x/s, y/s)

  type^ obj = {lines:[]line, curves:[]cbezier}

  let scale_obj (s: i32) (g:obj) : obj =
    {lines=map (\(l:line) -> l with p0=scp s l.p0 with p1=scp s l.p1) g.lines,
     curves=map (\(u:cbezier) -> u with p0=scp s u.p0 with p1=scp s u.p1
  	         with p2=scp s u.p2 with p3=scp s u.p3) g.curves}

  let transl_point ((x,y):point0) ((a,b):point0) : point0 = (a+x,b+y)

  let transl_line (p:point0) (l:line) : line =
    l with p0=transl_point p l.p0 with p1=transl_point p l.p1

  let transl_curve (p:point0) (u:cbezier) : cbezier =
    u with p0=transl_point p u.p0 with p1=transl_point p u.p1
      with p2=transl_point p u.p2 with p3=transl_point p u.p3

  let transl_obj (p:point0) (g:obj) : obj =
    {lines=map (transl_line p) g.lines,
     curves=map (transl_curve p) g.curves}

  let ymirror_point (y:i32) ((a,b):point0) : point0 = (a,y-b)

  let ymirror_line (y:i32) (l:line) : line =
    l with p0=ymirror_point y l.p0 with p1=ymirror_point y l.p1

  let ymirror_curve (y:i32) (u:cbezier) : cbezier =
    u with p0=ymirror_point y u.p0 with p1=ymirror_point y u.p1
      with p2=ymirror_point y u.p2 with p3=ymirror_point y u.p3

  let ymirror_obj (y:i32) (g:obj) : obj =
    {lines=map (ymirror_line y) g.lines,
     curves=map (ymirror_curve y) g.curves}

  let text0 = "Futhark Gotta Go Fast"
  let textN = length text0
  let text = (text0 :> [textN]u8)
  let glyfinfos = map (\c -> match F.glyph c
			     case #Some x -> x
			     case #None -> {char=0u8,nlines=0,ncurves=0,advance=0,firstlineidx=0,firstcurveidx=0})
                  text
  let text_advances = ([0] ++ scan (+) 0 (map (\gi -> gi.advance) glyfinfos))[:textN]

  let text_lines = expand (\(gi,_) -> gi.nlines)
                          (\(gi,adv) i -> #[unsafe] F.lines[gi.firstlineidx+i] |> transl_line (adv,0))
                          (zip glyfinfos text_advances)

  let text_curves = expand (\(gi,_) -> gi.ncurves)
                           (\(gi,adv) i -> #[unsafe] F.curves[gi.firstcurveidx+i] |> transl_curve (adv,0))
                           (zip glyfinfos text_advances)

  let text_obj : obj = {lines=text_lines,curves=text_curves}

  let mod_obj (s:state) (g:obj) : obj =
    {lines=map (\(l:line) -> l with p0=modify 0.5 (s.time*1.5) l.p0
	                       with p1=modify 0.5 (s.time*1.5) l.p1) g.lines,
     curves=map (\(c:cbezier) ->
		   c with p0=modify 0.5 (s.time*1.5) c.p0
		     with p1=modify 0.3 s.time c.p1
		     with p2=modify 0.3 s.time c.p2
		     with p3=modify 0.5 (s.time*1.5) c.p3) g.curves}

  let render (s: state) =
    let curves = map (\ {p0,p1,p2,p3,z,color} ->
			{p0=modify 1 (s.time*2.0) p0,
			 p1=modify 1 s.time p1,
			 p2=modify 1 s.time p2,
			 p3=p3,
			 z=z,color=color}) s.cbeziers
    let points_of_obj (g:obj) =
      let obj2 = ymirror_obj 1000 g |> transl_obj (200,200) |> scale_obj 16 |> mod_obj s
      in points_of_lbeziers_antialiased obj2.lines ++ points_of_cbeziers_antialiased obj2.curves

    let points =
      points_of_cbeziers_antialiased curves ++
      points_of_cbeziers_antialiased curv1 ++
      points_of_cbeziers curv2 ++
      points_of_lbeziers_antialiased lines ++
      points_of_lbeziers lines2 ++
      points_of_obj text_obj ++
      points_of_cbeziers curv3

    in drawpoints s.h s.w points
       |> halfer (s.resolution)
       |> doubler (s.resolution)

  type text_content = text_content

  let text_format () = "FPS: %d"

  let text_content (render_duration: f32) (_s: state): text_content =
    (t32 render_duration)

  let text_colour = const argb.blue
}
