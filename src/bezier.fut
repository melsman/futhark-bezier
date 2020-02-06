import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"
import "drawing"

type text_content = i32

module lys: lys with text_content = text_content = {
  type^ state = {time: f32, h: i32, w: i32,
                 moving: (i32, i32),
                 mouse: (i32, i32),
                 paused: bool,
		 cbeziers:[]cbezier
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
		     in {p0=(x1,y1),p1=(x2,y2),p2=(x3,y3),p3=(x4,y4),z=z+1,color=c}) rngs
             (indices rngs)
    in {time = 0, w, h,
	moving = (0,0),
	mouse = (0,0),
	paused = false,
	cbeziers = ts
	}

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let keydown (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.1 = 1
    else if key == SDLK_LEFT then s with moving.1 = -1
    else if key == SDLK_UP then s with moving.0 = -1
    else if key == SDLK_DOWN then s with moving.0 = 1
    else if key == SDLK_SPACE then s with paused = !s.paused
    else s

  let keyup (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.1 = 0
    else if key == SDLK_LEFT then s with moving.1 = 0
    else if key == SDLK_UP then s with moving.0 = 0
    else if key == SDLK_DOWN then s with moving.0 = 0
    else s

  let event (e: event) (s: state) =
    match e
    case #step td ->
      s with time = s.time + (if s.paused then 0 else td)
    case #keydown {key} ->
      keydown key s
    case #keyup {key} ->
      keyup key s
    case #mouse -> s
    case #wheel -> s

  let modify (t:f32) (p:point0) : point0 =
    (f32.cos (t*10.0) * 20, f32.sin t * 10) <+> (fpoint0_from_point0 p)
    |> point0_from_fpoint0

  let render (s: state) =
    let curves = map (\ {p0,p1,p2,p3,z,color} ->
			{p0=modify (s.time*2.0) p0,
			 p1=modify s.time p1,
			 p2=modify s.time p2,
			 p3=p3,
			 z=z,color=color}) s.cbeziers
    in drawpoints s.h s.w (points_of_cbeziers curves)

  type text_content = text_content

  let text_format () = "FPS: %d"

  let text_content (render_duration: f32) (_s: state): text_content =
    (t32 render_duration)

  let text_colour = const argb.blue
}
