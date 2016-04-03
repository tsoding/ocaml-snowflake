open Graphics

let window_width = 800
let window_height = 600

let pi = 3.1415

let round x = 
  let y = x -. (floor x) in
  if y < 0.5 then int_of_float (floor x) else int_of_float (ceil x)

let int_range a b =
  let rec int_range_rec l a b =
    if a > b then l
    else int_range_rec (b :: l) a (b - 1)
  in (int_range_rec [] a b)

let branch_endpoint center length alpha i = 
  let (center_x, center_y) = center in
  (center_x +. length *. cos (float_of_int i *. alpha), 
   center_y +. length *. sin (float_of_int i *. alpha))

let draw_line start finish =
  let (start_x, start_y) = start in
  let (finish_x, finish_y) = finish in
  moveto (round start_x) (round start_y);
  lineto (round finish_x) (round finish_y)

let rec draw_snowflake degree length level center =
  if level > 0 then
    let alpha = 2.0 *. pi /. (float_of_int degree) in
    let endpoints = List.map (branch_endpoint center length alpha) 
                             (int_range 0 (degree - 1)) in
    List.map (draw_line center) endpoints;
    List.map (draw_snowflake degree (length /. 3.0) (level - 1)) endpoints;
    ()
  else
    ()

let _ =
  let degree = 6 in
  let level = 4 in
  let center = ((float_of_int window_width) /. 2.0, 
                (float_of_int window_height) /. 2.0) in
  let length = (float_of_int (min window_width window_height)) /. 3.0 in
  open_graph "";
  resize_window window_width window_height;
  draw_snowflake degree length level center;
  read_line ()
