(*(*for top-level, comment out here*)
#load "graphics.cma";;
#load "unix.cma";;
#directory "+threads";;
#load "threads.cma";;
#use "basic_op.ml;;
*)
(*compile with #ocamlopt -I +threads unix.cmxa threads.cmxa graphics.cmxa sim2.ml *)
open Graphics;;
open_graph " 600x400";;

let ratio = 10;;


set_color @@ rgb 0 100 100;;

type target = Found | NotFound;;
type state = Running | Finished;;
let draw_robot x y angle status =
  if status = Found then set_color blue else set_color red;
  let x1,y1 = rotation2int (-10.) (-7.) angle in
  let x2,y2 = rotation2int (-10.) (+7.) angle in
  let x3,y3 = rotation2int (+10.) 0. angle in
  let x = int_of_float x in
  let y = int_of_float y in
  draw_poly [|x+x1,y+y1;x+x2,y+y2;x+x3,y+y3|];;

let draw_rect_ x y angle =
  set_color red;
  let x1,y1 = rotation2int (-20.) (-10.) angle in
  let x2,y2 = rotation2int (-20.) (+10.) angle in
  let x3,y3 = rotation2int (+20.) (+10.) angle in
  let x4,y4 = rotation2int (+20.) (-10.) angle in
  fill_poly [|x+x1,y+y1;x+x2,y+y2;x+x3,y+y3;x+x4,y+y4|];;

let rect_x = 400.;;
let rect_y = 380.;;
let rect_angle = 10.;;
draw_rect_ (int_of_float rect_x) (int_of_float rect_y) rect_angle;;

let camera_v = 90.;;
let camera_h = 90.;;

let motor x y robot_angle camera_h =
  let vision_x', vision_y' = rotation (rect_x -. x) (rect_y -. y) (-. robot_angle -. camera_h +. 90.) in
  let vision_x, vision_y = vision_y' +. 140. , 240. -. vision_x' in
  let angle = -.(mod_float (rect_angle -. robot_angle -. camera_h) 90. ) in
  let target_is = if 0. <= vision_x && vision_x <= 320. && 0. <= vision_y && vision_y <= 240. then Found else NotFound in
  (if vision_y > 220. then Finished else Running),target_is,
  130. -. (vision_x -. 140. )*. 0.03 , 130. +. (vision_x -. 140.)*. 0.03, camera_h
;;


let move x y angle motor = 
  let rec move' n x y angle camera_h motor = if n > 0 then
                                               begin
                                                 print_int n;
                                                 print_string ": robot is now at (";
                                                 print_float x;
                                                 print_string ", ";
                                                 print_float y;
                                                 print_string " ) , angle is ";
                                                 print_float angle;
                                                 print_endline "";
                                                 Thread.delay 0.1;
                                                 let alpha = 0.1 in
                                                 let status,target,motor_x, motor_y, new_camera_h = motor x y angle camera_h in
                                                 let new_x, new_y = rotation ((motor_x +. motor_y)*. alpha) 0. angle in
                                                 let new_x,new_y = new_x +. x, new_y +. y in
                                                 draw_robot x y angle target;
                                                 let angle_factor = alpha *. 10. in
                                                 let new_angle = angle +. angle_factor *. (motor_y -. motor_x) in
                                                 if status = Running
                                                 then move' (n-1) new_x new_y new_angle new_camera_h motor
                                                 else
                                                   begin
                                                     Thread.delay 5.;
                                                     ()
                                                   end
                                               end
                                             else
                                               begin
                                                 Thread.delay 10.;
                                                 ()
                                               end
  in
  move' 50 x y angle 90. motor
;;

let () = move 40. 50. 20. motor;;


