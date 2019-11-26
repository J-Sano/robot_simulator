let deg2rad x = x *. (atan 1.)*.8. /. 360.;;
let rad2deg x = x /. ((atan 1.)*.8.) *. 360.;;

let rotation x y angle =
  let rad = deg2rad angle in
  let sin_rad = sin rad in
  let cos_rad = cos rad in
  let x' = x *. cos_rad -. y *. sin_rad in
  let y' = x *. sin_rad +. y *. cos_rad in
  x',y'
;;

let rotation2int x y angle =
  let x',y' = rotation x y angle in
  int_of_float x', int_of_float y'
;;
