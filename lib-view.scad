include <lib-math.scad>

// functions that deal with the viewport

// convert from viewport coordinates to world coordinates
function world(x) = 
    let(r = rot_k(rot_j(rot_i([x.x, x.y, 0], $vpr[0]), $vpr[1]), $vpr[2]))
    let(s = r * $vpd * tan($vpf / 2))
    let(t = s + $vpt)
    t;

// convert from world coordinates to viewport coordinates (inverse of world)
function vp(x) = 
    let(t = x - $vpt)
    let(s = t / ($vpd * tan($vpf / 2)))
    let(r = rot_i(rot_j(rot_k(s, -$vpr[2]), -$vpr[1]), -$vpr[0]))
    r;

// tell whether a vector is within the viewport (assuming it's square - I don't know how to get the viewport aspect ratio)
function visible(x) = let(v = vp(x))
    v[0] < 1 && v[0] > -1 && v[1] < 1 && v[1] > -1;





