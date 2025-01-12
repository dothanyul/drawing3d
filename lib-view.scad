include <lib-math.scad>

// functions that deal with the viewport

// convert from viewport coordinates to world coordinates
function world(x) = 
    let(r = rot_k(rot_j(rot_i([x.x, x.y, 0], $vpr[0]), $vpr[1]), $vpr[2]))
    let(s = r * $vpd * tan($vpf / 2))
    let(t = s + $vpt)
    t;

// convert from world coordinates to viewport coordinates (inverse of world)
// w is a corrective factor for the width vs the height
function vp(x, w=1.2) = 
    let(t = x - $vpt)
    let(s = t / ($vpd * tan($vpf / 2)))
    let(z = rot_k(s, -$vpr[2]), zy = rot_j(z, -$vpr[1]), r = rot_i(zy, -$vpr[0]))
    [r.x/w, r.y, r.z];

// tell whether a vector is within the viewport (assuming it's square - I don't know how to get the viewport aspect ratio)
function visible(x, w=1.2) = let(v = vp(x, w))
    land([each [for(b=[v.x, v.y]) for(s = [1, -1]) b * s < 1], v.z < 2]);

// put a background of color c covering width w at distance d back from 0
module background(c="#000", w=1, d=1) {
    face($vpr) translate([0, 0, -d-1] + $vpt) color(c) cube([w, w, 1], center=true);
}

