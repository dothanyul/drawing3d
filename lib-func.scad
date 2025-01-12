// parametric functions and functions to deal with them
include <lib-math.scad>

// zero function in Rn
function f0(n) = 
    n < 1 ? undef :
    n == 1 ? function(t) 0 :
    function(t) vec_0(n);

/*
Parametric function [0,1] -> Rd for a line segment from a to b
*/
function to(a, b) = function(t)
    a + (b - a) * t;

// 90 degree elliptical arc from x1 to x2 in R2 for t ϵ [0,1]
// left handed or right handed curvature
function arc(x1, x2, left = true) = 
    x1 == x2 ? function(t) x1 :
    x1.x == x2.x || x1.y == x2.y ? to(x1, x2) :
    function(t) left ? 
    [x1.x + (x2 - x1).x * (1 - cos(t * 90)), x1.y + (x2 - x1).y * sin(t * 90)] :
    [x1.x + (x2 - x1).x * sin(t * 90), x1.y + (x2 - x1).y * (1 - cos(t * 90))];

/*
Parametric function x ϵ [0,1]^n -> Rd for rectangular basis b with o the image of the origin
*/
function rect(o, b) = function(x)
    o + sum([for(i=[0:1:len(b)-1]) b[i] * x[i]]);

/*
Parametric function x ϵ [0,1]^n -> Rd for cylindrical space with o the image of the origin, r the radius of the image of i, and k the image of k
*/
function cyl(o, r, k) = function(x)
    o + [each (r * x.x * [cos(x.y * 360), sin(x.y * 360)]), 0] + x.z * k;

// round capped stroke from x1 to x2 in <i3,j3> outline : [0,1] -> R3
// thick = thickness of the stroke, c = proportion of the domain containing the caps
function stroke(x1, x2, thick, p=0.5) =
    let(l = norm(x2 - x1), // length
        d = unit(x2 - x1), // direction
        r = thick / 2) // radius
    let(c = p / 2, // single cap length in t
        b = (1 - p) / 2) // single side length in t
    function(t)
        t < c / 2 ? let(h = t / c * 180) x1 + rot_k([cos(h), sin(h)], atan2(d.y, d.x) + 180) * r :
        t < 0.5 - c / 2 ? to(x1, x2)((t - c / 2) / b) + rot_k(d, -90) * r :
        t < 0.5 + c / 2 ? let(h = (t - (0.5 - c / 2)) / c * 180) x2 + rot_k([cos(h), sin(h)], atan2(d.y, d.x) - 90) * r :
        t < 1 - c / 2 ? to(x2, x1)((t - 0.5 - c / 2) / b) + rot_k(d, 90) * r :
        let(h = (t - 1 + c / 2) / (c / 2) * 90) x1 + rot_k([cos(h), sin(h)], atan2(d.y, d.x) + 90) * r;

// break [0,1] into n pieces with an integer index, index+1, and fractional part
// loop=true means add an extra segment to loop around to the beginning
// e.g. segment(0.23, 10, false) = [2, 3, 0.3], segment(0.95, 5, true) = [4, 0, 0.75]
function segment(s, n, loop) = 
    let(t = loop ? mod(s * n, 1) :
            s < 0 ? s * (n-1) : 
            s > 1 - 1/n ? (s * (n-1)) - (n - 2) : 
            mod(s * (n-1), 1),
        i = loop ? floor(mod(s * n, n)) :
            s < 0 ? 0 : 
            s > 1 - 1/n ? n - 2 : 
            floor(s * (n-1)),
        j = (i + 1) % n)
    [i, j, t];

// Collection of segments through a series of numbers or points in Rd
// Set loop to true to create a closed loop; otherwise, continues linearly after the endpoints
function path(x, loop=false) = function(s)
    // break [0,1] into (loop ? len(x) : len(x)-1) many pieces
    // i is which piece, t is how far into the piece
    let(params = segment(s, len(x), loop), 
        i = params[0], j = params[1], t = params[2])
    t == 0 ? x[i] :
    // consider only the two relevant endpoints
    to(x[i], x[j])(t);

// 3D spline through a list x of points and tangent to a parallel list d of vectors
// that is, for n = len(x) - 1, for all i ϵ [0:1:n], f(i/n) = x[i] and df/dt(i/n) || d[i]
function spline(x, d, loop=false) = function(s)
    // break [0,1] into len(x)-1 many pieces
    // i is which piece, t is how far into the piece
    let(params = segment(s, len(x), loop), 
        i = params[0], j = params[1], t = params[2])
    t == 0 ? x[i] :
    // consider only the two relevant endpoints
    let(x1 = x[i], x2 = x[j])
    let(c = [d[i], d[j]] * (is_num(x1) ? abs(x2 - x1) : norm(x2 - x1)),
        d1 = c[0], d2 = c[1],
        a = [ // constant, linear, square, cube coefficients
            x1,
            d1,
            3 * (x2 - x1) - 2 * d1 - d2,
            2 * (x1 - x2) + d2 + d1])
    sum([for(j=[0:1:3]) a[j] * pow(t, j)]);

// hilbert curve : t ϵ [0,1] -> [0,1] x [0,1]
// from [0, 0] to [1, 0]
function hilbert2(t) = 
    t < 0 ? hilbert2(-t) :
    t > 1 ? hilbert2(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    let(rest = hilbert2(t * 4 % 1) / 2)
    t < 1/4 ? [0, 0] + [rest.y, rest.x] :
    t < 1/2 ? [0, 0.5] + rest :
    t < 3/4 ? [0.5, 0.5] + rest :
    [0.5, 0] + [0.5 - rest.y, 0.5 - rest.x];

// hilbert curve : t ϵ [0,1] -> [0,1]^3
// from [0, 0, 0] to [1, 0, 0]
function hilbert3(t) =
    t < 0 ? hilbert3(-t) :
    t > 1 ? hilbert3(t - 1) :
    t == 0 ? [0, 0, 0] :
    t == 1 ? [1, 0, 0] :
    let(rest = hilbert3(t * 8 % 1) / 2)
    t < 1/8 ? [0, 0, 0] + [rest.y, rest.x, rest.z] :
    t < 1/4 ? [0, 0.5, 0] + [rest.z, rest.y, rest.x] :
    t < 3/8 ? [0, 0.5, 0.5] + [rest.z, rest.y, rest.x] :
    t < 1/2 ? [0, 0, 0.5] + [rest.x, 0.5 - rest.y, 0.5 - rest.z] :
    t < 5/8 ? [0.5, 0, 0.5] + [rest.x, 0.5 - rest.y, 0.5 - rest.z] :
    t < 3/4 ? [0.5, 0.5, 0.5] + [0.5 - rest.z, rest.y, 0.5 - rest.x] :
    t < 7/8 ? [0.5, 0.5, 0] + [0.5 - rest.z, rest.y, 0.5 - rest.x] :
    [0.5, 0, 0] + [0.5 - rest.y, 0.5 - rest.x, rest.z];

// dragon curve : t ϵ [0,1] -> R2
// goes from [0,0] to [1,0]
function dragon(t) =
    t < 0 ? dragon(-t) :
    t > 1 ? dragon(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    t < 1/2 ? let(rest = dragon(t * 2 % 1) / sqrt(2)) rot_k(rest, 45) :
    let(rest = dragon(1 - t * 2 % 1) / sqrt(2)) rot_k(rest, 135) + [1, 0];

// like the terdragon but with 5 segments at 72 degree angles
// t should be a list of base-5 digits (see base::lib-math)
function quindragon(t) =
    t == [] ? [0, 0] :
    t[0] == 5 ? [1, 0] :
    let(l = sin(18) / sin(108),
        rest = quindragon(cdr(t)) * l)
    let(a = [cos(54), sin(54)] * l,
        b = a + [cos(18), -sin(18)] * l,
        c = b + [0, -l],
        d = c + [cos(18), -sin(18)] * l)
    t[0] == 0 ? rot_k(rest, 54) :
    t[0] == 1 ? rot_k(rest, -18) + a :
    t[0] == 2 ? rot_k(rest, -90) + b :
    t[0] == 3 ? rot_k(rest, -18) + c :
    rot_k(rest, 54) + d;

// levy C curve : t ϵ [0,1] -> R2
// from [0,0] to [1,0]
function levy(t) =
    t < 0 ? levy(-t) :
    t > 1 ? levy(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    let(rest = levy(t * 2 % 1) / sqrt(2))
    t < 1/2 ? rot_k(rest, 45) :
    rot_k(rest, -45) + [1/2, 1/2];

// open koch curve : t ϵ [0,1] -> [0,1]^2
// from [0,0] to [1,0]
// 2w + 2w sin(a/2) = 1
// w (2 + 2sin(a/2) = 1
function koch(t, a=60) = 
    t < 0 ? koch(-t) :
    t > 1 ? koch(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    // width of the first section
    let(w = 1 / (2 + 2 * cos(a)))
    // height and half-width of the middle triangle
    let(h = w * sin(a), g = w * cos(a))
    let(rest = koch(t * 4 % 1, a) * w)
    t < 1/4 ? rest : 
    t < 1/2 ? rot_k(rest, a) + [w, 0] :
    t < 3/4 ? rot_k(rest, -a) + [w + g, h] :
    rest + [1-w, 0];


// parametric cubic branching (parametrize [0,1]^3)
function octahedra(t, n=0) = 
    n > 30 ? v0(3) :
    let(next = t < 1/6 ? i3 : t < 1/3 ? -i3 : t < 1/2 ? j3 :
            t < 2/3 ? -j3 : t < 5/6 ? k3 : -k3,
        rest = octahedra(t * 6 % 1, n+1))
    next + rest * (sqrt(5) - 1) / 2;

// parametric octal branching (parametrize [-1, -1]^3)
function hexahedra(t, n=0) =
    t == 0 ? v0(3) :
    let(next = (t < 1/2 ? i3 : -i3) + 
            (t % (1/2) < 1/4 ? j3 : -j3) + 
            (t % (1/4) < 1/8 ? k3 : -k3),
        rest = hexahedra(t * 8 % 1, n+1))
    next + rest * 0.618;







