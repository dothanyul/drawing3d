// parametric functions and functions to deal with them
include <lib-math.scad>

// zero function in Rn
function f_0(n) = 
    n < 1 ? undef :
    n == 1 ? function(t) 0 :
    function(t) vec_0(n);

/*
Parametric function [0,1] -> Rd for a line segment from a to b
*/
function segment(a, b) = function(t)
    a + (b - a) * t;

// 90 degree elliptical arc from x1 to x2 in R2 for t ϵ [0,1]
// left handed or right handed curvature
function arc(x1, x2, left = true) = 
    x1 == x2 ? function(t) x1 :
    x1.x == x2.x || x1.y == x2.y ? segment(x1, x2) :
    function(t) left ? 
    [x1.x + (x2 - x1).x * (1 - cos(t * 90)), x1.y + (x2 - x1).y * sin(t * 90)] :
    [x1.x + (x2 - x1).x * sin(t * 90), x1.y + (x2 - x1).y * (1 - cos(t * 90))];

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
        t < 0.5 - c / 2 ? segment(x1, x2)((t - c / 2) / b) + rot_k(d, -90) * r :
        t < 0.5 + c / 2 ? let(h = (t - (0.5 - c / 2)) / c * 180) x2 + rot_k([cos(h), sin(h)], atan2(d.y, d.x) - 90) * r :
        t < 1 - c / 2 ? segment(x2, x1)((t - 0.5 - c / 2) / b) + rot_k(d, 90) * r :
        let(h = (t - 1 + c / 2) / (c / 2) * 90) x1 + rot_k([cos(h), sin(h)], atan2(d.y, d.x) + 90) * r;

// 3D spline through a list x of points and tangent to a parallel list d of vectors
// that is, for n = len(x) - 1, for all i ϵ [0:1:n], f(i/n) = x[i] and df/dt(i/n) || d[i]
function spline(x, d) = function(s)
    // break [0,1] into len(x)-1 many pieces
    // i is which piece, t is how far into the piece
    let(t = s < 0 ? s : 
            s > 1 ? (s * (len(x) - 1)) - (len(x) - 2) : 
            (s * (len(x) - 1) % 1),
        i = s < 0 ? 0 : 
            s > 1 ? len(x) - 2 : 
            floor(s * (len(x) - 1)))
    t == 0 ? x[i] :
    // consider only the two relevant endpoints
    let(x1 = x[i], x2 = x[i+1])
    let(c = [d[i], d[i+1]] * norm(x2 - x1))
    let(d1 = c[0], d2 = c[1])
    let(a = [ // constant, linear, square, cube coefficients
        x1,
        d1,
        3 * (x2 - x1) - 2 * d1 - d2,
        2 * (x1 - x2) + d2 + d1])
    sum([for(j=[0:1:3]) a[j] * pow(t, j)]);

// hilbert curve : t ϵ [0,1] -> [0,1] x [0,1]
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

// dragon curve : t ϵ [0,1] -> [0,1]^2
function dragon(t) =
    t < 0 ? dragon(-t) :
    t > 1 ? dragon(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    t < 1/2 ? let(rest = dragon(t * 2 % 1) / sqrt(2)) rot_k(rest, 45) :
    let(rest = dragon(1 - t * 2 % 1) / sqrt(2)) rot_k(rest, 135) + [1, 0];

// levy C curve : t ϵ [0,1] -> [0,1]^2
function levy(t) =
    t < 0 ? levy(-t) :
    t > 1 ? levy(t - 1) :
    t == 0 ? [0, 0] :
    t == 1 ? [1, 0] :
    let(rest = levy(t * 2 % 1) / sqrt(2))
    t < 1/2 ? rot_k(rest, 45) :
    rot_k(rest, -45) + [1/2, 1/2];




