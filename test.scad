// tests for lib-draw and lib-func
// animate tests 24 fps 90 steps

include <lib-draw.scad>
include <lib-func.scad>


module rot_test() {
    r = 30;
    for(t = [0:10:360]) color(rainbow(t / 360)) {
        translate(rot_k(i3, t) * r) sphere(1);
        translate(rot_j(i3, t) * r) sphere(1);
        translate(rot_i(j3, t) * r) sphere(1);
    }
}

module align_test(base=false) {
    if(base) {
        for(x=allsum([i3, -i3, j3, -j3, k3, -k3] * 10)) {
            echo(x);
            hull() {
                sphere(1);
                translate(mat_apply(align(x, x), x)) sphere(1);
            }
        }
    } else {
        a = 9; b = 0;
        for(s = [for(i=[0:1:5], j=[0:1:5]) i * 10 + j * 0.8]) {
            t = $t * 360 - s;
            r = s == 0 ? 1.5 : 1 - s / 100;
            v1 = rot_i(rot_j([cos(t), sin(t), 0], 20), 10) * 10;
            v2 = v1 + rot_k([-cos(t * a + b), 0, sin(t * a + b)], t) * 4;
            v3 = mat_apply(align(v1, v2), v2);

            n = 20;
            color("#FF0000") translate(v1) sphere(r, $fn=n);
            color("#00FF00") translate(v2) sphere(r, $fn=n);
            color("#FFFF00") translate(v3) sphere(r, $fn=n);
            color("#FFAA00") translate(v3 / norm(v3) * 10) cube(r * 1.6 , center=true);
        }
    }
}

module follow_test() {
    n = 80;
    f = function(t)
        t < 0 ? f(t + 1) :
        t < 1/8 ? [1, (-cos(t * 4 * 360) - 1) / 2, sin(t * 4 * 360) * 0.7] :
        t < 1/4 ? [1, (1 + cos(t * 4 * 360)) / 2, sin(t * 4 * 360) * 0.7] :
        t < 1/2 ? [-1,-1,0] * -cos((t - 1/4) * 2 * 360) - k3 * sin(t * 2 * 360) * 1.4 :
        t < 5/8 ? [-1, (-cos(t * 4 * 360) - 1) / 2, -sin(t * 4 * 360) * 0.7] :
        t < 3/4 ? [-1, (1 + cos(t * 4 * 360)) / 2, -sin(t * 4 * 360) * 0.7] :
        t <= 1 ? [1,-1,0] * -cos((t - 3/4) * 2 * 360) + k3 * sin(t * 2 * 360) * 1.4 :
        f(t - 1);
    for(t=[0:0.2:n]) {
        translate(f(t/n) * 30) color(pulse(rainbow(t * 2 / n), "#444", 4)(t)) sphere(t % 1 == 0 ? 1 : 0.5);
    }
    
    a = follow(f, n);
    p = 6;
    r = 8;
    t = $t * n;
//    echo(a[t], det(a[t]));
    translate(f(t / n) * 30) origin(10, a[t], false);
}

module sweep_test() {
    p = function(t) 
        t > 1 ? p(t-1) : 
        t < 0 ? p(t+1) :
        t > 1/2 ? let(x=p(1-t)) [x.x, -x.y] :
        t < 1/4 ? [1 - 4 * t, 4 * t] :
        t < 5/16 ? [0, 1 - (t * 8 - 2)] :
        t < 7/16 ? [(5/16 - t) * 8, 1/2] :
        [-1, 1/2 - (t - 7/16) * 8];
    f = function(s) 
        [cos(360*s), s, sin(360*s)] * 100;
    sweep(p, f);
}

module loft_test() {
    p1 = function(t) [cos(t * 360), sin(t * 360), 0];
    p2 = function(t) let(x = 
        t < 0.25 ? segment([1,1], [-1,1])(t * 4) :
        t < 0.5 ? segment([-1,1], [-1,-1])(t * 4 - 1) :
        t < 0.75 ? segment([-1,-1], [1,-1])(t * 4 - 2) :
        segment([1,-1], [1,1])(t * 4 - 3)
    ) [x.x, x.y, 2];
    d1 = zero(2);
    d2 = zero(2);
    loft(p1, p2, d1, d2);
}

module stroke_test() {
    x1 = raise(rands(-10, 10, 2), 3);
    x2 = raise(rands(-10, 10, 2), 3);
    echo(x1, x2);
    for(x = [x1, x2]) translate(x) origin(3, align(i3, x2 - x1), $fn = 6);
    s = stroke(x1, x2, 5);
    n = 6;
    for(t = [0:1:n]) translate(s(t / n)) color(rainbow(t / n)) sphere(1);
}

module hilbert2_test() {
    s = 40;
    a = 8;
    b = 16;
    for(i = [0:1:a]) for(j = [0:1:b]) {
        k = i * b + j;
        c = a * b;
        x0 = hilbert2(k / c);
        dx = hilbert2((k == c ? c : k + 1) / c) - x0;
        color(rainbow(k / c)) translate(x0 * s) hull() {
            cylinder(1, 1, 1);
            translate(dx * s) cylinder(1, 1, 1);
        }
    }
}

module hilbert3_test(s=50, c=256, d=1, r=1) {
    sweep_nondifferentiable(function(t) hilbert3(t) * s, r, $fn=c);
    % frame([s, s, s], s / 12);
}

// stuff and tests for stuff that's not stable enough yet to put in a regular lib file

// convert rgb color to hsv
function hsv(rgb) = 
    is_string(rgb) ? color_str(hsv(color_list(rgb))) :
    is_num(rgb) ? color_num(hsv(color_list(rgb))) :
    let(v = sum(rgb),
        hs = [0.5, sqrt(3) / 2] * rgb[0] + [-1, 0] * rgb[1] + [0.5, -sqrt(3) / 2] * rgb[2])
    [mod(vec_ang(hs) / 360, 1), norm(hs), v / 3];

module hsv_test() {
    s = 100;
    l = 24000;
    origin(s / 10);
    % frame([s, s, s], 4);
    for(i = [0:1:999]) {
        n = rands(0, 1, l, 0)[l * $t + i];
        x = color_list(n);
        c = rgb(x);
        translate(x * s) color(color_str(c * 255)) rotate($vpr) cube(3, center=true);
    }
}

// convert hsv color to rgb
function rgb(hsv) = 
    is_num(hsv) ? color_num(rgb(color_list(hsv))) :
    is_string(hsv) ? color_str(rgb(color_list(hsv))) :
    let(t = hsv[0] * 6)
    let(hue = (function(t)
        t < 1 ? [1, t, 0] :
        t < 2 ? [2 - t, 1, 0] :
        t < 3 ? [0, 1, t - 2] :
        t < 4 ? [0, 4 - t, 1] :
        t < 5 ? [t - 4, 0, 1] :
        [1, 0, 6 - t])
        (hsv[0] * 6))
    let(huesat = hue / (1 - hsv[1]) + [for([0:1:2]) hsv[1]])
    huesat * hsv[2];

// points is an unordered list of points to form the centers
// bl is bottom left, tr is top right of the rectangle to bound the pattern
module fortune_voronoi(points, bl, tr) {
    sorted = sort(points, function (a,b)
    is_undef(a) || is_undef(b) ? 0 :
    a.x > b.x ? 1 : a.x < b.x ? -1 :
    a.y > b.y ? 1 : a.y < b.y ? -1 :
    a.z > b.z ? 1 : a.z < b.z ? -1 :
    0, []);
    for(i=[0:1:len(sorted)-1]) {
        translate(sorted[i]) color(rainbow(i / len(sorted))) cube(3, center=true);
    }
}

//bl = [0,0,0];
//tr = [100,100,0];
//seed = 3;
//num_points = 50;
//points = generate(bl, tr, [], [[],[]], 0, num_points, 5, seed);
//fortune_voronoi(points, bl, tr);

        
































