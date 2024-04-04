// tests for lib files
// animate tests 24 fps 90 steps

include <lib-draw.scad>
include <lib-func.scad>
include <lib-view.scad>


module truncate_test() {
    for(v = [
        [64, 2, 64],
        [65, 2, 64],
        [63, 2, 48],
        [-64, 2, -64],
        [0.01, 2, 1/128]]) {
        t = truncate(v[0], v[1]);
        if(t != v[2]) echo(v[0], v[1], t);
    }
}

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
    d = 100;
    m = 2 * PI;
    n = 4;
    p = function(t) 
        t > 1 ? p(t-1) : 
        t < 0 ? p(t+1) :
        t > 1/2 ? let(x=p(1-t)) [x.x, -x.y] :
        t < 1/4 ? [1 - 4 * t, 4 * t] :
        t < 5/16 ? [0, 1 - (t * 8 - 2)] :
        t < 7/16 ? [(5/16 - t) * 8, 1/2] :
        [-1, 1/2 - (t - 7/16) * 8];
    f = function(s) 
        [cos(360*s*n), m * s, sin(360*s*n)] * d;
    sweep(function(t) p(t) * 10, f, nt = 16, ns = 200);
    translate([d, m * d, 0.4]) origin(10);
}

module loft_test() {
    p1 = function(t) [cos(t * 360), sin(t * 360), 0];
    p2 = function(t) let(x = 
        t < 0.25 ? segment([1,1], [-1,1])(t * 4) :
        t < 0.5 ? segment([-1,1], [-1,-1])(t * 4 - 1) :
        t < 0.75 ? segment([-1,-1], [1,-1])(t * 4 - 2) :
        segment([1,-1], [1,1])(t * 4 - 3)
    ) [x.x, x.y, 2];
    d1 = f_0(2);
    d2 = f_0(2);
    loft(p1, p2, d1, d2);
}

module stroke_test() {
    x1 = raise(rands(-20, 20, 2), 3);
    x2 = raise(rands(-20, 20, 2), 3);
    echo(x1, x2);
    for(x = [x1, x2]) translate(x) origin(3, align(i3, x2 - x1), $fn = 6);
    s = stroke(x1, x2, 5, 2/3);
    n = 6;
    for(t = [0:1:n]) translate(s(t / n)) color(rainbow(t / n)) sphere(1);
}

module spline_test() {
    x = [[-3, 5, -2], [5, -7, 10], [1, 4, 5]];
    d = [[0, 0, 1], [1, 1, 0], [0, 0, 1]];
    n = 20;
    if(false) {
        for(t = [0:1:n]) {
            x1 = spline(x, d)(t/n);
            for(i = [0:1:2]) {
                c = ["#F00", "#0F0", "#00F"][i];
                translate([t, x1[i], 0]) color(c) sphere(0.5);
            }
        }
    } else {
        for(i = [0:1:len(x)-1]) translate(x[i]) face(d[i]) origin(3);
        for(t = [-2:1:n+2]) translate(spline(x, d)(t/n)) color(rainbow(t/n)) sphere(1);
    }
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

module visible_test() {
    s = 20;
    d = 4;
    for(x = [-s:d:s]) for(y = [-s:d:s]) for(z = [-s:d:s]) {
        v = [x, y, z];
        if(!visible(v)) translate(v) sphere(1);
    }
}

module world_test() {
    origin(10);
    for(v = [[-1, 1, 0], [1, 1, 0], [-1, -1, 0], [1, -1, 0]]) {
        w = world(v);
        translate(w) rotate($vpr) sphere($vpd / 100, $fn = 5);
    }
}

// stuff and tests for stuff that's not stable enough yet to put in a regular lib file

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

        
































