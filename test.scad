// tests for lib files
// animate tests 24 fps 90 steps

include <lib-color.scad>
include <lib-draw.scad>
include <lib-func.scad>
include <lib-tree.scad>
include <lib-view.scad>


// tests for lib-color
{
module color_spline_test() {
    n = 720;
    h = 2;
    r = 30;
    p = [[0, 0, 0], [0, 0, h], 
        each [for(t = [0:360/n:360]) each [
            [r * cos(t), r * sin(t), 0], 
            [r * cos(t), r * sin(t), h]]]];
    c = color_spline([[1.00, 0.57, 0.1], [0.98, 0.94, 0.20], [0.65, 0.42, 0.2], [0.60, 0.95, 0], [0.95, 0.45, 0.97], [0.9, 0.9, 0.8]] // autumn
        );
    for(t=[0:2:2*n]) color(c(t/n/2)) polyhedron(
        [for(i=[0, 1, each [for(j=[0:1:3]) t + j]]) p[i]],
        [[0, 1, 3, 2], [0, 2, 4], [1, 0, 4, 5], [1, 5, 3], [2, 3, 5, 4]],
        1);
}

//color_spline_test();

module rainbow_test() {
    if(true) {
        n = 720;
        h = 2;
        r = 30;
        p = [[0, 0, 0], [0, 0, h], 
            each [for(t = [0:360/n:360]) each [
                [r * cos(t), r * sin(t), 0], 
                [r * cos(t), r * sin(t), h]]]];
        for(t=[0:2:2*n]) rotate([0, -20, 0]) color(rainbow(t/n/2)) polyhedron(
            [for(i=[0, 1, each [for(j=[0:1:3]) t + j]]) p[i]],
            [[0, 1, 3, 2], [0, 2, 4], [1, 0, 4, 5], [1, 5, 3], [2, 3, 5, 4]],
            1);
    } else {
        n = 36;
        s = 100;
        for(i = [0:1:n]) let(t=i/n) 
            let(c = rainbow(t))
            translate(color_list(c) * s) color(c) sphere(4);
        % frame([s, s, s], 5);
    }
}

module randcolor_test() {
    s = 3;
    d = 1;
    v = [for(x = [-s:d:s], y = [-s:d:s], z = [-s:d:s],
            m = [for(i = [i3, j3, k3], c = [1/s:1/s:1]) c * i])
        [x, y, z] + m + rands(0, 0.1, 3)];
    for(x = v) translate(x) color(randcolor(x)) cube(0.1);
    c = color_sum([for(x = v) randcolor(x)]);
    translate([0, 0, -s * 2]) color(c) 
        cube([s * 3, s * 3, 0.1], center=true);
}

module color_sum_test() {
    c = [for(b = ["#F00", "#FF0", "#0F0", "#0FF", "#00F", "#F0F"])
        each [for(i=[0:1:10]) b]];
    for(i=[0:1:len(c)-1]) translate([i * 2, 2, 0]) color(c[i]) cube(1);
    color(color_sum(c)) cube(1);
}

module synth_test() {
    background(c=gray(0.30), w=200, d=20);
    for(i=[0:1:10]) translate([i * 3, 0, 0]) {
        prim = synth(i);
        sec = synth(i, 1);
        tert = synth(i, 2);
        quat = synth(i, 3);
        
        d1 = 0.13;
        d2 = 0.05;
        w = 0.4;
        
        color(prim) cube(1);
        if(sec != prim) translate([-d1, -d1, 1 - d1 - w]) color(sec) cube([1 + d1 * 2, 1 + d1 * 2, w]);
        if(tert != prim) {
            if(quat != prim) translate([0, 0, 1 - d2 - d1]) {
                rotate([1, 1, 0] * 2) color(quat) linear_extrude(d1 * 0.4) polygon([[1+d2, 1+d2], [-d2, -d2], [-d2, 1+d2]]);
                rotate([-1, -1, 0] * 2) color(tert) linear_extrude(d1 * 0.4) 
                    polygon([[1+d2, -d2], [-d2, -d2], [1+d2, 1+d2]]);
            }
            else translate([-d2, -d2, 1 - d2 - d1]) color(tert) cube([1 + d2 * 2, 1 + d2 * 2, d1]);
        }
    }
}

//synth_test();
}

// tests for lib-tree
{

t = [8, [
        [0, [
            [0, [0, 1]],
            [2, [0, 1]]
        ]],
        [4, [
            [0, [0, 1]],
            [2, [0, 1]]]]
    ]];

module treesum_test() {
    for(s = cdr(t)) for(r = s) 
        for(q = cdr(r)) for(p = q) 
            for(o = cdr(p)) for(n = o)
                echo(n + p[0] + r[0] + t[0]);
    echo(treesum(t));
}

module leaves_test() {
    echo(leaves(t));
}

module treefold_test() {
    echo(treefold(t, function(a, b) echo(a, b) is_undef(a) ? b : a + b));
}
}

// tests for lib-math
{

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

module logrest_test() {
    s = 100;
    n = 128;
    for(x1=[1:1:n]) let(x = x1 / n) translate([x, logrest(x), 0] * s) sphere(1);
}

module base_test() {
    for(i=[0:1:25]) let(l=base([i, 25], 5, n=16)) echo(l);
}

module width_test() {
    c = "FFF"; h = 5; 
    x = 0.568;
    $vpt = [3, 2, 0];
    $vpd = 17.8;
    color("#BC5") translate([0, 0, 1]) text(c, size=h);
    color("#222") translate([width(c, h)/2 + x, -1, 0]) cube([1, 100, 1]);
    color("#222") translate([x, -1, 0]) cube([1, 100, 1]);
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

}

// tests for lib-draw
{

module sector_test() sector(10, 7, (1 - 2 * $t) * 360, $fn=30);

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
        rot_k([cos(s * 360 * n) + 2, 0, sin(s * 360 * n)] * d, s * 360);
//        [cos(360*s*n), m * s, sin(360*s*n)] * d;
    sweep(function(t) p(t) * 10, f, nt = 16, ns = 200);
    translate([d, m * d, 0.4]) origin(10);
}

module loft_test() {
    p1 = function(t) [cos(t * 360), sin(t * 360), 0];
    p2 = function(t) let(x = 
        t < 0.25 ? to([1,1], [-1,1])(t * 4) :
        t < 0.5 ? to([-1,1], [-1,-1])(t * 4 - 1) :
        t < 0.75 ? to([-1,-1], [1,-1])(t * 4 - 2) :
        to([1,-1], [1,1])(t * 4 - 3)
    ) [x.x, x.y, 2];
    d1 = f_0(2);
    d2 = f_0(2);
    loft(p1, p2, d1, d2);
}

}

// tests for lib-gear
{

// a little demo that uses all the kinds of gears
module gears_test() {
    all = true;
    module axle() cylinder(thick * 2, 1.5, 1.5, center = true, $fn = 8);
    if(false || all) ring(16, right = true);
    if(false || all) translate([radius(8), 0, 0]) rotate([0, 0, 360 / 16]) 
    difference() {
        spur(8, right = false);
        axle();
    }
    dz = -13;
    wide = radius(16) + 4;
    if(false || all) translate([radius(8), 0, dz]) rotate([180, 0, 360 * 0.5/12]) 
    difference() {
        bevel(12, 8, right = true);
        axle();
    }
    if(false || all) translate([radius(8) - radius(12), 0, dz - radius(8)]) rotate([0, 90, 0]) 
    difference() {
        bevel(8, 12, right = false);
        axle();
    }
    if(false || all) translate([-wide, 0, dz - radius(8)]) rotate([0, -90, 0]) 
    difference() {
        spur(12, right = true);
        axle();
    }
    if(false || all) translate([-wide, -T / 2, dz + radius(4)]) rotate([-90, 0, 90]) 
        rack(10, dr = d * 3 - 0.5);
    if(false || all) difference() {
        union() {
            translate([-wide - thick, -wide, -4.3]) cube([wide * 2 + thick, wide * 2, 4]);
            translate([-wide, -10, -31]) cube([8.7, 20, 30]);
            translate([0, 0, -1]) difference() {
                union() {
                    diag = wide * 1.18;
                    rotate([0, 0, 45]) translate([-diag, -5, 0])
                        cube([diag * 2, 10, 5]);
                    rotate([0, 0, -45]) translate([-diag, -5, 0])
                        cube([diag * 2, 10, 5]);
                }
                cylinder(15, radius(16) + d * 2.2, radius(16) + d * 2.1, $fn = 100);
            }
        }
        translate([radius(8) - radius(12) - 10, 0, dz - radius(8)]) rotate([0, 90, 0]) axle();
        translate([radius(8), 0, 0]) axle();
    }
}

}

// tests for lib-func
{

module stroke_test() {
    x1 = raise(rands(-20, 20, 2), 3);
    x2 = raise(rands(-20, 20, 2), 3);
    echo(x1, x2);
    for(x = [x1, x2]) translate(x) origin(3, align(i3, x2 - x1), $fn = 6);
    s = stroke(x1, x2, 5, 2/3);
    n = 6;
    for(t = [0:1:n]) translate(s(t / n)) color(rainbow(t / n)) sphere(1);
}

module path_test() {
    points = [[0,0,0], [0,1,.1], [1,1,.2], [1,0,.3]] * 10;
    f = path(points, loop=false);
    for(t = [-0.5:1/12:1.5])  translate(f(t) + [0, 0, t]) 
        color(rainbow(t)) sphere(1);
}

module spline_test() {
    x = [[-3, 5, -2], [5, -7, 10], [1, 4, 5]];
    d = [[0, 0, 1], [1, 1, 0], [0, 0, 1]];
    f = spline(x, d, false);
    n = 20;
    if(false) {
        for(t = [0:1:n]) {
            x1 = f(t/n);
            for(i = [0:1:2]) {
                c = ["#F00", "#0F0", "#00F"][i];
                translate([t, x1[i], 0]) color(c) sphere(0.5);
            }
        }
    } else {
        for(i = [0:1:len(x)-1]) translate(x[i]) face(d[i]) origin(3);
        for(t = [-2:1:n+2]) translate(f(t/n)) color(rainbow(t/n)) sphere(1);
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

module dragon_test() {
    s = 100;
    n = 625;
    f = function(t)
        quindragon(t) * s;
    c = function(t) rainbow(t/n);
//    for(t = [0:1:n]) translate(f(t/n)) color(rainbow(t/n)) sphere(2);
    sweep_nondifferentiable(f, 0.2, c, n = n, $fn=4);
    % frame([s, s, 1]);
}

module hedra_test() {
    n = 5 * 7 * 11 * 13;
    echo(n);
    for(i = [0:1:n]) {
        t = i / n;
        translate(hexahedra(t) * 20) color(rainbow(t)) sphere(1);
    }
}

//hedra_test();
}

// tests for lib-view
{

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
        translate(w) rotate($vpr) sphere($vpd / 50, $fn = 5);
    }
}

}

// stuff and tests for stuff that's not stable enough yet to put in a regular lib file
{

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

}
































