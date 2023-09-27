// tests for lib-draw
// animate tests 24 fps 90 steps
include <lib-draw.scad>

module align_test() {
    t = $t * 360;
    v1 = [cos(t), sin(t), 0] * 10;
    a = 9;
    v2 = v1 + [sin(t * a), 0, cos(t * a)] * 3;
    translate(v1) sphere(1);
    translate(v2) sphere(1);
    translate(mat_apply(align(v1, v2), v2)) sphere(1);
}

module follow_test() {
    f = function(t) [cos(t * 360), 4 * t, sin(t * 360)];
    n = 100;
    a = follow(f, n, []);
    for(t=[0:1:n]) {
        translate(f(t / n) * 30) hull() {
            sphere(1);
            echo(a[t]);
            translate(mat_apply(a[t], [0, 0, 10])) sphere(1);
        }
    }
}

align_test();