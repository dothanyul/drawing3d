// tests for lib-draw
// animate tests 24 fps 90 steps
include <lib-draw.scad>

module align_test() {
    a = 9; b = 0;
    for(s = [for(i=[0:1:5], j=[0:1:5]) i * 10 + j * 0.8]) {
        t = $t * 360 - s;
        r = s == 0 ? 1.5 : 1 - s / 100;
        v1 = rot_i(10, rot_j(20, [cos(t), sin(t), 0])) * 10;
        v2 = v1 + rot_k(t, [-cos(t * a + b), 0, sin(t * a + b)]) * 4;
        v3 = mat_apply(align(v1, v2), v2);

        n = 20;
        color("#FF0000") translate(v1) sphere(r, $fn=n);
        color("#00FF00") translate(v2) sphere(r, $fn=n);
        color("#FFFF00") translate(v3) sphere(r, $fn=n);
        color("#FFAA00") translate(v3 / norm(v3) * 10) cube(r * 1.6 , center=true);
    }
    
    for(x=allsum([i3, -i3, j3, -j3, k3, -k3] * 10)) {
        hull() {
            sphere(1);
            translate(align(x, x)(x)) sphere(1);
        }
    }
}

module follow_test() {
    f = function(t) 
        t < 0.25 ? [-1, 1 - t * 4, t * 16] :
        t < 0.75 ? [cos(t * 720), t * 4, sin(t * 720)] : 
        [-1, 7 - t * 4, (t - 1) * 16];
    n = 100;
    a = follow(f, n, []);
    for(t=[0:1:n]) {
        translate(f(t / n) * 30) hull() {
            sphere(10);
            translate(mat_apply(a[t], [0, 0, 10])) sphere(1);
        }
    }
}

