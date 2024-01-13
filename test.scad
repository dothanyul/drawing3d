// tests for lib-draw
// animate tests 24 fps 90 steps
include <lib-draw.scad>


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
            v1 = rot_i(10, rot_j(20, [cos(t), sin(t), 0])) * 10;
            v2 = v1 + rot_k(t, [-cos(t * a + b), 0, sin(t * a + b)]) * 4;
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
    n = 200;
    g = function(t)
        t < 0 ? g(t + 1) :
        t < 1/8 ? [1, (-cos(t * 4 * 360) - 1) / 2, sin(t * 4 * 360) * 0.7] :
        t < 1/4 ? [1, (1 + cos(t * 4 * 360)) / 2, sin(t * 4 * 360) * 0.7] :
        t < 1/2 ? [-1,-1,0] * -cos((t - 1/4) * 2 * 360) - k3 * sin(t * 2 * 360) * 1.4 :
        t < 5/8 ? [-1, (-cos(t * 4 * 360) - 1) / 2, -sin(t * 4 * 360) * 0.7] :
        t < 3/4 ? [-1, (1 + cos(t * 4 * 360)) / 2, -sin(t * 4 * 360) * 0.7] :
        t < 1 ? [1,-1,0] * -cos((t - 3/4) * 2 * 360) + k3 * sin(t * 2 * 360) * 1.4 :
        g(t - 1);
    f = function(t) g(t * 2);
    for(t=[0:1:n]) {
        translate(f(t/n) * 30) color(rainbow(t, n / 2)) sphere(1);
    }

    a = follow(f, n, []);
    echo(a[t], det(a[t]));
    p = 6;
    r = 8;
    t = $t * n;
    translate(f(t / n) * 30) {
        for(x=[i3, j3, k3]) {
            color(color_switch(x * 255)) hull() {
                sphere(2);
                translate(mat_apply(a[t], x * 10)) sphere(1);
            }
        }
        % sphere(10);
    }
}

module misc_test() {
    % sphere(r, $fn=40);
    c = function(v1, v2) norm(v1 + v2) < norm(v1 - v2) ? "#FF0000" : "#22FF44";
    v1 = [-1, -1, 0];
    r = 50; fn = 30;
    translate(v1 * r) color("#AAAAAA") sphere(10, $fn=fn);
    for(q=[0:1:1]) {
        for(p=[q/2:0.01:q/2+0.25], t=[0:15:359]) {
            a2 = 1;
            v2 = rot_j(180 - $t * 360 * 5, rot_i(t + q * 7.5, [cos((p + $t) * 360 * a2), sin((p + $t) * 360 * a2), 0]));
            d = 18 * (p - q/2) + 0.01;
            translate(v2 * r) color(c(v1, v2)) sphere(d/2, $fn=fn);
        }
    }
}











