include <lib-math.scad>
include <lib-color.scad>
include <lib-func.scad>

// functions related to drawing things

// polar function for the radius of an n-gon with radius 1
function poly(n) = function(t) 
    (cos(180 / n) / cos(t % (360 / n) - 180 / n));
    
// draw a polygon given a parametric continuous 1-periodic function p : [0,1] -> R2
module fill(p, $fn=40) {
    dt = 1 / $fn;
    polygon([for(i=[0:1:$fn-1]) p(dt * i)]);
}

/*
Draw positive x,y,z axes of length size transformed by function or matrix f
*/
module origin(size=1, f=id, ball=true) {
    function f1(x) = is_function(f) ? f(x) * size : 
        is_matrix(f) ? mat_apply(f, x) * size :
        x * size;
    if(ball) {
        for(x=[i3, j3, k3]) color(color_str(x * 255)) hull() {
            sphere(size / 5);
            translate(f1(x)) sphere(size / 10);
        }
        % sphere(size, $fn = 20);
    } else {
        // sorted by z decasing then y decreasing then x decreasing
        // so 0 = [1, 1, 1], then going down in z is an increase by 9, y an increase by 3, and x an increase by 1
        points = [let(d = 0.25) for(k = [k3, vec_0(3), -k3 * d]) for(j = [j3, vec_0(3), -j3 * d]) for(i = [i3, vec_0(3), -i3 * d]) f1(i + j + k)];
        // faces of tetrahedra along each axis
        for(p = [[13, 16, 22, 12, i3], [13, 22, 14, 10, j3], [13, 14, 16, 4, k3]]) {
            faces = [for(f = [[0, 1, 2], [0, 2, 3], [0, 3, 1], [1, 3, 2]]) [for(i = f) p[i]]];
            color(color_str(p[4])) polyhedron(points, faces, 1);
        }
    }
}
/*
Draw square edges of thickness r around a cube with dimensions [width, depth, height]
*/
module frame(dimensions, r=1, center=false) {
    let(i = dimensions[0], j = dimensions[1], k = dimensions[2])
    difference() {
        translate([-r, -r, -r] * (center ? 0 : 1)) cube([i + r * 2, j + r * 2, k + r * 2], center=center);
        for(v = [i3, j3, k3]) {
            translate(-v * r * 2 * (center ? 0 : 1)) cube([i, j, k] + v * r * 4, center=center);
        }
    }
}

/*
Regular pyramid with base radius r, height h, and n sides
*/
module pyramid(r, h, n) {
    points = [[0, 0, h], 
        each [for(t=[0:360/n:359.9])
            r * [cos(t), sin(t), 0]
        ]
    ];
    faces = [[for(i=[1:1:n]) i], 
        each [for(i=[1:1:n])
            i == n ? [0, 1, n] : [0, i+1, i]
        ]
    ];
    polyhedron(points, faces, 1);
}


/**
Randomly populate a space with a color function f : [0,1]^3 -> rgb
*/
module colorspace(f) {
    for(i=[0:1:1000]) {
        v = rands(0, 1, 3);
        translate(v * 30) color(f(v)) sphere(1);
    }
}

/**
pass
    two parametric functions p1,p2 : tϵ[0,1] -> R2 x [[0,0,h]] for top and bottom faces
        must be simple closed curves
    two parametric functions d1,d2 : tϵ[0,1] -> R for the slopes at the ends
        in the plane <k3, p2(t) - p1(t)> with k3 horizontal, so 0 = vertical
this function defines a curved face in R3 between these two curves thus:
    construct a parametric spline s : [t,z] -> r in the plane P = [r cos t, r sin t, z] where
        angle t ranges from 0 to 1 * 360 degrees
        height z ranges from 0 to h
        radius r is positive
    such that for all t,
        s(t,0) = r1(t)
        s(t,h) = r2(t)
        ∂s/∂z(t,0) = d1(t)
        ∂s/∂z(t,h) = d2(t)
    see documentation on spline:lib-func.scad for more info
break t into intervals of width i = 1/nt
calculate L = average of path lengths of r1 and r2 and break h into equal-length intervals of width roughly L / nt (number of intervals is rounded up)
sample the surface of s at this density to create a square grid of points over it
pass these points and squares (as well as the top and bottom faces) to polyhedron()
*/
module loft(p1, p2, d1 = zero(2), d2 = zero(2), nt = $preview ? 20 : 360, nz = $preview ? 10 : 0) {
    // segment t : [0,1]
    dt = 1 / nt;

    // segment h
    h = p2(0).z - p1(0).z;
    nz1 = (nz == 0) ?
        // calculate the average length of the two perimeters
        let(l1 = length(p1, dt), l2 = length(p2, dt))
        let(L = (l1 + l2) / 2)
        // split h into segments as wide as those on perimeter L
        ceil(nt * h / L)
    : nz;
    dz = h / nz1;

    // create the square net of points
    // loop over angles, then heights
    points = [for(i = [0:1:nz1])
        each [for(j = [0:1:nt-1])
        // define the radius at an angle and height
        let(z1 = p1(0).z + i * dz, z2 = p1(0).z + (i + 0.5) * dz, t1 = j * dt, t2 = (j + 0.5) * dt)
        let(s1 = (spline(p1(t1), p2(t1), d1(t1), d2(t1))(z1)),
            s2 = (spline(p1(t2), p2(t2), d1(t2), d2(t2))(z2)))
        // transform that into x,y,z
        each (i == nz1 ? [[s1.x, s1.y, z1]] :
        [[s1.x, s1.y, z1], [s2.x, s2.y, z2]])
    ]];
    
    // create the faces in the net of points
    faces = [
        // bottom face
        [for(i=[0:2:2*nt-1]) i],
        // top face
        [for(i=[0:1:nt-1]) len(points) - 1 - i],
        // other faces
        each [for(i = [0:2:2*nz1-2], j = [0:2:2*nt-2])
            let(bl = i * nt + j,
                br = i * nt + (j + 2) % (nt * 2),
                c = i * nt + j + 1,
                tl = i / 2 == nz - 1 ? (i + 2) * nt + j / 2 :
                    (i + 2) * nt + j,
                tr = i / 2 == nz - 1 ? (i + 2) * nt + ((j + 2) / 2) % nt :
                    (i + 2) * nt + (j + 2) % (nt * 2))
            each [
                [br, bl, c],
                [bl, tl, c],
                [tl, tr, c],
                [tr, br, c]
            ]
        ]
    ];
    
    polyhedron(points, faces, 1);
}


/**
create a round pipe around a path f : [0,1] -> R2 or R3, which may be nondifferentiable and/or noncontinuous, colored with c : [0,1] -> "#rgb"

*/
module sweep_nondifferentiable(f, r = 1, c = function(t) default, $fn = 10) {
    for(i = [0:1:$fn-1]) {
        color(c(i)) hull() {
            translate(f(i / $fn)) sphere(r, $fn = $preview ? 5 : 15); 
            translate(f((i + 1) / $fn)) sphere(r, $fn = $preview ? 5 : 15);
        }
    }
}

// create a list of n+1 rotations which rotate [0,0,1] to be parallel to f(t) for t=[0:1/n:1]
function follow(f, n, acc=[]) = 
    // 0 thru n 
    len(acc) == n+1 ? acc : 
    let(t = len(acc) / n)
    acc == [] ? follow(f, n, [align(k3, unit(f(1 / n) - f(0)))]) : 
    // create the next rotation by adding to the previous one the angle difference between the last index and this one
    // gradient of f at current index
    let(v1 = unit(f(t + 1 / n) - f(t)))
    // previous result
    let(last=mat_apply(acc[len(acc)-1], k3))
    // apply the current difference to the last index to avoid discontinuities
    follow(f, n, [each acc, 
        mat_mult(align(last, v1), acc[len(acc)-1])
    ]);

/**
pass
    a parametric continuous 1-periodic function p : t ϵ [0,1] -> R2 for the cross section
    a parametric C2 function f : s ϵ [0,1] -> R3 for the sweep path
    a number of segments to divide the cross section perimeter into
    a number of segments to divide the path length into
    whether to keep the profile flat the whole way (true) or normal to the sweep path (false)
draws a solid by sweeping the face given by p along the sweep path f
*/
module sweep(p, f, nt = $preview ? 15 : 360, ns = $preview ? 10 : 100, parallel=false) {
    // segment the parameters
    dt = 360 / nt;
    ds = 1 / ns;
    
    // create a list of rotations to apply to the flat profile before translating it
    rots = parallel ? [for (i=[0:1:ns]) mat_i(3)] : follow(f, ns);
    
    // create the square net of points
    // loop over t, then s
    points = [for(i = [0:1:ns], j = [0:1:nt-1])
        // convert indices to parameter values
        let(s1 = i * ds, s2 = (i + 0.5) * ds, t1 = j * dt, t2 = (j + 0.5) * dt)
        let(p1 = p(t1), p2 = p(t2))
        let(x1 = f(s1) + mat_apply(rots[i], [p1.x, p1.y, 0]),
            x2 = f(s2) + mat_apply(rots[i], [p2.x, p2.y, 0]))
        each (i == ns ? [x1] : [x1, x2])
    ];
    
    // create the faces in the net of points
    faces = [
        // bottom face
        [for(i=[0:2:2*nt-1]) i],
        // top face
        [for(i=[0:1:nt-1]) len(points) - 1 - i],
        // other faces
        each [for(i = [0:2:2*ns-2], j = [0:2:2*nt-2])
            let(bl = i * nt + j,
                br = i * nt + (j + 2) % (nt * 2),
                c = i * nt + j + 1,
                tl = i / 2 == ns - 1 ? (i + 2) * nt + j / 2 :
                    (i + 2) * nt + j,
                tr = i / 2 == ns - 1 ? (i + 2) * nt + ((j + 2) / 2) % nt :
                    (i + 2) * nt + (j + 2) % (nt * 2))
            each [
                [br, bl, c],
                [bl, tl, c],
                [tl, tr, c],
                [tr, br, c]
            ]
        ]
    ];

//    for(i=[0:1:ns]) {
//        translate(f(i/ns)) origin(5, function (v) mat_apply(rots[i], v));
//    }
        
    polyhedron(points, faces, 1);
}

// a ruler l millimeters long, labeled in inches and centimeters
module ruler(l) {
    in = [for(x = [0:25.4:l]) x];
    halfin = [for(x = [12.7:25.4:l]) x];
    quarterin = [for(x = [6.35:12.7:l]) x];
    eighthin = [for(x = [3.175:6.35:l]) x];
    sixteenthin = [for(x = [1.5875:3.175:l]) x];
    thirtysecondin = [for(x = [0.79375:1.5875:l]) x];
    cm = [for(x = [0:10:l]) x];
    fivemm = [for(x = [5:10:l]) x];
    mm = [for(x0 = [0:5:l]) each [for(x1 = [1:1:4]) x1 + x0]];
    tick = "#542";
    number = "#555";
    w = 20;
    difference() {
        translate([0, -w/2, -2]) cube([l, w, 2]);
        for(i = [1:1:len(in)-1]) {
            translate([in[i] - 1.5 * floor(log(i) + 1), 1.4, -0.5]) color(number) linear_extrude(1) text(str(i), size=4);
        }
        for(i = [1:1:len(cm)-1]) {
            h = i % 10 == 0 ? 4 : 3;
            d = i % 10 == 0 ? 1.5 : 1.2;
            y = i % 10 == 0 ? -1.4 : -2.4;
            translate([cm[i] + d * floor(log(i) + 1), y, -0.5]) color(number) rotate([0, 0, 180]) linear_extrude(1) text(str(i), size=h);
        }
        imperial = [[in, 4], [halfin, 5], [quarterin, 4.5], [eighthin, 3.5], [sixteenthin, 2.5], [thirtysecondin, 1.5]];
        metric = [[cm, 4], [fivemm, 5], [mm, 2.5]];
        for(L = imperial) {
            for(i = [0:1:len(L[0])-1]) {
                translate([L[0][i], w/2 - L[1], 0]) 
                rotate([-90, 0, 0])
                color(tick) 
                pyramid(0.2, L[1], 4);
            }
        }
        for(L = metric) {
            for(i = [0:1:len(L[0])-1]) {
                translate([L[0][i], L[1] - w/2, 0])
                rotate([90, 0, 0])
                color(tick)
                pyramid(0.2, L[1], 4);
            }
        }
    }
}