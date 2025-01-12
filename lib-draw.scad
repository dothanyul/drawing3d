include <lib-color.scad>
include <lib-func.scad>

// functions related to drawing things

// polar function for the radius of an n-gon with radius 1
function poly(n) = function(t) 
    (cos(180 / n) / cos((t * 360) % (360 / n) - 180 / n));
    
// draw a polygon given a parametric continuous 1-periodic function p : [0,1] -> R2
module fill(p, $fn=40) {
    dt = 1 / $fn;
    polygon([for(i=[0:1:$fn-1]) p(dt * i)]);
}

// draw a low-poly solid between distinct points x1 and x2, with max thickness wide * |x1 - x2|
module line(x1, x2, wide = 0.25) {
    c = (x1 + x2) / 2;
    dx = norm(x2 - x1);
    dir = unit(x2 - x1);
    drx = cross(dir, 
        norm(raise(dir, 2)) == 0 ? i3 : k3);
    dry = cross(drx, dir);
    polyhedron(
        [x1, x2, each [for(d = [drx, dry, -drx, -dry]) c + d * dx * wide]],
        [[0, 2, 3], [0, 3, 4], [0, 4, 5], [0, 5, 2], 
        [1, 2, 5], [1, 3, 2], [1, 4, 3], [1, 5, 4]], 
        1);
}

// draw a tetrahedron the right way out for any four points
module tetra(points) {
    if(distinct(points)) {
        a = points[1] - points[0];
        b = points[2] - points[0];
        c = points[3] - points[0];
        polyhedron(points, 
            dot(c, cross(a, b)) > 0 ? 
                [[0, 1, 2], [0, 2, 3], [0, 3, 1], [1, 3, 2]] :
                [[0, 1, 3], [0, 2, 1], [0, 3, 2], [1, 2, 3]],
            1);
    }
    else echo(str("Planar tetrahedron: ", points));
}

/*
Draw positive x,y,z axes of length size transformed by function or matrix f
*/
module origin(size=1, f=id, ball=false) {
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
        for(b = [[i3, -j3, -k3], [j3, -k3, -i3], [k3, -i3, -j3]]) {
            p = [for(x = [b[0], b[1]/4, b[2]/4, v0(3)]) 
                f1(x)];
            r = [[0, 1, 3], [0, 2, 1], [0, 3, 2], [1, 2, 3]];
            color(color_str(b[0])) polyhedron(p, r, 1);
        }
    }
}

/*
Draw square edges of thickness r around a cube with dimensions [width, depth, height]
*/
module frame(dimensions, r=1, center=false) {
    let(i = is_list(dimensions) ? dimensions[0] : dimensions,
        j = is_list(dimensions) ? dimensions[1] : dimensions, 
        k = is_list(dimensions) ? dimensions[2] : dimensions)
    difference() {
        translate([-r, -r, -r] * (center ? 0 : 1)) cube([i + r * 2, j + r * 2, k + r * 2], center=center);
        for(v = [i3, j3, k3]) {
            translate(-v * r * 2 * (center ? 0 : 1)) cube([i, j, k] + v * r * 4, center=center);
        }
    }
}

/*
Black background for something that fits in a cube([s, s, s])
*/
module black(s) {
    translate([-2*s, -2*s, -3]) color("#000") cube([s*5, s*5, 1]);
}

/*
Regular pyramid with base radius r, height h, and n sides
*/
module pyramid(r, h, n) {
    n1 = floor(n);
    points = [[0, 0, h], 
        each [for(t=[0:360/n1:359.9])
            r * [cos(t), sin(t), 0]
        ]
    ];
    faces = [[for(i=[1:1:n1]) i], 
        each [for(i=[1:1:n1])
            i == n1 ? [0, 1, n1] : [0, i+1, i]
        ]
    ];
    polyhedron(points, faces, 1);
}

/*
Cylinder sector with radius r, height h, angle a
*/
module sector(r, h, a, $fn=4) {
    points = [[0, 0, 0], [0, 0, h], 
        each [for(h1=[0, h]) for(t = [0:1:$fn]) let(a1 = a * t / $fn)
            r * [cos(a1), sin(a1), 0] + [0, 0, h1]]];
    faces = [[0, 1, 3 + $fn, 2], [0, 2 + $fn, 3 + 2 * $fn, 1], 
        [0, each [for(t = [0:1:$fn]) 2 + $fn - t]],
        [1, each [for(t = [0:1:$fn]) 3 + 2 * $fn - t]],
        each [for(t = [0:1:$fn-1]) t * [1, 1, 1, 1] + [2, 3 + $fn, 4 + $fn, 3]]];
    polyhedron(points, faces, 1);
}

/**
Randomly populate a space with a color function f : [0,1]^3 -> rgb
*/
module colorspace(f, s, d=1000) {
    for(i=[0:1:d]) {
        v = rands(0, 1, 3);
        translate(v * (is_list(s) ? s[i] : s)) rotate($vpr) color(f(v)) cylinder(1, 1, 1, center=true);
    }
}

/**
pass
    two parametric functions p1,p2 : tϵ[0,1] -> R2 x [[0,0,h]] for top and bottom faces
        must be simple closed curves
    two parametric functions d1,d2 : tϵ[0,1] -> R for the slopes at the ends
        in the plane <k3, p2(t) - p1(t)> with k3 horizontal, so 0 = vertical
this function defines a curved face in R3 between these two curves thus:
construct a parametric spline s(t) : [0,1] => R3 per spline:lib-func
break t into intervals of width i = 1/nt
calculate L = average of path lengths of r1 and r2 and break h into equal-length intervals of width roughly L / nt (number of intervals is rounded up)
sample the surface of s at this density to create a square grid of points over it
pass these points and squares (as well as the top and bottom faces) to polyhedron()
*/
module loft(p1, p2, d1 = f0(2), d2 = f0(2), nt = $preview ? 20 : 360, nz = $preview ? 10 : 0) {
    // step length for t and z : [0,1]
    dt = 1 / nt;
    nz1 = nz == 0 ? 
        let(l1 = length(p1, $fn=nt), l2 = length(p2, $fn=nt),
            y = avg_dist(p1, p2, $fn=nt))
        // average of the step lengths on the two faces
        let(a = avg(l1 / nt, l2 / nt))
        // number of steps you take of that length on an average walk from p1 to p2
        ceil(y / a)
        : nz;
    dz = 1 / nz1;
    h = p2(0).z - p1(0).z;

    // create the square net of points
    // loop over t (longitude) then z (latitude)
    points = [for(i = [0:1:nz1]) for(j = [0:1:nt-1])
        // [t1, z1] and [t2, z2] are the cylindrical coordinates of the bottom left corner and center of the square, respectively
        let(z1 = i * dz, z2 = (i + 0.5) * dz, t1 = j * dt, t2 = (j + 0.5) * dt)
        // xi and ci are the endpoints and tangent vectors for the spline at ti
        let(x1 = [p1(t1), p2(t1)], c1 = [for(d = [d1(t1), d2(t1)]) [d.x, d.y, is_undef(d.z) ? 1 : d.z]],
            x2 = [p1(t2), p2(t2)], c2 = [for(d = [d1(t2), d2(t2)]) [d.x, d.y, is_undef(d.z) ? 1 : d.z]])
        // take both splines at each latitude
        let(s1 = spline(x1, c1)(z1), s2 = spline(x2, c2)(z2))
        each (i == nz1 ? [s1] : [s1, s2])
    ];
    
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
    
    if(false) for(x = points) translate(x) color(randcolor()) sphere(1);
    else polyhedron(points, faces, 1);
}


/**
create a round pipe around a path f : [0,1] -> R2 or R3, which may be nondifferentiable and/or noncontinuous, colored with c : [0,1] -> "#rgb"

*/
module sweep_nondifferentiable(f, r = 1, c = function(t) default, n=30, $fn=5) {
    for(i = [0:1:n-1]) {
        d = raise(f((i + 1) / n) - f(i / n), 3);
        x = avg([f((i + 1) / n), f(i / n)]);
        translate(x) face(d) rotate([0, 0, 45]) color(c(i)) cylinder(norm(d), r, r, center=true);
        translate(f(i / n)) face(d) sphere(r);
    }
    translate(f(1)) face(raise(f(1) - f(1 - 1/n), 3)) sphere(r);
}

// create a list of n+1 rotations which rotate [0,0,1] to be parallel to f'(t) for t=[0:1/n:1]
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
    dt = 1 / nt;
    ds = 1 / ns;
    
    // create a list of rotations to apply to the flat profile before translating it
    rots = follow(f, ns * 2);
    
    // create the square net of points
    // loop over t, then s
    points = [for(i = [0:1:ns], j = [0:1:nt-1])
        // convert indices to parameter values
        let(s1 = i * ds, s2 = (i + 0.5) * ds, t1 = j * dt, t2 = (j + 0.5) * dt)
        let(p1 = p(t1), p2 = p(t2))
        let(x1 = f(s1) + (parallel ? raise(p1, 3) :
            mat_apply(rots[i * 2], raise(p1, 3))))
        if(i == ns) x1 else 
        let(x2 = f(s2) + (parallel ? raise(p2, 3) :
            mat_apply(rots[i * 2 + 1], raise(p2, 3))))
        each [x1, x2]
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

    if(false) for(f = faces) {
        g = [for(p = f) points[p]];
        m = avg(g);
        color(randcolor()) union() for(p = g) hull() {
            translate(p) sphere(1);
            translate(m) sphere(1);
        }
    } else if(false) {
        for(i = [0:1:len(points)-1]) color(rainbow(i / len(points)))
            translate(points[i]) sphere(1);
    }
    else polyhedron(points, faces, ns);
}

// a ruler l millimeters long, labeled in inches and/or centimeters
// set inch=false to put the centimeters side up
// set both=true to render both sides
// set feet=true to emphasize every 12th inch instead of every 10th
module ruler(l, inch=true, both=false, feet=false) {
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
    w = 12;
    // font size, horizontal adjustment per digit, vertical position
    // b is the interval for emphasis
    hdy = function(i, b=10) i % b == 0 ? [4, 1.5, -9] : [3, 1.2, -8];
    difference() {
        translate([0, -w, -2]) cube([l, w, 2]);
        // inch labels
        if(inch || both) for(i = [1:1:len(in)-1]) {
            v = hdy(i, b = feet ? 12 : 10);
            h = v[0]; d = v[1]; y = v[2];
            translate([in[i] - (inch ? d : -d) * floor(log(i) + 1), y, inch ? -0.5 : -1.5]) 
            color(number) rotate([0, inch ? 0 : 180, 0]) linear_extrude(1)
            text(str(i), size=h);
        }
        // cm labels
        if(!inch || both) for(i = [1:1:len(cm)-1]) {
            v = hdy(i);
            h = v[0]; d = v[1]; y = v[2];
            translate([cm[i] - (!inch ? d : -d) * floor(log(i) + 1), y, !inch ? -0.5 : -1.5])
            color(number) rotate([0, !inch ? 0 : 180, 0]) linear_extrude(1)
            text(str(i), size=h);
        }
        imperial = [[in, 4, 0.5], [halfin, 5, 0.2], [quarterin, 4.5, 0.2], [eighthin, 3.5, 0.2], [sixteenthin, 2.5, 0.2], [thirtysecondin, 1.5, 0.2]];
        metric = [[cm, 4, 0.4], [fivemm, 5, 0.2], [mm, 2.5, 0.2]];
        if(inch || both) for(L = imperial) {
            for(i = [0:1:len(L[0])-1]) {
                translate([L[0][i], -L[1], !inch ? -2 : 0]) 
                rotate([-90, 0, 0])
                color(tick) 
                pyramid(L[2], L[1], 4);
            }
        }
        if(!inch || both) for(L = metric) {
            for(i = [0:1:len(L[0])-1]) {
                translate([L[0][i], -L[1], inch ? -2 : 0])
                rotate([-90, 0, 0])
                color(tick)
                pyramid(L[2], L[1], 4);
            }
        }
    }
}