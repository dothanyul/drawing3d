include <lib-math.scad>

// functions related to drawing things

// polar function for the radius of an n-gon with radius 1
function poly(n) = function(t) 
    (cos(180 / n) / cos(t % (360 / n) - 180 / n));
    
// draw a polygon given a parametric function p
module face(p, $fn=40) {
    dt = 360 / $fn;
    polygon([for(i=[0:1:$fn-1])
        let(t = dt * i)
        [p(t) * cos(t), p(t) * sin(t)]
    ]);
}

/*
Parametric function R -> R3 for a line segment from a to b
*/
function segment(a, b) = function(t)
    a + (b - a) * t;

/*
Draw positive x,y,z axes of length size transformed by function f
*/
module origin(size=1, f=id) {
    color("#FF8844") sphere(1);
    for (x=[i3, j3, k3]) {
        translate(f(x * size)) sphere(1);
    }
    % hull() {
        sphere(1);
        for (x=[i3, j3, k3]) {
            translate(f(x * size)) sphere(1);
        }
    }
}

/*
Draw around the edges of a % cube with dimensions [width, depth, height]
*/


/**
pass two parametric functions p1, p2 : t ϵ [0:360) => [x(t), y(t), z(t)] ϵ R3
returns the slope [dx, dy] : t ϵ [0:360) => [dx(t), dy(t)] ϵ R2
where dx is the slope of the image vector from [x1(t), z1(t)] to [x2(t), z2(t)]
and likewise for dy.
*/
function direct(p1,p2) = function(t) [(p2(t).x - p1(t).x) / (p2(t).z - p1(t).z), (p2(t).y - p1(t).y) / (p2(t).z - p1(t).z)];

/**
Make two cubic splines, one in the x plane and one in the y plane, and project them together
calculate a cubic function f : [0,1] -> R2 such that
    f(x1.z) = [x1.x, x1.y]
    f(x2.z) = [x2.x, x2.y]
    df/dz(x1.z) = d1 in x and y
    df/dz(x2.z) = d2 in x and y
    f lies in the plane (r, z) => [(x2.x - x1.x)r + x1.x, (x2.y - x1.y)r + x1.y, z]
*/
function spline(x1, x2, d1, d2) =
    let(z1 = x1.z, h = x2.z - x1.z)
    let(a = function(t1,t2,d1,d2)
            (d1 + d2) / pow(h,2)
            - 2 * (t2 - t1) / pow(h,3),
        b = function(t1,t2,d1,d2)
            -(2 * d1 + d2) / h
            + 3 * (t2 - t1 - z1 * (d1 + d2)) / pow(h,2)
            + 6 * z1 * (t2 - t1) / pow(h,3),
        c = function(t1,t2,d1,d2)
            d1
            + 2 * z1 * (2 * d1 + d2) / h
            + 3 * z1 * (z1 * (d1 + d2) - 2 * (t2 - t1)) / pow(h,2)
            - 6 * pow(z1,2) * (t2 - t1) / pow(h,3),
        d = function(t1,t2,d1,d2)
            t1 - d1 * z1
            - pow(z1,2) * (2 * d1 + d2) / h
            + pow(z1,2) * (3 * (t2 - t1) - z1 * (d1 + d2)) / pow(h,2)
            + 2 * pow(z1,3) * (t2 - t1) / pow(h,3))
    function (z)
        let(rx = a(x1.x, x2.x, d1.x, d2.x) * pow(z,3) + b(x1.x, x2.x, d1.x, d2.x) * pow(z,2) + c(x1.x, x2.x, d1.x, d2.x) * z + d(x1.x, x2.x, d1.x, d2.x),
            ry = a(x1.y, x2.y, d1.y, d2.y) * pow(z,3) + b(x1.y, x2.y, d1.y, d2.y) * pow(z,2) + c(x1.y, x2.y, d1.y, d2.y) * z + d(x1.y, x2.y, d1.y, d2.y))
        [rx, ry, z];

/**
pass
    two parametric functions r1,r2 : tϵ[0,360) -> R2 x [[0,0,h]]] for top and bottom faces
    two parametric functions d1,d2 : tϵ[0,360) -> R for the slopes at the ends
    a number $fn of intervals to divide t into
this function defines a curved face in R3 between these two curves thus:
    construct a parametric spline s : [t,z] -> r in the plane P = [r cos t, r sin t, z] where
        angle t ranges from 0 to 360 in degrees
        height z ranges from 0 to h
        radius r is positive
    such that for all t,
        s(t,0) = r1(t)
        s(t,h) = r2(t)
        ∂s/∂z(t,0) = tan(ends[0])
        ∂s/∂z(t,h) = -tan(ends[1])
        if there is an inflection point, the slope there is minimized
    four control points so for a fixed t, s(z) = (r2(t) - r1(t)) * (3(x/h)^2 - 2(x/h)^3) + r1(t)
break t into intervals of width i = 360/$fn
calculate the path length L of the larger of r1 and r2 and break h into equal-length intervals of width roughly L / $fn (j intervals where j = ceil(L / $fn))
sample the surface of s at this density to create a square grid of points over it
pass these points and squares (as well as the top and bottom faces) to polyhedron()
*/
module loft(p1, p2, d1, d2, nt = $preview ? 20 : 360, nz = $preview ? 10 : 0) {
    // segment t : [0,360)
    dt = 360 / nt;

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

// create a list of n+1 rotations which rotate [0,0,1] to be parallel to f(t) for t=[0:1/n:1]
function follow(f, n, acc) = 
    // 0 thru n 
    len(acc) == n+1 ? acc : 
    let(t = len(acc) / n)
    acc == [] ? follow(f, n, [align(k3, unit(f(1 / n / 10) - f(0)))]) : 
    // create the next rotation by adding to the previous one the angle difference between the last index and this one
    // gradient of f at current index
    let(v1 = unit(f(t + 1 / n / 20) - f(t - 1 / n / 20)))
    // previous result
    let(last=mat_apply(acc[len(acc)-1], k3))
    // apply the current difference to the last index to avoid discontinuities
    follow(f, n, [each acc, 
        mat_mult(align(last, v1), acc[len(acc)-1])
    ]);

/**
pass
    a parametric function p : t ϵ [0,360) -> R2 for the cross section
    a parametric function f : s ϵ [0,1] -> R3 for the sweep path
    a number of segments to divide the cross section perimeter into
    a number of segments to divide the path length into
    whether to keep the profile flat the whole way (true) or normal to the sweep path (false)
draws a solid by sweeping the face given by p along the sweep path f
*/
module sweep(p, f, nt = $preview ? 15 : 360, ns =  $preview ? 10 : 100, parallel=false) {
    // segment the parameters
    dt = 360 / nt;
    ds = 1 / ns;
    
    // create a list of rotations to apply to the flat profile before translating it
    rots = parallel ? [for (i=[0:1:ns]) mat_i(3)] : follow(f, ns, []);
    
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

function drop(t) =
    let(r = 10)
    t < 0 ? drop(t+1) :
    t < 0.25 ? [cos(360 * 2 * t) + 1, 0, -sin(360 * 2 * t)] * r :
    t < 0.5 ? [cos(360 * (2 * t - 0.5)) - 1, 0, -sin(360 * 2 * t)] * r :
    t <= 1.5 ? [2 * cos(360 * t), 0, 2 * sin(360 * t)] * r :
    drop(t-1);





