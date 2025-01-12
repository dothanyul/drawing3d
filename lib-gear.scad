
include <lib-func.scad>

/*
herringbone gears
give a number of teeth and a thickness to get a spur gear, rack, ring gear, or conical gear
for the conical gear, tooth count and profile is determined by the circle at the center of the teeth
the conical gear additionally requires a complement size which is the tooth size of the gear that would mesh on a perpendicular axis
*/

// total period of tooth profile in mm
T = 7;
// proportion of the total period taken up by the tooth itself
t1 = 0.47;
// number of teeth to rotate through
w = 1.3;
// half the tooth depth in mm (depth to the action line)
d = 1.5;
// angle of the tooth sides
a = 20;
// thickness of the gears in mm
thick = 12;

// radius of a gear with n teeth
radius = function(n) T * n / PI / 2;

// herringbone gear for an arbitrary cross section
// thickness h, swept through s degrees
module gear(h, s, right, $fn = 3) {
    k = 4;
    linear_extrude(h / 2, center=false, twist = right ? s : -s, slices = $fn) 
        children();
    translate([0, 0, h / 2]) rotate([0, 0, right ? -s : s]) 
    linear_extrude(h / 2, center=false, twist = right ? -s : s, slices = $fn) 
        children();
}

// draw a spur gear with n teeth and thickness h
module spur(n, h = thick, right = true, w = w, d = d, a = a, $fn = 4) {
    // the two corners that lie on the action circle
    top = rot_k([radius(n), 0], 360 / n / 2 * t1);
    bot = cmult(top, [1, -1]);
    // directions of the tooth's outer sloped edges, going counterclockwise
    out = [1, tan(a)] * d;
    back = cmult(out, [-1, 1]);
    tooth = [bot,
        for(i = [0: 1/$fn: 1])
            rot_k([radius(n) + d, 0], 
                to(vec_ang(bot + out), vec_ang(top - back))(i)),
        top, 
        for(i = [0: 1/$fn: 1])
            rot_k([radius(n) - d, 0],
                to(vec_ang(top + rot_k(back, 360 / n / 2)),
                    vec_ang(bot - rot_k(out, -360 / n / 2)) + 360 / n)
                (i))];
    gear(h, w * 360 / n, right, $fn * 2) 
        polygon([for(i=[0:1:n-1])
            for(p = tooth) rot_k(p, i * 360 / n)]);
}

// draw a rack n teeth long with the first tooth centered on the origin
// dr is the distance from the action plane to the back face
module rack(n, h = thick, dr = d * 3, w = w, d = d, a = a, right = true) {
    l = [-T / 2 * t1, 0];
    r = cmult(l, [-1, 1]);
    up = d * [tan(a), 1];
    down = cmult(up, [1, -1]);
    tooth = [l - up, l, l + up, r - down, r, r + down];
    points = [for(i = [0:1:n-1]) 
            for(p = tooth) 
                p + [i * T, 0, 0],
        [T * (n - 3/4), -dr],
        [-T/4, -dr]
    ];
    module profile() polygon(points);
    k = 2;
    // vector to extrude along
    v = [(right ? -T : T) * w, 0, h/2];
    linear_extrude(norm(v), v = v, 
        center=false, slices = $fn * k) profile();
    translate([-T * w, 0, h / 2]) linear_extrude(norm(v), v = cmult(v, [-1,1,1]), 
        center=false, slices = $fn * k) profile();
}

// ring gear with n teeth, with dr between the action radius and the outer radius
// in a planetary gearset, the sun and the ring match hands, and the planets are all the opposite
module ring(n, h = thick, dr = d * 2, w = w, d = d, a = a, right = true, $fn = 4) {
    top = rot_k([radius(n), 0], 360 / n / 2 * t1);
    bot = cmult(top, [1, -1]);
    out = [1, -tan(a)] * d;
    back = cmult(out, [-1, 1]);
    // last point is on the outer circumference of the ring
    tooth = [
        bot, 
        for(i = [0: 1/$fn: 1]) rot_k([radius(n) - d, 0],
            to(vec_ang(bot - out), vec_ang(top + back))(i)),
        top,
        for(i = [0: 1/$fn: 1]) rot_k([radius(n) + d, 0],
            to(vec_ang(top - rot_k(back, 360 / n / 2)),
                vec_ang(bot + rot_k(out, -360 / n / 2)) + 360 / n)
            (i)),
        for(i = [0: 1/$fn: 1]) rot_k([radius(n) + dr, 0], i * 360 / n)];
    points = [for(i = [0: 1: n-1])
        for(x = tooth) rot_k(x, i * 360 / n)];
    paths = [[for(i = [1: 1: n]) for(j = [$fn: -1: 0]) 
            i * len(tooth) - j - 1],
        [for(i = [0: 1: n-1]) for(j = [0: 1: len(tooth) - $fn - 2]) 
            i * len(tooth) + j]];
    gear(h, w * 360 / n, !right, $fn * 2)
        polygon(points, paths);
}

// planetary gearset with 4 planets, with s teeth on the sun and p teeth on the planets
// right is the handedness of the sun and the ring
module planetary(s = 8, p = 4, np = 4, h = thick, w = w, d = d, a = a, right = true, $fn = 4) {
    r = s + p * 2;
//    rotate([0, 0, -$t * 360]) {
//        color("#FD2") 
//        rotate([0, 0, $t * 360]) 
            spur(s, h = h, w = w, d = d, a = a, right = right);
//        color("#F48") 
            for(i = [0:1:np]) rotate([0, 0, i * 360 / np]) 
            translate([radius(s) + radius(p), 0, 0]) 
            rotate([0, 0, 360 * ($t * -s / p + (p % 2 == 0 ? 1/p/2 : 0))]) 
                spur(p, h = h, w = w, d = d, a = a, right = !right);
//        color("#4BF") 
            rotate([0, 0, $t * 360 * (-s) / r]) 
                ring(r, h = h, w = w, d = d, a = a, right = right);
//    }
}

// bevel gear with n teeth that mates perpendicularly with one with m teeth
// teeth track is h wide and gear is k thick
// tooth dimensions are from the center of the track
// action circle lies in the xy plane
module bevel(n, n2, h = thick, k = thick / 3, w = w, d = d, a = 30, right = true, $fn = 4) {
    // default value in case it's not passed
    m = is_num(n2) ? n2 : n;
    // radius of the action circle
    rad = radius(n);
    // angle from the center of the tooth to the edge
    ang = 360 / n / 2 * t1;
    // angle of the conical face
    b = atan(m / n);
    // normal to the conical face in the xz plane
    up = rot_j([0, 0, d], b);
    // intersection of the axes of this gear and its mate
    org = [0, 0, radius(m)];
    
    // points on the action circle in cylindrical coordinates
    top = [rad, ang, 0];
    bot = cmult(top, [1, -1, 1]);
    // profile edge vectors in the same coordinate system
    asc = [0, atan(tan(a) * d / rad), 0] + up;
    desc = cmult(asc, [-1, 1, -1]);
    // points on the center profile to project the herringbone from
    profile = [bot, 
        for(i = [0: 1: $fn]) 
            to(bot + asc, top - desc)(i / $fn),
        top, 
        // extend the bottom a little into the cone
        top - up * 1.1, 
        bot - up * 1.1];
    
    // spiral profile of the teeth given a starting point and proportional radius
    // j = -1/2 at the inside edge of the track, j = 1/2 at the outside, j = 0 at the center
    function spiral(from, j) = 
        let(rz = cmult(from, [1, 0, 1]),
            cyl = rz + unit(rz - org) * h * j)
        rot_k(cyl, w * 360 / n * abs(j) * (right ? 1 : -1) + from[1]);
    
    // one herringbone tooth
    module tooth() {
        points = [for(j = [0: 1/$fn/2: 1]) for(x = profile) spiral(x, j - 1/2)];
        faces = [for(j = [0: 1: 2*$fn-1]) for(i = [0: 1: len(profile) - 1])
            j * len(profile) * [1, 1, 1, 1] + 
                (i == len(profile) - 1 ? [i, 0, len(profile), len(profile) + i] :
                [i, i + 1, i + 1 + len(profile), i + len(profile)]),
            [for(i = [len(profile)-1: -1: 0]) i],
            [for(i = [len(profile): -1: 1]) len(points) - i]];
        polyhedron(points, faces);
    }

    // the cone the teeth sit on
    module cone() {
        points = [for(j = [-1/2, 1/2]) let(x = spiral(top - up, j))
            [norm(raise(x, 2)), x.z]];
        x = points[1] - rot_k([0, k], -b);
        profile = [[0, x.y], [0, points[0].y], points[0], points[1], x];
        n1 = n * 4;
        rotate_extrude($fn = n * $fn * 2) polygon(profile);
    }

    union() {
        for(i = [0: 1: n-1]) rotate([0, 0, i * 360 / n])
            tooth();
        cone();
    }
}

