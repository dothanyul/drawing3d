// various math functions
include <lib-list.scad>

// identity function
id = function(x) x;

// functions to deal with scalars

function is_finite(x) = is_num(x) && x < 1/0 && x > -1/0;

// compare two numbers
comp = function(a, b)
    is_list(a) && is_list(b) ? x_comp(a, b) :
    is_list(a) ? comp(norm(a), b) :
    is_list(b) ? comp(a, norm(b)) :
    a < b ? -1 :
    a > b ? 1 :
    0;

// mod that deals with negatives correctly
function mod(a,b) = (a % b + b) % b;

// Euclid's algorithm
function gcd(a,b) = 
    a<b ? gcd(b,a) :
    b==0 ? a : 
    gcd(b, a%b);

// list of prime numbers up to n 
function primes(n, acc=[], i=0) =
    n < 2 ? [] :
    acc == [] ? primes(n, [for(i=[2:1:n]) i], i) :
    acc[i] > sqrt(n) ? acc :
    primes(n, filter(acc, function(a) a <= acc[i] || a % acc[i] != 0), i+1);

// list of prime factors of n (with repeats)
function factors(n, primes=[], acc=[]) = 
    !is_num(n) || n < 2 ? [] :
    primes == [] ? factors(n, primes(n), acc) :
    contains(primes, [n]) ? [each acc, n] :
    n % primes[0] == 0 ? factors(n / primes[0], primes, [each acc, primes[0]]) :
    factors(n, cdr(primes), acc);

// truncate float or vector a after p bits
// for vectors, truncate each component
function truncate(a, p) = 
    is_list(a) ? [for(b = a) truncate(b, p)] :
    a < 0 ? -truncate(-a, p) :
    a == 0 ? 0 :
    p <= 0 ? a :
    let(b = pow(2, max(floor(log(a) / log(2)), 0) - p + 1))
    floor(a / b) * b;

// find the angle of an arc given length and radius
function angle(arclength, radius) = arclength / (radius * 2 * PI) * 360;

// map x ϵ R to how far it is between the nearest powers of b
// that is, give the ratio of the lower digits of x to the MSD of x base b
function logrest(x, b) = 
    let(xb = log(x) / log(b))
    let(x0 = pow(b, floor(xb)), x1 = pow(b, floor(xb)+1))
    (x - x0) / (x1 - x0);

// convert a rational number t ϵ [0,1] given as [p,q] to an n-length list of digits in base b
function base(t, b, n=5, acc=[]) = 
    len(acc) == n ? acc :
    base([t[0] * b % t[1], t[1]], b, n, [each acc, floor(t[0] * b / t[1])]);

// approximate thomae's function given max q
function thomae(x, maxq, best=[0,1], q=1) = 
    x > 1 ? thomae(x % 1, maxq, best, q) :
    x < 0 ? thomae(-x, maxq, best, q) :
    q >= maxq ? 1 / best[1] :
    // given x ϵ [0,1] and [p,q] the best approximation to x we have p <= q
    // for r > q we have p/r < p/q
    let(d = function(pair) abs(x - pair[0] / pair[1]))
    d([floor(x * q), q]) < d(best) ? thomae(x, maxq, [floor(x * q), q], q+1) :
    d([ceil(x * q), q]) < d(best) ? thomae(x, maxq, [ceil(x * q), q], q+1) :
    thomae(x, maxq, best, q+1);

// bitwise math on two numbers
function bitwise(a, b, op, c=[]) = 
    a == 0 && b == 0 ? sum([for(i=[0:1:len(c)]) c[i] ? pow(2, i) : 0]) :
    bitwise(floor(a / 2), floor(b / 2), op, c=[each c, op((a % 2 == 1), (b % 2 == 1))]);
function and(a, b) = bitwise(a, b, function(a, b) a && b);
function or(a, b) = bitwise(a, b, function(a, b) a || b);
function xor(a, b) = bitwise(a, b, function(a, b) a != b);
function nand(a, b) = bitwise(a, b, function(a, b) !(a && b));
function nor(a, b) = bitwise(a, b, function(a, b) !(a || b));

// functions to deal with vectors

// standard basis vectors in R3
i3 = [1, 0, 0];
j3 = [0, 1, 0];
k3 = [0, 0, 1];

// zero vector in Rn
function v0(n) = [for(i=[0:1:n-1]) 0];

// increase dimension of a vector by adding zeroes, or decrease dimension by discarding the extra components
function raise(v, n) = [for(i=[0:1:n-1]) is_undef(v[i]) ? 0 : v[i]];

// polar angle (argument) of a vector a ϵ R2
function vec_ang(a) = atan2(a.y, a.x);

// latitude of a vector v ϵ R3
function lat(v) = atan2(v.z, norm([v.x, v.y]));

// longitude of a vector v ϵ R2
function long(v) = atan2(v.y, v.x);

// unit vector parallel to v ϵ Rn
function unit(v) = v / norm(v);

// dot product of two vectors a,b ϵ Rn
// set strict = true to fail when len(a) != len(b)
function dot(a, b, strict = false) = 
    strict && len(a) != len(b) ? undef :
    a == [] || b == [] ? 0 :
    a[0] * b[0] + dot(cdr(a), cdr(b));

// rotate a in xy plane by t
function rot_k(a, t) = let(b = [a.x * cos(t) - a.y * sin(t), a.y * cos(t) + a.x * sin(t), a.z])
    is_undef(a.z) ? [b.x, b.y] : b;
// rotate a in zx plane by t
function rot_j(a, t) = [a.z * sin(t) + a.x * cos(t), a.y, a.z * cos(t) - a.x * sin(t)];
// rotate a in yz plane by t
function rot_i(a, t) = [a.x, a.y * cos(t) - a.z * sin(t), a.y * sin(t) + a.z * cos(t)];

// multiply two vectors componentwise
// if they aren't the same length, will pad the shorter one out with zeroes first
function cmult(a, b) = 
    let(d = max(len(a), len(b)),
        a1 = raise(a, d), b1 = raise(b, d))
    [for(i=[0:1:d-1]) a1[i] * b1[i]];

// distance between two vectors a,b ϵ R or Rd
function distance(a,b) = 
    is_num(a) && is_num(b) ? abs(a - b) :
    norm(a - b);

// distance squared between two vectors in Rd
function distsq(a, b) =
    is_num(a) ? pow(a - b, 2) : 
    len(a) == 0 || len(b) == 0 ? 0 :
    pow(a[0] - b[0], 2) + distsq(cdr(a), cdr(b));

// distance between polar points a,b ϵ R x [0,360)
function dist_polar(a, b) = 
    let(c = [a[0] * cos(a[1]), a[0] * sin(a[1])],
        d = [b[0] * cos(b[1]), b[0] * sin(b[1])])
    distance(c,d);

// compare the distance between two pairs of vectors a,b ϵ [Rd, Rd]
function dist_comp(a,b) = (is_undef(a) || is_undef(b)) ? 0 : distance(a[0], a[1]) - distance(b[0], b[1]);

/**
pass two parametric functions p1, p2 : t ϵ R => [x(t), y(t), z(t)] ϵ R3
returns the slope [dx, dy] : t ϵ [0:360) => [dx(t), dy(t)] ϵ R2
where dx is the slope of the image vector from [x1(t), z1(t)] to [x2(t), z2(t)]
and likewise for dy.
*/
function direct(p1,p2) = function(t) [(p2(t).x - p1(t).x) / (p2(t).z - p1(t).z), (p2(t).y - p1(t).y) / (p2(t).z - p1(t).z)];

/**
align vector whence to vector thither, preserving relative angles and distances
pass two vectors vw,vt ϵ R3 
returns a rotation matrix M ϵ R3xR3 which rotates R3 continuously to align vw to vt
construct an orthonormal basis B on R3 such that vw || i|B and vt ϵ <i|B, j|B>
where i|B = unit(vw), k|B = unit(vw x vt), and j|B = unit(k|B x i|B)
to rotate vw to vt, rotate by t = atan2(vt . j|B, vt . i|B) in B
convert R3 to B, apply R=[[cos t, -sin t, 0], [sin t, cos t, 0], [0, 0, 1]], convert B to R3
v = v.x*i3 + v.y*j3 + v.z*k3 = v⋅B1*B1 + v⋅B2*B2 + v⋅B3*B3 so Bfrom = [B1, B2, B3] and Bto = inv(Bfrom)

let v ϵ R3 and a, b, c ϵ R3 and define u = v.x * a + v.y * b + v.z * c
then u.a = v.x * a.x + v.y * b.x + v.z * c.x
so the matrix A such that u|R3 = Av|[a,b,c] will be defined by
[a.x b.x c.x]
[a.y b.y c.y]
[a.z b.z c.z]
therefore the operation "convert to B, rotate about k3 by t, convert to the standard basis" is defined by
[a.x b.x c.x]
[a.y b.y c.y]       convert to standard basis
[a.z b.z c.z]

[cos(t) -sin(t) 0]
[sin(t)  cos(t) 0]  rotate by t
[  0       0    1]

[a.x b.x c.x] ^1
[a.y b.y c.y]       convert to B
[a.z b.z c.z]
*/
function align(vw, vt) = 
    // degenerate-ish case - >90 degrees between v0 and v1
    norm(vw + vt) < norm(vw - vt) ? 
        mat_mult(align(unit(vw) + unit(vt), vt), align(vw, unit(vw) + unit(vt))) :
    // degenerate case - parallel (not antiparallel from above case)
    norm(cross(vw, vt)) == 0 ? mat_i(3) :
    // angle between is greater than 0 and less than 90
    // define an orthonormal basis with unit(vw) = i and vt ϵ <i,j>
    let(i = unit(vw), k = unit(cross(vw, vt)))
    let(j = unit(cross(k, i)))
    // sine and cosine of the angle to rotate by, defined by the projection of unit(vt) onto j and i respectively
    let(sint = dot(j, unit(vt)), cost = dot(i, unit(vt)))
    // matrix to convert to the standard basis from the new orthonormal basis
    let(to = [[i.x, j.x, k.x], [i.y, j.y, k.y], [i.z, j.z, k.z]])
    // convert to B, rotate by t, convert to SB
    mat_prod([to, [[cost, -sint, 0], [sint, cost, 0], [0, 0, 1]], mat_inv(to)]);

// rotate k parallel to v
module face(v) {
    rotate([0, 90 - lat(v), long(v)]) children();
}

// comparators for x, y, and z coordinates of vectors a,b ϵ R3
x_comp = function (a,b)
    is_undef(a) || is_undef(b) ? 0 :
    a.x > b.x ? 1 :
    a.x < b.x ? -1 :
    a.y > b.y ? 1 :
    a.y < b.y ? -1 :
    a.z > b.z ? 1 :
    a.z < b.z ? -1 :
    0;
y_comp = function (a,b)
    is_undef(a) || is_undef(b) ? 0 :
    a.y > b.y ? 1 :
    a.y < b.y ? -1 :
    a.x > b.x ? 1 :
    a.x < b.x ? -1 :
    a.z > b.z ? 1 :
    a.z < b.z ? -1 :
    0;
z_comp = function (a,b)
    is_undef(a) || is_undef(b) ? 0 :
    a.z > b.z ? 1 :
    a.z < b.z ? -1 :
    a.x > b.x ? 1 :
    a.x < b.x ? -1 :
    a.y > b.y ? 1 :
    a.y < b.y ? -1 :
    0;

// create a list of every unique sum of vectors in a list
function allsum(vecs) = 
    len(vecs) <= 1 ? vecs :
    let(rest=allsum(cdr(vecs)))
    merge(
        merge(vecs, rest), 
        merge([for(v=rest) vecs[0] + v], [for(v=cdr(vecs)) vecs[0] + v])
    );

// functions on matrices

function is_matrix(a) = 
    land([for(el = a) is_list(el) && 
        land([for(em = el) is_num(em)])]) && 
    land([for(i = [1:1:len(a)-1]) len(a[i]) == len(a[i-1])]);

// set the value at row i, column j in matrix to val
function mat_insert(matrix, i,j, val) = [for(a=[0:1:len(matrix)-1]) a==i ? [for(b=[0:1:len(matrix[i])-1]) b==j ? val : matrix[i][b]] : matrix[a]];

// nxn identity matrix
function mat_i(n) = [for(i=[1:1:n]) [for(j=[1:1:n]) i == j ? 1 : 0]];

// multiply two matrices
// a[i][j] where i is row (up/down), j is column (right/left)
// len(a[0]) == len(b) so width of a == height of b, and len(ab)=len(a) and len(ab[0]) = len(b[0])
function mat_mult(a, b) = 
    [for(i = [0:1:len(a)-1])
        [for(j = [0:1:len(b[0])-1])
            sum([for(k = [0:1:len(a[0])-1]) 
                a[i][k] * b[k][j]
            ])
        ]
    ];

// matrix product of multiple matrices
function mat_prod(mats) = 
    len(mats) == 1 ? mats[0] :
    mat_mult(mats[0], mat_prod(cdr(mats)));

// apply a linear map (matrix) to a vector in R3
function mat_apply(A, x) = 
    let(v = mat_mult(A, [[x.x], [x.y], [x.z]]))
    [v[0][0], v[1][0], v[2][0]];

// determinant of a 3x3 matrix (formula stolen from Wolfram Alpha)
function det(A) = 
    let(a = A[0][0], b = A[0][1], c = A[0][2], 
        d = A[1][0], e = A[1][1], f = A[1][2],
        g = A[2][0], h = A[2][1], i = A[2][2])
    a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g;

// matrix inverse of a 3x3 matrix (formula stolen from Wolfram Alpha)
function mat_inv(A) = 
    let(a = A[0][0], b = A[0][1], c = A[0][2], 
        d = A[1][0], e = A[1][1], f = A[1][2],
        g = A[2][0], h = A[2][1], i = A[2][2])
    det(A) == 0 ? undef : 
    [[e * i - f * h, c * h - b * i, b * f - c * e],
    [f * g - d * i, a * i - c * g, c * d - a * f],
    [d * h - e * g, b * g - a * h, a * e - b * d]]
    / det(A);
    

// functions on functions

// length of a parametric function f : [0,1] -> Rn
function length(f) = 
    sum([for(t=[0:1:$fn-1]) 
        let(t0 = t / $fn, t1 = (t + 1) / $fn)
        distance(f(t0), f(t1))
    ]);

// average distance between two parametric functions f,g : [0,1] -> Rn
function avg_dist(f, g) = 
    avg([for(t = [0:1/$fn:1]) distance(f(t), g(t))]);

// integral of f : R => R from a to b
function integral(f, a, b) = 
    let(t = function(i) a + (i / $fn) * (b - a))
    sum([for(i = [1:1:$fn])
        1 / $fn * (f(t(i)) + f(t(i-1))) / 2
    ]);

// normal function n : R2 -> R3 to a parametric function f : R2 -> R3
// that is, given a parametric function for a surface in R3, return a function that takes the same parameter [t,s] and returns the unit vector normal to f at f([t,s])
// partial derivatives are approximated by ∂t = ∂s = d
function normal(f, d) = function(x)
    let(dt = f(x + [d/2, 0]) - f(x - [d/2, 0]),
        ds = f(x + [0, d/2]) - f(x - [d/2, 0])
    )
    !(norm(dt) > 0 && norm(ds) > 0) || unit(dt) == unit(ds) ? k3 :
    unit(cross(dt, ds));






