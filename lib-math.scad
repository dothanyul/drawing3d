include <lib-list.scad>

// functions to deal with scalars

// identity function
id = function(x) x;

// mod that deals with negatives correctly
function mod(a,b) = (a % b + b) % b;

// Euclid's algorithm
function gcd(a,b) = 
    a<b ? gcd(b,a) :
    b==0 ? a : 
    gcd(b, a%b);

// sum a list of numbers or vectors
function sum(L, acc=0) = 
    L == [] ? acc :
    is_list(L[0]) && is_num(acc) ? sum(L, [for(i=L[0]) 0]) :
    sum(cdr(L), L[0] + acc);

// find the angle of an arc given length and radius
function angle(arclength, radius) = arclength / (radius * 2 * PI) * 360;

// approximate thomae's function given max q
function thomae(x, maxq, best=[0,1], q=1) = 
    x > 1 ? thomae(x % 1, maxq, best, q) :
    x < 0 ? thomae(-x, maxq, best, q) :
    q >= maxq ? 1 / best[1] :
    // given x ϵ [0,1] and [p,q] the best approximation to x we have p <= q
    // for r > q we have
    // p/r < p/q, x < 
    let(d = function(pair) abs(x - pair[0] / pair[1]))
    d([floor(x * q), q]) < d(best) ? thomae(x, maxq, [floor(x * q), q], q+1) :
    d([ceil(x * q), q]) < d(best) ? thomae(x, maxq, [ceil(x * q), q], q+1) :
    thomae(x, maxq, best, q+1);

// functions to deal with vectors

// standard basis vectors in R3
i3 = [1, 0, 0];
j3 = [0, 1, 0];
k3 = [0, 0, 1];

// zero vector in Rn
function vec_0(n) = [for(i=[0:1:n-1]) 0];

// average of two vectors
function vec_avg(a,b) = (a + b) / 2;

// polar angle (argument) of a vector a ϵ R2
function vec_ang(a) = atan2(a.y, a.x);

// unit vector parallel to v ϵ Rn
function unit(v) = v / norm(v);

// dot product of two vectors a,b ϵ Rn
// set strict = true to fail when len(a) != len(b)
function dot(a, b, strict = false) = 
    strict && len(a) != len(b) ? undef :
    a == [] || b == [] ? 0 :
    a[0] * b[0] + dot(cdr(a), cdr(b));

// cross product of two vectors a,b ϵ R3
function cross(a, b) = [a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x];

// rotate a in xy plane by t
function rot_k(t, a) = [a.x * cos(t) - a.y * sin(t), a.y * cos(t) + a.x * sin(t), a.z];
// rotate a in xz plane by t
function rot_j(t, a) = [a.x * cos(t) - a.z * sin(t), a.y, a.x * sin(t) + a.z * cos(t)];
// rotate a in yz plane by t
function rot_i(t, a) = [a.x, a.y * cos(t) - a.z * sin(t), a.y * sin(t) + a.z * cos(t)];

// distance between two vectors a,b ϵ Rd
function distance(a,b) = sqrt(distsq(a, b));

// distance squared between two vectors in Rd
function distsq(a, b) =
    is_num(a) ? pow(a - b, 2) : 
    len(a) == 0 ? 0 :
    pow(a[0] - b[0], 2) + distsq(cdr(a), cdr(b));

// distance between polar points a,b ϵ R x [0,360)
function dist_polar(a, b) = 
    let(
        c = [a[0] * cos(a[1]), a[0] * sin(a[1])],
        d = [b[0] * cos(b[1]), b[0] * sin(b[1])])
    distance(c,d);

// compare the distance between two vectors a,b ϵ Rd
function dist_comp(a,b) = (is_undef(a) || is_undef(b)) ? 0 : distance(a[0], a[1]) - distance(b[0], b[1]);

// calculate the length of a parametric function f : [0:360] -> R2 with resolution dt
function length(f, dt) = 
    sum([for(t=[0:360:dt]) 
        let(t0 = t, t1 = t + dt)
        distance(f(t0), f(t1))
    ]);

/**
align vector whither to vector thither, preserving relative angles and distances
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
    norm(vw + vt) < norm(vw - vt) ? mat_mult(align(unit(vw) + unit(vt), vt), align(vw, unit(vw) + unit(vt))) :
    // degenerate case - parallel
    norm(vw + vt) == norm(vw) + norm(vt) ? mat_i(3) :
    // angle between is greater than 0 and less than 90
    // define an orthonormal basis with unit(vw) = i and vt ϵ <i,j>
    let(i = unit(vw), k = unit(cross(vw, vt)))
    let(j = unit(cross(k, i)))
    // sine and cosine of the angle to rotate by defined by the projection of unit(vt) onto j and i respectively
    let(sint = dot(j, unit(vt)), cost = dot(i, unit(vt)))
    // matrix to convert to the standard basis from the new orthonormal basis
    let(to = [[i.x, j.x, k.x], [i.y, j.y, k.y], [i.z, j.z, k.z]])
    // convert to B, rotate by t, convert to SB
    mat_prod([to, [[cost, -sint, 0], [sint, cost, 0], [0, 0, 1]], mat_inv(to)]);

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

// determinant of a 3x3 matrix
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

// integral of f : R => R from a to b
function integral(f, a, b) = (
    let(t = function(i) a + (i / $fn) * (b - a))
    sum([for(i = [1:1:$fn])
        1 / $fn * (f(t(i)) + f(t(i-1))) / 2
    ])
);

// path integral of f : R => Rn from a to b
function path_integral(f, a, b) = (
    let(t = function(i) a + (i / $fn) * (b - a))
    sum([for(i = [1:1:$fn])
        norm([1 / $fn, each f(t(i)) - f(t(i-1))])
    ])
);











