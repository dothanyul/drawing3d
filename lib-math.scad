// functions to deal with scalars

include <lib-list.scad>


// identity function
id = function(x) x;

// mod that deals with negatives correctly
function mod(a,b) = (a % b + b) % b;

// Euclid's algorithm
function gcd(a,b) = 
    a<b ? gcd(b,a) :
    b==0 ? a : 
    gcd(b, a%b);

// sum a list of numbers
function sum(L) = 
    L == [] ? 0 :
    is_num(L[0]) ? L[0] + sum(cdr(L)) :
    sum(cdr(L));

// don't give arguments more than 15
function nybble(n) = n < 10 ? str(floor(n)) : chr(floor(n)+55);
// don't give arguments outside unsigned byte range
function byte(n) = 
    n > 255 ? "FF" : 
    n < 0 ? "00" : 
    n < 16 ? str("0", nybble(n)) : 
    hex(n);

// convert number to hexadecimal string
function hex(n) = 
    !is_num(n) ? n : 
    n < 16 ? nybble(n) :
    str(hex(n / 16), nybble(n % 16));

// find the angle of an arc given length and radius
function angle(arclength, radius) = arclength / (radius * 2 * PI) * 360;

// functions to deal with vectors

// standard basis vectors in R3
i3 = [1, 0, 0];
j3 = [0, 1, 0];
k3 = [0, 0, 1];

// zero vector in Rn
function vec_0(n) = [for(i=[0:1:n-1]) 0];

// average of two vectors
function vec_avg(a,b) = (a + b) / 2;

// polar angle (argument) of a vector in R2
function vec_ang(a) = atan2(a.y, a.x);

// dot product of two vectors in Rn
// set strict = true to fail when len(a) != len(b)
function dot(a, b, strict = false) = 
    strict && len(a) != len(b) ? undef :
    a == [] || b == [] ? 0 :
    a[0] * b[0] + dot(cdr(a), cdr(b));

// cross product of two vectors in R3
function cross(a, b) = [a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * a.y - a.y * a.x];

// rotate in xy plane
function vec_rot(a,t) = [a.x * cos(t) - a.y * sin(t), a.y * cos(t) + a.x * sin(t), each(cdr(cdr(a)))];

// distance between two vectors in Rd
function distance(a,b) = sqrt(distsq(a, b));

// distance squared between two vectors in Rd
function distsq(a, b) =
    is_num(a) ? pow(a - b, 2) : 
    len(a) == 0 ? 0 :
    pow(a[0] - b[0], 2) + distsq(cdr(a), cdr(b));

// distance between polar points in R x [0,360)
function dist_polar(a, b) = 
    let(
        c = [a[0] * cos(a[1]), a[0] * sin(a[1])],
        d = [b[0] * cos(b[1]), b[0] * sin(b[1])])
    distance(c,d);

// compare the distance between two vectors in Rd
function dist_comp(a,b) = (is_undef(a) || is_undef(b)) ? 0 : distance(a[0], a[1]) - distance(b[0], b[1]);

// calculate the length of a parametric function f : [0:360] -> R2 with resolution dt
function length(f, dt) = 
    sum([for(t=[0:360:dt]) 
        let(t0 = t, t1 = t + dt)
        distance(f(t0), f(t1))
    ]);

// polar angle bac in (-180,180] in xy plane
function subtend(a,b,c) = (function(b,c) vec_ang(vec_rot(c, -atan2(b.y, b.x)))) (b-a, c-a);

// comparators for x, y, and z coordinates of vectors in R3
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

// matrix inverse of a 3x3 matrix (formula stolen from Wolfram Alpha)
function mat_inv(A) = 
    let(a = A[0][0], b = A[0][1], c = A[0][2], 
        d = A[1][0], e = A[1][1], f = A[1][2],
        g = A[2][0], h = A[2][1], i = A[2][2])
    let(stuff = a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g)
    stuff == 0 ? undef : 
    [[e * i - f * h, c * h - b * i, b * f - c * e],
    [f * g - d * i, a * i - c * g, c * d - a * f],
    [d * h - e * g, b * g - a * h, a * e - b * d]]
    / stuff;













