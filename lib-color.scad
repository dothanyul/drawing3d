include <lib-math.scad>
include <lib-func.scad>

// functions related to color and rainbow

// default color
default = "#FFD820";

// don't give arguments outside hex range
function nybble(n) = 
    n < 10 ? str(floor(n)) : chr(floor(n)+55);

// convert number to hexadecimal string
function hex(n, acc="") = 
    !is_num(n) ? n : 
    n < 16 ? str(nybble(n), acc) :
    hex(n / 16, str(nybble(n % 16), acc));

// don't give arguments outside hex range
function denybble(c) = let(n = ord(c))
    n >= 48 && n < 58 ? n - 48 :
    n >= 65 && n < 91 ? n - 55 :
    n >= 97 && n < 113 ? n - 87 :
    0;

// convert hexadecimal string to number
function fromhex(s, acc=0) = 
    !is_string(s) ? s : 
    sum([for(i=[1:1:len(s)]) pow(16, i-1) * denybble(s[len(s)-i])]);

// convert integer to a byte as a hex string
// flattens off numbers outside unsigned  byte range
function byte(n) = 
    n > 255 ? "FF" : 
    n < 0 ? "00" : 
    n < 16 ? str("0", nybble(n)) : 
    hex(n);

// convert color string to list of channels in [0,1]^3
function color_list(c) =
    is_num(c) ? color_list(color_str(c)) :
    let(d = floor((len(c) - 1) / 3))
    [for(i=[0:1:2])
        sum([for(j=[1:1:d]) denybble(c[i * d + j]) * pow(16, d - j)]) / pow(16, d)];

// convert color list or decimal number to color string
function color_str(c) =
    is_list(c) ? let(cn = c * 255) str("#", byte(cn[0]), byte(cn[1]), byte(cn[2])) :
    color_str([c * 16 % 16 * 16 + c * pow(16, 4) % 16, c * pow(16, 2) % 16 * 16 + c * pow(16, 5) % 16, c * pow(16, 3) % 16 * 16 + c * pow(16, 6) % 16]);

// convert color string to number (basically a bit shift)
function color_num(c) = 
    is_list(c) ? color_num(color_str(c)) :
    fromhex(str(c[0], c[3], c[1], c[4], c[2], c[5])) / pow(256, 3);

// given a list of color lists, create a spline path through those colors
function color_spline(colors) = function(t)
    let(d = [for(i=[0:1:len(colors)-1]) 
                let(i0 = mod(i-1, len(colors)), i2 = mod(i+1, len(colors)))
                colors[i2] - colors[i0]])
    color_str(spline(colors, d, loop=true)(t));

// given a list of color lists, create a piecewise linear path through those colors
function color_path(colors) = function(t)
    color_str(path(colors, loop = true)(t));

// rainbow of period 1 at point n
// at point x1 channel is c1, at point x1+1 channel is c2, so equation for channel is c = 255 * cv * (x - x1) + c1
function rainbow(t) = color_path([
    [1, 0, 0], 
    [1, 0.5, 0], 
    [1, 1, 0], 
    [0, 0.7, 0.1], 
    [0.2, 0.2, 1], 
    [0.5, 0, 0.5]]
    )(t);

function gray(k) = 
    let(g = k < 0 ? 0 : k > 1 ? 1 : k)
    color_str([for(i=[0:1:2]) g]);

// oscillating color pattern that displays c1 on every integer and fades down to c2 in between over a polynomial curve with degree power
function pulse(c1, c2, power=1) = function(t)
    let(y = pow(max(t % 1, 1 - (t % 1)) * 2 - 1, power))
    color_sum([c1, c2], [y, 1 - y]);

// random color string with each channel between the given bounds
function randcolor(s=undef) =
    let(seed = is_undef(s) ? rands(0, 100, 1)[0] :
        is_num(s) ? s :
        is_list(s) ? let(p = primes(pow(3, len(s))))
            sum([for(i=[0:1:len(s)-1]) s[i] * p[i * 2]]) :
        s)
    let(v = rands(0, 127, 6, seed))
    color_str(shuffle(shuffle([v[0] + 128, v[1], v[2] + (v[3] >= 64 ? 128 : 0)], v[4]), v[5]) / 255);

// average two color strings
function color_add(c1, c2) = 
    color_str((color_list(c1) + color_list(c2)) / 2);

// average n color strings with optional weights
function color_sum(c, w=[]) =
    w == [] ? let(cl = [for(b=c) color_list(b)])
        color_str(sum(cl) / len(c)) :
    let(cl = [for(i=[0:1:len(c)-1]) 
        color_list(c[i]) * w[i]])
    color_str(sum(cl) / sum(w));

// scale a color uniformly in brightness by a ϵ [0,1]
function color_scale(c, a) =
    color_str(color_list(c) * a);

// convert hsv color to rgb
function rgb(hsv) = 
    is_num(hsv) ? color_num(rgb(color_list(hsv))) :
    is_string(hsv) ? color_str(rgb(color_list(hsv))) :
    [for(i=[0:1:2])
        let(r = function(h)
            h < 0 || h > 1 ? r(mod(h, 1)) :
            h < 1/6 || h > 5/6 ? 1 :
            h > 1/3 && h < 2/3 ? 0 :
            h < 1/3 ? (1/3 - h) * 6 :
            (h - 2/3) * 6)
        let(b = hsv[2] * (1 - r(hsv[0] - i/3 - 1/2) * hsv[1]))
        truncate(b, 8)];

// convert rgb color to hsv
function hsv(rgb) = 
    is_string(rgb) ? color_str(hsv(color_list(rgb))) :
    is_num(rgb) ? color_num(hsv(color_list(rgb))) :
    let(v = max(rgb),
        s = max(rgb) - min(rgb),
        hd = rgb - avg(rgb) * [1, 1, 1])
    let(h = rot_k(mat_apply(align([1, 1, 1], k3), hd), 15))
    [mod(atan2(h.y, h.x) / 360, 1), s, v];

// color by numbers
function synth(n, i=0) = 
    n % 1 > 0 ? rainbow(n) :
    n < 0 ? color_scale(synth(-n), 0.5) :
    n == 0 ? gray(0.2) :
    n == 1 ? i == 1 ? gray(0.4) : gray(1) :
    n == 2 ? i == 1 ? color_str([0.95, 0.98, 0.87])  :
        color_str([0.27, 0.47, 1]) :
    n == 3 ? i == 1 ? color_str([0.78, 0.33, 0.13]) :
        color_str([1, 0.54, 0.21]) :
    n == 4 ? i == 1 ? color_str([0.60, 0.64, 0.81]) :
        color_str([0.60, 0.33, 0.8]) :
    n == 5 ? i == 1 ? color_str([1.00, 0.60, 0.16]) :
        i == 2 ? color_str([1, 0.56, 0.77]) :
        color_str([0.97, 0.97, 0.27]) :
    n == 6 ? i == 1 ? color_str([0.17, 0.39, 0.83]) :
            i == 2 ? color_str([1, 1, 1]) :
        color_str([0.2, 0.75, 0.3]) :
    n == 7 ? i == 1 ? color_str([0.3, 0.6, 0.2]) :
            i == 2 ? color_str([0.9, 0.6, 0.1]) :
        color_str([0.44, 0.25, 0.06]) :
    n == 8 ? i == 1 ? color_str([0.86, 0.59, 0.27]) :
        color_str([0.86, 0.86, 0.92]) :
    n == 9 ? i == 1 ? color_str([0.9, 0.9, 0.94]) :
            i == 2 ? color_str([0.21, 0.24, 0.35]) :
        color_str([0.17, 0.74, 0.25]) :
    n == 10 ? i == 1 ? gray(0.2) :
            i == 2 ? color_str([0.97, 0.98, 0.49]) :
            i == 3 ? color_str([0.30, 0.44, 0.8]) :
        color_str([0.87, 0.87, 0.94]) :
    rainbow(n * 2 / (1 + sqrt(5)));





