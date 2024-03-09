include <lib-math.scad>

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

// rainbow of period 1 at point n
// at point x1 channel is c1, at point x1+1 channel is c2, so equation for channel is c = 255 * cv * (x - x1) + c1
function rainbow(n) = 
    let(x = mod(n, 1) * 6)
    x < 1 ? color_str([1, 0.5 * x, -0.1 * x + 0.1]) :
    x < 2 ? color_str([1, 0.5 * (x - 1) + 0.5, 0]) :
    x < 3 ? color_str([-1 * (x - 2) + 1, -0.3 * (x - 2) + 1, 0.1 * (x - 2)]) :
    x < 4 ? color_str([0.2 * (x - 3), -0.5 * (x - 3) + 0.7, 0.9 * (x - 3) + 0.1]) :
    x < 5 ? color_str([0.3 * (x - 4) + 0.2, -0.2 * (x - 4) + 0.2, -0.4 * (x - 4) + 1]) :
    color_str([0.5 * (x - 5) + 0.5, 0, -0.5 * (x - 5) + 0.6]);

// oscillating color pattern that displays c1 on every integer and fades down to c2 in between over a polynomial curve with degree power
function pulse(c1, c2, power=1) = function(t)
    let(y = pow(max(t % 1, 1 - (t % 1)) * 2 - 1, power))
    color_sum([c1, c2], [y, 1 - y]);

// random color string with each channel between the given bounds
function randcolor(lo=64, hi=255) =
    color_str(rands(max(lo, 0), min(hi, 255), 3));

// average two color strings
function color_add(c1, c2) = 
    color_str((color_list(c1) + color_list(c2)) / 2);

// average n color strings with optional weights
function color_sum(c, w=[]) =
    let(cl = [for(i=[0:1:len(c)-1]) 
        let(ci = color_list(c[i]), 
            wi = w == [] ? 1 / len(c) : w[i] / sum(w))
        ci * wi])
    color_str(sum(cl));

// scale a color uniformly in brightness by a ϵ [0,1]
function color_scale(c, a) = 
    color_switch(color_switch(c) * a);













