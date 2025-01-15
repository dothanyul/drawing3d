
lib-color holds functions for making colors: nybble, hex, denybble, fromhex, byte, color_list, color_str, color_num, color_spline, color_path, rainbow, gray, pulse, randcolor, color_add, color_sum, color_scale, rgb, hsv, and synth, as well as constant default. Periodic parametric color functions have period 1.

lib-draw holds various drawing utilities: poly, fill, line, tetra, origin, frame, black, pyramid, sector, colorspace, loft, sweep_nondifferentiable, follow, sweep, ruler. Functions for paths or face outlines are evaluated over domain [0,1].

lib-func holds parametric functions of one scalar value: f0, to, arc, rect, cyl, stroke, segment, path, spline, hilbert2, hilbert3, dragon, quindragon, levy, koch, octahedra, hexahedra. All are evaluated over domain [0,1].

lib-gear holds modules that draw herringbone gears: gear, spur, rack, ring, planetary, and bevel, as well as radius. Currently tooth dimensions are globals and their exposure to consumers is somewhat haphazard; this should be cleaned up in the future.

lib-list holds list utilities: cdr, reverse, distinct, flatten, shuffle, sum, land, lor, avg, splice, remove, filter, select, sort, contains, merge, index, anamorph.

lib-math holds the identity function and the standard basis vectors, as well as functions dealing with:
* scalars: is_finite, comp, mod, gcd, primes, factors, truncate, angle, logrest, base, thomae, bitwise, and, or, xor, nand, nor.
* vectors: v0, raise, vec\_ang, lat, long, unit, dot, rot\_k, rot\_j, rot_i, cmult, distance, distsq, dist\_polar, dist\_comp, direct, align, face, x\_comp, y\_comp, z\_comp, allsum.
* matrices: is_matrix, mat\_insert, mat_i, mat\_mult, mat\_prod, mat\_apply, det, mat\_inv.
* functions: length, avg_dist, integral, normal.

lib-view things related to the 3D viewport: visible, vp, world, and background.

Currently working tests in test: 
- lib-color: color_spline, rainbow, randcolor, color_sum, synth
- lib-math: truncate, logrest, base, rot, align
- lib-draw: sector, follow, sweep, loft
- lib-gear: gears, planetary, bevel
- lib-func: stroke, path, spline, hilbert2, hilbert3, dragon, hedra
- lib-view: visible, world

Tests are currently as-needed; since running a test involves visual inspection tests are tailored to the edits that require them

Dependencies:
<!---
     [d] [g]
     / \ /
    [c][f][v]
      \ | / 
      [ m ]
        |
       [l]
-->
* draw imports color and func
* gear imports func
* func, color, and view each import math
* math imports list
* list imports nothing
