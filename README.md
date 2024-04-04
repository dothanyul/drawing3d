
lib-color holds functions for making colors: nybble, hex, denybble, fromhex, byte, color_list, color_str, color_num, rainbow, pulse, randcolor, color_add, color_sum, color_scale, rgb, hsv, as well as constant default. Periodic parametric color functions have period 1.

lib-draw holds various drawing utilities: poly, fill, origin, frame, pyramid, colorspace, loft, sweep_nondifferentiable, follow, sweep, ruler. Functions for paths or face outlines are evaluated over domain [0,1].

lib-func holds parametric functions of one scalar value: f0, segment, arc, stroke, spline, hilbert2, hilbert3, dragon, levy, koch. All are evaluated over domain [0,1].

lib-list holds list utilities: cdr, reverse, flatten, shuffle, sum, avg, and, or, remove, filter, select, sort, contains, merge, index, anamorph.

lib-math holds the identity function and the standard basis vectors, as well as functions dealing with:
* scalars: is_finite, mod, gcd, truncate, angle, thomae.
* vectors: v0, raise, vec\_ang, unit, dot, rot\_k, rot\_j, rot_i, distance, distsq, dist\_polar, dist\_comp, direct, align, face, x\_comp, y\_comp, z\_comp, allsum.
* matrices: is_matrix, mat\_insert, mat_i, mat\_mult, mat\_prod, mat\_apply, det, mat\_inv.
* functions: length, avg_dist, integral, normal.

lib-view holds visible, as well as its helper vp and vp's inverse world.

Currently working tests in test: truncate, rot, align, follow, sweep, loft, stroke, spline, hilbert2, hilbert3, visible, world

Tests are currently as-needed; since running a test involves visual inspection tests are tailored to the edits that require them

Dependencies:
<!---
     [d]
     /  \
    [c][f][v]
      \ | /
      [ m ]
        |
       [l]
-->
* draw imports color and func
* func imports math
* color imports math
* view imports math
* math imports list
* list imports nothing
