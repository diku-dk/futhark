This directory contains tests for the primtive types in Futhark, their
operators, and some built-in functions.  It is a good place to start
looking when implementing a new code generator backend, because if you
don't add integers correctly, you're unlikely to get anywhere quickly.

We would get a combinatorial explosion if we had one test program for
every combination of type and operator, so instead we try to test
several things in the same program (via different return values, and
perhaps operations predicated on input).
