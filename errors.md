[due@nixos:~/git/diku-dk/futhark]$ futhark test -c --backend=multicore tests --no-tuning
tests/intragroup/scan0.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/intragroup/scan0:
Entry point: main; dataset: [10][256]i32:
tests/intragroup/scan0.fut.main.1.actual and tests/intragroup/scan0.fut.main.1.expected do not match:
Value #0 index [0,0]: expected -223823371, got -1131114624
...and 2559 other mismatches.
tests/issue1435.fut:
Compiling with --backend=multicore:
Warning at tests/issue1435.fut:39:16-18:
  Unused name "xs".

Warning at tests/issue1435.fut:39:20-22:
  Unused name "ys".

Warning at tests/issue1435.fut:40:13-47:44:
  Defaulting ambiguous type to i32.

Warning at tests/issue1435.fut:40:24-26:
  Unused name "xs".

Warning at tests/issue1435.fut:40:32-33:
  Unused name "x".

Warning at tests/issue1435.fut:42:36-38:
  Unused name "ys".
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In expression of statement
  {defunc_0_map_res_9096 : ({}, [idxs_8809]i32),
   defunc_0_map_res_pretr_9097 : ({}, [idxs_8809][3i64]i32)}
In call of anonymous function:
expecting 2 arguments of type(s)
i64, i64
Got 2 arguments of types
bool, i64

tests/quickselect.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/quickselect:
Entry point: main; dataset: #1 ("[4i32, -8i32, 2i32, 2i32, 0i32, 0i32, 5i32, 9i32, ..."):
tests/quickselect.fut.main.1.actual and tests/quickselect.fut.main.1.expected do not match:
Value #0: expected 4, got 2
tests/issue419.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/issue419:
Entry point: main; dataset: #0 ("[1i32, 3i32, 1i32, 1i32, 1i32, 53i32, 2i32, 2i32, ..."):
tests/issue419.fut.main.0.actual and tests/issue419.fut.main.0.expected do not match:
Value #0 index [0]: expected 5, got 31
...and 7 other mismatches.
tests/issue656.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_main
When checking function body
In expression of statement
  {defunc_0_map_res_6320 : ({}, [n_6040]i32)}
Type error:
Anonymous function defined with 2 parameters:
[eta_p_6217 : i32, eta_p_6218 : i32]
but expected to take 1 arguments.

tests/issue1755.fut:
Command failed with ExitFailure 1:
./tests/issue1755
Stderr:
./tests/issue1755: Error during context initialisation:
Error: Index [-1] out of bounds for array of shape [0].

Backtrace:
-> #0  /prelude/array.fut:28:29-37
   #1  tests/issue1755.fut:37:25-35
   #2  tests/issue1755.fut:54:18-56:28
   #3  tests/issue1755.fut:58:40-59:34
   #4  tests/issue1755.fut:61:12-45

CallStack (from HasCallStack):
  error, called at src/Futhark/Server.hs:172:7 in futhark-server-1.2.3.0-2cb378fa80f3187f3efd972e31a2d20d0a789b56cca5e72699e68c9ece532093:Futhark.Server
tests/distribution/segscan0.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/distribution/segscan0:
Entry point: main; dataset: #0 ("[[1i32, 2i32], [3i32, 4i32]] [[5.0f32, 6.0f32], [7..."):
tests/distribution/segscan0.fut.main.0.actual and tests/distribution/segscan0.fut.main.0.expected do not match:
Value #0 index [1,0]: expected 7.0, got 6.354691e-33
...and 3 other mismatches.
tests/distribution/icfp16-example.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/distribution/icfp16-example:
Entry point: main; dataset: #0 ("[[1i32, 2i32, 3i32], [3i32, 2i32, 1i32], [4i32, 5i..."):
tests/distribution/icfp16-example.fut.main.0.actual and tests/distribution/icfp16-example.fut.main.0.expected do not match:
Value #0 index [0,0,0]: expected 1, got 372743504
...and 7 other mismatches.
tests/psums.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/psums:
Entry point: main; dataset: #0 ("[[0i32, 1i32, 2i32, 3i32], [4i32, 5i32, 6i32, 7i32..."):
tests/psums.fut.main.0.actual and tests/psums.fut.main.0.expected do not match:
Value #0 index [0,0]: expected 0, got 896322416
...and 3 other mismatches.
tests/fusion/map-scan3.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/fusion/map-scan3:
Entry point: main; dataset: #1 ("10i64 1000i64"):
tests/fusion/map-scan3.fut.main.1.actual and tests/fusion/map-scan3.fut.main.1.expected do not match:
Value #0: expected 1986778316, got -974856980
tests/fusion/iswim1.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/fusion/iswim1:
Entry point: main; dataset: #0 ("[[1i32, 2i32, 3i32], [4i32, 5i32, 6i32], [7i32, 8i..."):
tests/fusion/iswim1.fut.main.0.actual and tests/fusion/iswim1.fut.main.0.expected do not match:
Value #0 index [0,1]: expected 4, got 993110177
...and 5 other mismatches.
tests/fusion/WithAccs/ker2-radix.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_radixIt
When checking function body
In expression of statement
  {scatter_res_9990 : ({}, [d<{(*) m bq}>_8786]i32),
   defunc_0_map_res_pretr_pretr_9907 : ({}, [m_8787][bq_8788]i64),
   defunc_0_map_res_pretr_pretr_9908 : ({}, [m_8787][bq_8788]i32)}
In expression of statement
  {acc_res_10544 : ({}, acc(acc_cert_p_10102, [d<{(*) m bq}>_8786], {i32})),
   defunc_0_map_res_pretr_pretr_10545 : ({}, [m_8787][bq_8788]i64),
   defunc_0_map_res_pretr_pretr_10546 : ({}, [m_8787][bq_8788]i32)}
In expression of statement
  {xs'_10191 : ({eta_p_10189}, [bq_8788]i32)}
Inside the loop body
Type error:
defunc_0_map_res_10111 was invalidly consumed.
Nothing can be consumed here.


tests/fusion/fuse-across-transpose6.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/fusion/fuse-across-transpose6:
Entry point: main; dataset: #0 ("[[1.0f32, 0.6f32, 0.8f32], [0.6f32, 0.8f32, 0.15f3..."):
tests/fusion/fuse-across-transpose6.fut.main.0.actual and tests/fusion/fuse-across-transpose6.fut.main.0.expected do not match:
Value #0 index [0,1]: expected 1.3138063, got 1.8221047e-28
...and 9 other mismatches.
tests/fusion/iswim2.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/fusion/iswim2:
Entry point: main; dataset: #0 ("[[1i32, 2i32, 3i32], [4i32, 5i32, 6i32], [7i32, 8i..."):
tests/fusion/iswim2.fut.main.0.actual and tests/fusion/iswim2.fut.main.0.expected do not match:
Value #0 index [0,1]: expected 7, got 790138215
...and 5 other mismatches.
tests/issue1794.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_main
When checking function body
In expression of statement
  {defunc_0_map_res_7087 : ({}, [aoa_len_6958]bool)}
Type error:
Anonymous function defined with 1 parameters:
[eta_p_6992 : i64]
but expected to take 2 arguments.

tests/issue1838.fut:
Compiling with --backend=multicore:
Warning at tests/issue1838.fut:7:37-38:
  Unused name "i".
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_main
When checking function body
In expression of statement
  {defunc_0_map_res_pretr_6046 : ({}, [n_5735][n_5735]i64)}
In expression of statement
  {xs_6009 : ({smaller_replicate_6008}, [n_5735]i64)}
Inside the loop body
Type error:
xs_6011 was invalidly consumed.
Nothing can be consumed here.


tests/accs/fusion0.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/accs/fusion0:
Entry point: main; dataset: #0 ("[0i32, 1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8..."):
tests/accs/fusion0.fut.main.0.actual and tests/accs/fusion0.fut.main.0.expected do not match:
Value #0 index [0]: expected 0, got 162
...and 18 other mismatches.
tests/issue1610.fut:
Compiling with --backend=multicore:
Warning at tests/issue1610.fut:68:50-51:
  Unused name "i".
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_main
When checking function body
In expression of statement
  {defunc_0_f_res_10807 : ({}, [i32_res_9868]i32)}
Type error:
Anonymous function defined with 1 parameters:
[eta_p_9946 : i32]
but expected to take 2 arguments.

tests/soacs/scan4.fut:
Compiling with --backend=multicore:
Warning at tests/soacs/scan4.fut:21:17-18:
  Unused name "i".
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_main
When checking function body
In expression of statement
  {main_res_5702 : ({xs_5617}, [n_5616]i32)}
Inside the loop body
In expression of statement
  {defunc_0_map_res_5742 : ({}, [n_5616]i32)}
Type error:
Anonymous function defined with 2 parameters:
[eta_p_5726 : i32, eta_p_5727 : i32]
but expected to take 1 arguments.

tests/soacs/scan7.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/soacs/scan7:
Entry point: main; dataset: [10][1][10]i32:
tests/soacs/scan7.fut.main.0.actual and tests/soacs/scan7.fut.main.0.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got 1010770128
...and 99 other mismatches.
Entry point: main; dataset: [10][10][1]i32:
tests/soacs/scan7.fut.main.1.actual and tests/soacs/scan7.fut.main.1.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got 718877817
...and 99 other mismatches.
Entry point: main; dataset: [10][10][10]i32:
tests/soacs/scan7.fut.main.2.actual and tests/soacs/scan7.fut.main.2.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got -692957936
...and 999 other mismatches.
tests/soacs/map9.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/soacs/map9:
Entry point: main; dataset: #0 ("[1i32, 1i32, 1i32] [[1i32, 2i32, 3i32], [4i32, 5i3..."):
tests/soacs/map9.fut.main.0.actual and tests/soacs/map9.fut.main.0.expected do not match:
Value #0 index [0,1]: expected 6, got 664159523
...and 9 other mismatches.
tests/soacs/scan8.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/soacs/scan8:
Entry point: main; dataset: [10][1][10]i32:
tests/soacs/scan8.fut.main.0.actual and tests/soacs/scan8.fut.main.0.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got 568271072
...and 99 other mismatches.
Entry point: main; dataset: [10][10][1]i32:
tests/soacs/scan8.fut.main.1.actual and tests/soacs/scan8.fut.main.1.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got -18844749
...and 99 other mismatches.
Entry point: main; dataset: [10][10][10]i32:
tests/soacs/scan8.fut.main.2.actual and tests/soacs/scan8.fut.main.2.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got -153029197
...and 999 other mismatches.
tests/soacs/scan5.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/soacs/scan5:
Entry point: main; dataset: [1][100]i32:
tests/soacs/scan5.fut.main.0.actual and tests/soacs/scan5.fut.main.0.expected do not match:
Value #0 index [0,0]: expected -223823371, got 17183
...and 99 other mismatches.
Entry point: main; dataset: [100][1]i32:
tests/soacs/scan5.fut.main.1.actual and tests/soacs/scan5.fut.main.1.expected do not match:
Value #0 index [0,0]: expected -223823371, got -1753277664
...and 99 other mismatches.
Entry point: main; dataset: [100][100]i32:
tests/soacs/scan5.fut.main.2.actual and tests/soacs/scan5.fut.main.2.expected do not match:
Value #0 index [0,0]: expected -223823371, got 70362655
...and 9999 other mismatches.
tests/soacs/scan6.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/soacs/scan6:
Entry point: main; dataset: [1][10][100]i32:
tests/soacs/scan6.fut.main.0.actual and tests/soacs/scan6.fut.main.0.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got -702602832
...and 999 other mismatches.
Entry point: main; dataset: [100][10][1]i32:
tests/soacs/scan6.fut.main.1.actual and tests/soacs/scan6.fut.main.1.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got 0
...and 999 other mismatches.
Entry point: main; dataset: [100][10][100]i32:
tests/soacs/scan6.fut.main.2.actual and tests/soacs/scan6.fut.main.2.expected do not match:
Value #0 index [0,0,0]: expected -223823371, got -702602960
...and 99907 other mismatches.
tests/ad/scan4.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_fwd_J
When checking function body
In expression of statement
  {defunc_0_f_res_pretr_8342 : ({}, [n_7004][n_7004][3i64]f32)}
In expression of statement
  {defunc_res_pretr_8339 : ({}, [n_7004][3i64]f32)}
Type error:
Anonymous function defined with 3 parameters:
[eta_p_7951 : f32, eta_p_7952 : f32, eta_p_7953 : f32]
but expected to take 5 arguments.

tests/ad/scan8.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_fwd
When checking function body
In expression of statement
  {defunc_0_f_res_pretr_16746 : ({}, [d<{(*) n 9}>_13008][n_10045][9i64]f32)}
In expression of statement
  {defunc_res_pretr_16736 : ({}, [n_10045][9i64]f32)}
Type error:
Anonymous function defined with 9 parameters:
[eta_p_13389 : f32, eta_p_13390 : f32, eta_p_13391 : f32, eta_p_13392 : f32, eta_p_13393 : f32, eta_p_13394 : f32, eta_p_13395 : f32, eta_p_13396 : f32, eta_p_13397 : f32]
but expected to take 18 arguments.

tests/ad/scan7.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/scan7:
Entry point: fwd_J; dataset: #0 ("[[[1.0f32, 2.0f32], [2.0f32, 3.0f32]], [[4.0f32, 5..."):
tests/ad/scan7.fut.fwd_J.0.actual and tests/ad/scan7.fut.fwd_J.0.expected do not match:
Value #0 index [0,0,0,0,0,0]: expected 1.0, got -8412.641
...and 34 other mismatches.
Entry point: test; dataset: #0 ("[[[1.0f32, 2.0f32], [2.0f32, 3.0f32]], [[4.0f32, 5..."):
tests/ad/scan7.fut.test.0.actual and tests/ad/scan7.fut.test.0.expected do not match:
Value #0 index [0,1,1,0,1,1]: expected 1.0, got 0.0
tests/ad/scan2.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/scan2:
Entry point: fwd_J; dataset: #0 ("[[1.0f32, 1.0f32], [2.0f32, 2.0f32], [3.0f32, 3.0f..."):
tests/ad/scan2.fut.fwd_J.0.actual and tests/ad/scan2.fut.fwd_J.0.expected do not match:
Value #0 index [1,1,0]: expected 1.0, got 0.0
...and 15 other mismatches.
Entry point: rev_J; dataset: #0 ("[[1.0f32, 1.0f32], [2.0f32, 2.0f32], [3.0f32, 3.0f..."):
tests/ad/scan2.fut.rev_J.0.actual and tests/ad/scan2.fut.rev_J.0.expected do not match:
Value #0 index [0,0,0]: expected 1.0, got 0.0
tests/ad/scan1.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/scan1:
Entry point: fwd_J; dataset: #0 ("[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]"):
tests/ad/scan1.fut.fwd_J.0.actual and tests/ad/scan1.fut.fwd_J.0.expected do not match:
Value #0 index [1,1]: expected 1.0, got 0.0
...and 8 other mismatches.
Entry point: rev_J; dataset: #0 ("[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]"):
tests/ad/scan1.fut.rev_J.0.actual and tests/ad/scan1.fut.rev_J.0.expected do not match:
Value #0 index [0,4]: expected 0.0, got -3.2348173e28
...and 11 other mismatches.
tests/ad/scan9.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_fwd
When checking function body
In expression of statement
  {defunc_0_f_res_pretr_29537 : ({}, [d<{(*) n 16}>_17926][n_12616][16i64]f32)}
In expression of statement
  {defunc_res_pretr_29520 : ({}, [n_12616][16i64]f32)}
Type error:
Anonymous function defined with 16 parameters:
[eta_p_18630 : f32, eta_p_18631 : f32, eta_p_18632 : f32, eta_p_18633 : f32, eta_p_18634 : f32, eta_p_18635 : f32, eta_p_18636 : f32, eta_p_18637 : f32, eta_p_18638 : f32, eta_p_18639 : f32, eta_p_18640 : f32, eta_p_18641 : f32, eta_p_18642 : f32, eta_p_18643 : f32, eta_p_18644 : f32, eta_p_18645 : f32]
but expected to take 32 arguments.

tests/ad/scan5.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/scan5:
Entry point: fwd_J; dataset: #0 ("[[1.0f32, 1.0f32], [2.0f32, 2.0f32], [3.0f32, 3.0f..."):
tests/ad/scan5.fut.fwd_J.0.actual and tests/ad/scan5.fut.fwd_J.0.expected do not match:
Value #0 index [0,0,0]: expected 1.0, got 0.0
...and 26 other mismatches.
Entry point: rev_J; dataset: #0 ("[[1.0f32, 1.0f32], [2.0f32, 2.0f32], [3.0f32, 3.0f..."):
tests/ad/scan5.fut.rev_J.0.actual and tests/ad/scan5.fut.rev_J.0.expected do not match:
Value #0 index [1,0,0]: expected 2.0, got 0.0
...and 3 other mismatches.
tests/ad/reducebyindex1.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/reducebyindex1:
Entry point: rev; dataset: #0 ("[0i64, 1i64, 2i64, 3i64, 2i64, 1i64, 0i64, 1i64, 2..."):
tests/ad/reducebyindex1.fut.rev.0.actual and tests/ad/reducebyindex1.fut.rev.0.expected do not match:
Value #2 index [8]: expected 0.0, got 1.0
...and 1 other mismatches.
Entry point: revvec; dataset: #0 ("[0i64, 1i64, 2i64, 1i64, 0i64, 1i64] [[1.0f32, 2.0..."):
tests/ad/reducebyindex1.fut.revvec.0.actual and tests/ad/reducebyindex1.fut.revvec.0.expected do not match:
Value #2 index [3,0]: expected 0.0, got 1.0
...and 13 other mismatches.
tests/ad/scan0.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/scan0:
Entry point: fwd_J; dataset: #0 ("[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]"):
tests/ad/scan0.fut.fwd_J.0.actual and tests/ad/scan0.fut.fwd_J.0.expected do not match:
Value #0 index [1,1]: expected 1.0, got 0.0
...and 9 other mismatches.
Entry point: rev_J; dataset: #0 ("[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]"):
tests/ad/scan0.fut.rev_J.0.actual and tests/ad/scan0.fut.rev_J.0.expected do not match:
Value #0 index [1,0]: expected 2.0, got 1.0
...and 13 other mismatches.
tests/ad/issue2239.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/issue2239:
Entry point: main; dataset: #0 ("[1.0f64, 1.0f64, 1.0f64]"):
tests/ad/issue2239.fut.main.0.actual and tests/ad/issue2239.fut.main.0.expected do not match:
Value #0 index [0]: expected -4.0, got 0.0
...and 1 other mismatches.
tests/ad/scan3.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_fwd_J
When checking function body
In expression of statement
  {defunc_0_f_res_pretr_10788 : ({}, [d<{(*) n 4}>_9781][n_8305][4i64]f32)}
In expression of statement
  {defunc_res_pretr_10783 : ({}, [n_8305][4i64]f32)}
Type error:
Anonymous function defined with 4 parameters:
[eta_p_9961 : f32, eta_p_9962 : f32, eta_p_9963 : f32, eta_p_9964 : f32]
but expected to take 8 arguments.

tests/ad/scangenbenchtests.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_testmm2by2
When checking function body
In expression of statement
  {defunc_0_map_res_pretr_60074 : ({}, [d<{(*) n 4}>_39610][n_23123]i32),
   defunc_0_map_res_pretr_60075 : ({}, [d<{(*) n 4}>_39610][n_23123]i32),
   defunc_0_map_res_pretr_60076 : ({}, [d<{(*) n 4}>_39610][n_23123]i32),
   defunc_0_map_res_pretr_60077 : ({}, [d<{(*) n 4}>_39610][n_23123]i32),
   defunc_0_f_res_pretr_60078 : ({}, [d<{(*) n 4}>_39610][n_23123][4i64]i32)}
In expression of statement
  {defunc_res_pretr_60050 : ({}, [n_23123][4i64]i32)}
Type error:
Anonymous function defined with 4 parameters:
[eta_p_41112 : i32, eta_p_41113 : i32, eta_p_41114 : i32, eta_p_41115 : i32]
but expected to take 8 arguments.

tests/ad/issue2256.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/issue2256:
Entry point: fwd; dataset: #0 ("[1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64]"):
tests/ad/issue2256.fut.fwd.0.actual and tests/ad/issue2256.fut.fwd.0.expected do not match:
Value #0 index [0]: expected 5040.0, got 1.386629829261683e-309
...and 4 other mismatches.
tests/ad/scan6.fut:
Compiling with --backend=multicore:
Internal compiler error.  Please report this:
  https://github.com/diku-dk/futhark/issues
Type error after pass 'Fuse SOACs':
In function entry_fwd_J
When checking function body
In expression of statement
  {defunc_0_f_res_pretr_15100 : ({}, [d<{(*) n 2}>_13280][n_9800][2i64]f32)}
In expression of statement
  {defunc_res_pretr_15097 : ({}, [n_9800][2i64]f32)}
Type error:
Anonymous function defined with 2 parameters:
[eta_p_13608 : f32, eta_p_13609 : f32]
but expected to take 4 arguments.

tests/ad/issue1879.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/issue1879:
Entry point: main_ad; dataset: #1 ("[[1.0f64, 2.0f64, 3.0f64], [7.0f64, 8.0f64, 9.0f64..."):
tests/ad/issue1879.fut.main_ad.1.actual and tests/ad/issue1879.fut.main_ad.1.expected do not match:
Value #0 index [0,0]: expected 1.0, got 0.0
...and 2 other mismatches.
tests/ad/reducebyindex0.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/reducebyindex0:
Entry point: main; dataset: #0 ("[4i64, 5i64, 2i64, 4i64, 5i64, 2i64, 0i64, 0i64, 4..."):
tests/ad/reducebyindex0.fut.main.0.actual and tests/ad/reducebyindex0.fut.main.0.expected do not match:
Value #1 index [0]: expected 0.0, got 50.0
...and 7 other mismatches.
tests/ad/reducebyindex4.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/reducebyindex4:
Entry point: rev; dataset: #0 ("[0i64, 1i64, 2i64, 1i64, 0i64, 1i64, 2i64, 1i64, 0..."):
tests/ad/reducebyindex4.fut.rev.0.actual and tests/ad/reducebyindex4.fut.rev.0.expected do not match:
Value #0 index [0]: expected 252.0, got 1.0
...and 16 other mismatches.
tests/ad/reducebyindexgenbenchtests.fut:
Compiling with --backend=multicore:
Running compiled program:
Running ./tests/ad/reducebyindexgenbenchtests:
Entry point: argmax; dataset: [500]u64 [50]i32 [50]i32 [500]i32 [500]i32:
tests/ad/reducebyindexgenbenchtests.fut.argmax.0.actual and tests/ad/reducebyindexgenbenchtests.fut.argmax.0.expected do not match:
Value #0: expected True, got False
┌──────────┬────────┬────────┬───────────┐
│          │ passed │ failed │ remaining │
├──────────┼────────┼────────┼───────────┤
│ programs │ 2305   │ 42     │ 0/2347    │
├──────────┼────────┼────────┼───────────┤
│ runs     │ 3250   │ 53     │ 0/3303    │
└──────────┴────────┴────────┴───────────┘
