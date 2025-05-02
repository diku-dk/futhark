[due@nixos:~/git/diku-dk/futhark]$ futhark test -c --backend=multicore tests --no-tuning
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

┌──────────┬────────┬────────┬───────────┐
│          │ passed │ failed │ remaining │
├──────────┼────────┼────────┼───────────┤
│ programs │ 2335   │ 13     │ 0/2348    │
├──────────┼────────┼────────┼───────────┤
│ runs     │ 3294   │ 10     │ 0/3304    │
└──────────┴────────┴────────┴───────────┘
