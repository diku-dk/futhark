src/Futhark/CodeGen/ImpGen/GPU/SegScan/TwoPass.hs:492:42: warning: [-Wunused-matches]
    Defined but not used: ‘post_op’
    |
492 | compileSegScan pat lvl space scans kbody post_op = do
    |                                          ^^^^^^^

src/Futhark/CodeGen/ImpGen/GPU/SegScan/SinglePass.hs:224:48: warning: [-Wunused-matches]
    Defined but not used: ‘post_op’
    |
224 | compileSegScan pat lvl space scan_op map_kbody post_op = do
    |                                                ^^^^^^^

src/Futhark/CodeGen/ImpGen/GPU/Block.hs:368:66: warning: [-Wunused-matches]
    Defined but not used: ‘post_op’
    |
368 | compileBlockOp pat (Inner (SegOp (SegScan lvl space _ body scans post_op))) = do
    |
