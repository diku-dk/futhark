{ pkgs, python, haskell }:
with pkgs;
[hlint
 ormolu
 parallel
 python.mypy
 python.black
]
