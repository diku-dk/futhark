{ pkgs, python, haskell }:
with pkgs;
[hlint
 ormolu
 parallel
 mypy
 black
]
