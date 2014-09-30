#!/bin/bash

auto_gen_main_name="AutoGenMain"
src_dir="src"
bin_dir="."
main_name="CursedThreads"

mkdir -p $bin_dir

cc="ghc"

#ccopts="--make -O2 -prof -auto-all -i$src_dir -package hscurses"
ccopts="--make -O2 -threaded -i$src_dir -package hscurses"

$cc $ccopts $src_dir/*.lhs $src_dir/*/*.lhs

cat > $src_dir/$auto_gen_main_name.hs <<EOF
module Main where
import qualified Roguelike as A (main)
main = A.main
EOF

$cc $ccopts -o $bin_dir/$main_name $src_dir/$auto_gen_main_name.hs

rm $src_dir/$auto_gen_main_name.hs
