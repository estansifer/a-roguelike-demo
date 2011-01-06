#!/bin/bash

# No parameters:
#   Compile all files in src/
# One parameter:
#   Compile all files in src/, then compile module $1 as an executable
#   (it must export a variablen name 'main' of type 'IO ()')
# Two parameters:
#   Also, the executable is called $2

auto_gen_main_name="AutoGenMain"
src_dir="src"
bin_dir="bin"

# ghc --make -O2 *.lhs -threaded -prof -auto-all -i$src_dir
cc="ghc"
#ccopts="--make -O2 -prof -auto-all -i$src_dir"
ccopts="--make -O2 -threaded -i$src_dir"

$cc $ccopts $src_dir/*.lhs $src_dir/*/*.lhs

if [ -n "$1" ]
then

cat > $src_dir/$auto_gen_main_name.hs <<EOF
module Main where
import qualified $1 as A (main)
main = A.main
EOF

$cc $ccopts -o $bin_dir/$1 $src_dir/$auto_gen_main_name.hs

rm $src_dir/$auto_gen_main_name.hs

if [ -n "$2" ]
then
    mv $bin_dir/$1 $bin_dir/$2
fi

fi
