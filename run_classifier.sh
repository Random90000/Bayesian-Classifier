#!/bin/bash

use_base=false
create_base=false
for key in ${BASH_ARGV[*]}
do
    if [ $key = "create_base" ]
    then
        create_base=true
    fi
    if [ $key = "use_base" ]
    then
        use_base=true
    fi
done

if $create_base
then
    ocaml create_bases.ml
    ocaml bayessian_classifier.ml base
elif $use_base
then 
    ocaml bayessian_classifier.ml base
else
    ocaml bayessian_classifier.ml
fi
