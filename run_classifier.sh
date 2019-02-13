#!/bin/bash

is_base=false
for key in ${BASH_ARGV[*]}
do
    if [ $key = "create_base" ]
    then
        is_base=true
        break
    fi
done

if $is_base
then
    ocaml create_bases.ml
    ocaml bayessian_classifier.ml base
else
    ocaml bayessian_classifier.ml
fi
