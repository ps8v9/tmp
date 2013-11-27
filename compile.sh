#!/bin/bash

cobc -xg test.cbl

echo Clean? [Y/n]
read clean
if [ $clean != n ]; then
    rm *.[chi]
fi
