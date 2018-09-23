#!/bin/bash
path=/usr/local/esmf/mod/modO/Linux.gfortran.64.mpiuni.default/esmf.mod
mpif90 -o test test.F90 $path -I/usr/local/esmf/include -L/usr/local/esmf/lib 
