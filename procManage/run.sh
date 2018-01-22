#!/bin/bash


mpif90 -c manage.F90
mpif90 -c main.F90
mpif90 -o main main.o manage.o
