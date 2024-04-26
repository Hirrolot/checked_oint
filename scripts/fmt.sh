#!/bin/bash

dune build @fmt --auto-promote
find lib -type f \( -iname \*.c -o -iname \*.h \) |
    xargs clang-format -i
