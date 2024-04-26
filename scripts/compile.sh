#!/bin/bash

set -e

clang -c lib/support.c -std=gnu99 -Wall -Wextra -pedantic -Werror
rm support.o
