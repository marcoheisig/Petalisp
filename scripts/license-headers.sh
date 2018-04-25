#!/bin/bash
# -*- coding: utf-8 -*-

find . \
     -type f \
     -name '*.lisp' \
     -print \
     -exec sed -i 's/^;.*©.*/;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-/g' {} +
