#!/usr/bin/env python

import numpy as np
import time
import sys

def jacobi(dst, src):
    dst[1:-1, 1:-1] = (src[0:-2, 1:-1] + src[2:, 1:-1] + src[1:-1, 0:-2] + src[1:-1, 2:]) * 0.25

def main(h, w):
    src = np.zeros((h, w), dtype=np.float64)
    dst = np.zeros((h, w), dtype=np.float64)

    nrep = 0
    t0 = time.time()
    while True:
        jacobi(dst, src)
        dst, src = src, dst
        nrep += 1
        t1 = time.time()
        if (dt := (t1 - t0)) > 2.0:
            flops = ((w-2) * (h-2) * 4 * nrep) / dt
            print(f"| {w} | {h} | {flops:.2e} |")
            return


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Use: {sys.argv[0]} h w", file=sys.stderr)
    else:
        main(int(sys.argv[1]), int(sys.argv[2]))
