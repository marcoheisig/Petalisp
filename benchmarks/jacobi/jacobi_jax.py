#!/usr/bin/env python

import jax.numpy as jnp
import time
import sys

def jacobi(u):
    return u.at[1:-1, 1:-1].set(u[0:-2, 1:-1] + u[2:, 1:-1] + u[1:-1, 0:-2] + u[1:-1, 2:]) * 0.25

def main(h, w):
    src = jnp.zeros((h, w), dtype=jnp.float64)
    dst = jnp.zeros((h, w), dtype=jnp.float64)

    nrep = 0
    t0 = time.time()
    while True:
        for _ in range(10):
            dst = jacobi(src)
            dst, src = src, dst
            nrep += 1
        src.block_until_ready()
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
