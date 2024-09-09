#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

void jacobi(double* restrict dst, double* restrict src, size_t h, size_t w) {
    #pragma omp parallel for schedule(static)
    for (size_t row = 1; row < h-1; ++row) {
        for (size_t col = 1; col < w-1; ++ col) {
            size_t i = row * w + col;
            dst[i] = (src[i+1] + src[i-1] + src[i+w] + src[i-w]) * 0.25;
        }
    }
}

double tdiff(struct timeval* start_time, struct timeval* end_time) {
    long sec  = end_time->tv_sec  - start_time->tv_sec;
    long usec = end_time->tv_usec - start_time->tv_usec;
    double seconds     = (double)sec + (double)usec / (1000.0 * 1000.0);
    return seconds;
}

double *alloc(size_t h, size_t w) {
    double* data = (double*)malloc(h*w*sizeof(double));
    for (size_t i = 0; i < h * w; ++i) {
        data[i] = 0.01;
    }
    return data;
}

int main(int argc, char** argv) {
    if(argc != 3) {
        printf("Usage: %s h w\n", argv[0]);
        exit(EXIT_FAILURE);
    };
    size_t h = atoi(argv[1]);
    size_t w = atoi(argv[2]);
    double* dst = alloc(h, w);
    double* src = alloc(h, w);
    double benchtime = 2.0; // seconds

    struct timeval start_time;
    struct timeval end_time;
    size_t nrep = 0;
    gettimeofday(&start_time, NULL);
    do {
        jacobi(dst, src, h, w);
        double* tmp = dst;
        dst = src;
        src = tmp;
        nrep += 1;
        gettimeofday(&end_time, NULL);
    } while (tdiff(&start_time, &end_time) < benchtime);

    double flops = ((w-2) * (h-2) * 4 * nrep) / tdiff(&start_time, &end_time);

    printf("| %5lu | %5lu | %5.2e |\n",
           h, w, flops);

    return 0;
}
