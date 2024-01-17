# Dot Benchmarks

## dot_product

### ifx

```shell
fpm run --example dot  --profile release --compiler ifx --flag "-Ofast -mtune=native -xHost -qmkl -qopenmp -DINT64"
```

```shell
cd benchmarks/dot && python results/export.py dot_intel.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_speedup.png" width="250">

### gfortran

```shell
fpm run --example dot  --profile release --compiler gfortran --flag "-Ofast -march=native -llapack -lblas -fopenmp -flto -DINT64"
```
```shell
cd benchmarks/dot && python results/export.py dot_gfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_speedup.png" width="250">

### nvidia

```shell
fpm run --example dot  --profile release --compiler nvfortran --flag "-Ofast -fast -march=native -mtune=native -stdpar=gpu,multicore -llapack -lblas -openmp -DINT64"
```

```shell
cd benchmarks/dot && python results/export.py dot_nvfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_speedup.png" width="250">

## dot_product (Coarray)

### ifx

```shell
fpm run --example dot_co  --profile release --compiler ifx --flag "-Ofast -mtune=native -xHost -qmkl -qopenmp -DINT64 -coarray -coarray-num-images=4 -DUSE_COARRAY"
```

```shell
cd benchmarks/dot && python results/export_co.py dot_intel_co.data && cd ../..
cd benchmarks/dot && python results/export_im.py dot_intel_im1.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/benchmarks/dot/results/dot_co_time_max.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_co_perf_tot.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_co_speedup_max.png" width="250">