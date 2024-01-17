# Dot Benchmarks

## dot_product

### ifx

```shell
fpm @benchmark-dot-ifx
```

```shell
cd benchmarks/dot && python results/export.py dot_intel.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_speedup.png" width="250">

### gfortran

```shell
fpm @benchmark-dot-gfortran
```
```shell
cd benchmarks/dot && python results/export.py dot_gfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_speedup.png" width="250">

### nvidia

```shell
fpm @benchmark-dot-nvfortran
```

```shell
cd benchmarks/dot && python results/export.py dot_nvfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_time.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_perf.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_speedup.png" width="250">

## dot_product (Coarray)

### ifx

```shell
fpm @benchmark-dot-ifx-coarray
```

```shell
cd benchmarks/dot && python results/export_co.py dot_intel_co.data && cd ../..
cd benchmarks/dot && python results/export_im.py dot_intel_im1.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_co_time_max.png" width="250"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_co_perf_tot.png" width="250"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_intel_co_speedup_max.png" width="250">