# Dot Benchmarks

## dot_product

### ifx

```shell
fpm @benchmark-dot-ifx
```

```shell
cd benchmarks/dot && python results/export.py dot_ifx.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_time.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_perf.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_speedup.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifx.html)

### ifort

```shell
fpm @benchmark-dot-ifort
```

```shell
cd benchmarks/dot && python results/export.py dot_ifort.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_time.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_perf.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_speedup.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifort.html)

### gfortran

```shell
fpm @benchmark-dot-gfortran
```
```shell
cd benchmarks/dot && python results/export.py dot_gfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_time.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_perf.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_speedup.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_gfortran.html)

### nvidia

```shell
fpm @benchmark-dot-nvfortran
```

```shell
cd benchmarks/dot && python results/export.py dot_nvfortran.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_time.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_perf.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_speedup.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_nvfortran.html)

## dot_product (Coarray)

### ifx

```shell
fpm @benchmark-dot-ifx-coarray
```

```shell
cd benchmarks/dot && python results/export_co.py dot_ifx_co.data && cd ../..
cd benchmarks/dot && python results/export_im.py dot_ifx_im1.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_co_time_max.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_co_perf_tot.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_co_speedup_max.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifx_co.html)

### ifort

```shell
fpm @benchmark-dot-ifort-coarray
```

```shell
cd benchmarks/dot && python results/export_co.py dot_ifort_co.data && cd ../..
cd benchmarks/dot && python results/export_im.py dot_ifort_im1.data && cd ../..
```

<img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_co_time_max.png" width="300"> <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_co_perf_tot.png" width="300"> <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_co_speedup_max.png" width="300">

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifort_co.html)
