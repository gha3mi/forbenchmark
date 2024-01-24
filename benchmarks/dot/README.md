# Dot Benchmarks

- [Dot Benchmarks](#dot-benchmarks)
  - [Usage Guide](#usage-guide)
    - [Running Benchmarks](#running-benchmarks)
    - [Generating Results](#generating-results)
  - [dot\_product](#dot_product)
    - [Intel Fortran Compiler (ifx)](#intel-fortran-compiler-ifx)
    - [Intel Fortran Compiler Classic (ifort)](#intel-fortran-compiler-classic-ifort)
    - [GNU Fortran (gfortran)](#gnu-fortran-gfortran)
    - [NVIDIA HPC (nvfortran)](#nvidia-hpc-nvfortran)

## Usage Guide

If you have an implementation for `dot_product`, kindly contribute by adding it to the `dot.f90` or `dot_co.f90` file and submit a pull request.

### Running Benchmarks

Execute the following commands to run benchmarks for different compilers:

```shell
fpm @benchmark-dot-<compiler>
```
`<compiler> : ifx, ifort, gfortran, nvfortran`

For Coarrays:

```shell
fpm @benchmark-dot-<compiler>-coarray
```
`<compiler> : ifx, ifort`

### Generating Results

Run the following commands to generate plots after running benchmarks:

```shell
cd benchmarks/dot
python results/export.py dot_<compiler>.data
cd ../..
```

`<compiler> : ifx, ifort, gfortran, nvfortran`

For Coarrays:

```shell
cd benchmarks/dot
python results/export_co.py dot_<compiler>_co.data
python results/export_im.py dot_<compiler>_im1.data
cd ../..
```

`<compiler> : ifx, ifort`

## dot_product

System specifications for the obtained results:

| **Specification**    | **Details**                                 |
|----------------------|---------------------------------------------|
| **Processor**        | Intel(R) Core(TM) i9-9980HK CPU @ 2.40GHz   |
| **Memory**           | 64 GB                                       |
| **Operating System** | Ubuntu 22.04.3 LTS                      |

### Intel Fortran Compiler (ifx)

| Elapsed Time | Performance |
|--------------|-------------|
| <img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_time.png" width="300"> | <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_perf.png" width="300"> |

| Speedup | Average Weighted Speedup |
|---------|--------------------------|
| <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_speedup.png" width="300"> | <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifx_speedup_avg.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifx.html)

### Intel Fortran Compiler Classic (ifort)

| Elapsed Time | Performance |
|--------------|-------------|
| <img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_time.png" width="300"> | <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_perf.png" width="300"> |

| Speedup | Average Weighted Speedup |
|---------|--------------------------|
| <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_speedup.png" width="300"> | <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_ifort_speedup_avg.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_ifort.html)

### GNU Fortran (gfortran)

| Elapsed Time | Performance |
|--------------|-------------|
| <img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_time.png" width="300"> | <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_perf.png" width="300"> |

| Speedup | Average Weighted Speedup |
|---------|--------------------------|
| <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_speedup.png" width="300"> | <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_gfortran_speedup_avg.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_gfortran.html)

### NVIDIA HPC (nvfortran)

| Elapsed Time | Performance |
|--------------|-------------|
| <img alt="dot_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_time.png" width="300"> | <img alt="dot_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_perf.png" width="300"> |

| Speedup | Average Weighted Speedup |
|---------|--------------------------|
| <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_speedup.png" width="300"> | <img alt="dot_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/dot/results/dot_nvfortran_speedup_avg.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/dot/results/dot_nvfortran.html)