# MatMul Benchmarks

- [MatMul Benchmarks](#matmul-benchmarks)
  - [Usage Guide](#usage-guide)
    - [Running Benchmarks](#running-benchmarks)
    - [Generating Results](#generating-results)
  - [matmul (matrix-matrix)](#matmul-matrix-matrix)
    - [Intel Fortran Compiler (ifx)](#intel-fortran-compiler-ifx)
    - [Intel Fortran Compiler Classic (ifort)](#intel-fortran-compiler-classic-ifort)
    - [GNU Fortran (gfortran)](#gnu-fortran-gfortran)
    - [NVIDIA HPC (nvfortran)](#nvidia-hpc-nvfortran)
  - [matmul (matrix-vector)](#matmul-matrix-vector)
    - [Intel Fortran Compiler (ifx)](#intel-fortran-compiler-ifx-1)
    - [Intel Fortran Compiler Classic (ifort)](#intel-fortran-compiler-classic-ifort-1)
    - [GNU Fortran (gfortran)](#gnu-fortran-gfortran-1)
    - [NVIDIA HPC (nvfortran)](#nvidia-hpc-nvfortran-1)

## Usage Guide

If you have an implementation for `matmul`, kindly contribute by adding it to the `matmul_mm.f90` (matrix-matrix) or `matmul_mv.f90` (matrix-vector) or `matmul_mm_co.f90` or `matmul_mv_co.f90` file and submit a pull request.

### Running Benchmarks

Execute the following commands to run benchmarks for different compilers:

```shell
fpm @benchmark-matmul-mm-<compiler>
fpm @benchmark-matmul-mv-<compiler>
```
`<compiler> : ifx, ifort, gfortran, nvfortran`

For Coarrays:

```shell
fpm @benchmark-matmul-mm-<compiler>-coarray
fpm @benchmark-matmul-mv-<compiler>-coarray
```
`<compiler> : ifx, ifort`

### Generating Results

Run the following commands to generate plots after running benchmarks:

```shell
cd benchmarks/matmul
python results/export.py matmul_mm_<compiler>.data
python results/export.py matmul_mv_<compiler>.data
cd ../..
```

`<compiler> : ifx, ifort, gfortran, nvfortran`

For Coarrays:

```shell
cd benchmarks/matmul
python results/export_co.py matmul_mm_<compiler>_co.data
python results/export_co.py matmul_mv_<compiler>_co.data
python results/export_im.py matmul_mm_<compiler>_im1.data
python results/export_im.py matmul_mv_<compiler>_im1.data
cd ../..
```

`<compiler> : ifx, ifort`

## matmul (matrix-matrix)

**TODO**:
- Generate results.

### Intel Fortran Compiler (ifx)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifx_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifx_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifx_speedup.png" width="300"> |


[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mm_ifx.html) -->

### Intel Fortran Compiler Classic (ifort)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifort_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifort_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_ifort_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mm_ifort.html) -->

### GNU Fortran (gfortran)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_gfortran_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_gfortran_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_gfortran_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mm_gfortran.html) -->

### NVIDIA HPC (nvfortran)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_nvfortran_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_nvfortran_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mm_nvfortran_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mm_nvfortran.html) -->

## matmul (matrix-vector)

### Intel Fortran Compiler (ifx)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifx_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifx_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifx_speedup.png" width="300"> |


[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mv_ifx.html) -->

### Intel Fortran Compiler Classic (ifort)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifort_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifort_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_ifort_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mv_ifort.html) -->

### GNU Fortran (gfortran)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_gfortran_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_gfortran_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_gfortran_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mv_gfortran.html) -->

### NVIDIA HPC (nvfortran)

<!-- | Elapsed Time | Performance | Speedup |
|--------------|-------------|---------|
| <img alt="matmul_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_nvfortran_time.png" width="300"> | <img alt="matmul_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_nvfortran_perf.png" width="300"> | <img alt="matmul_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/benchmarks/matmul/results/matmul_mv_nvfortran_speedup.png" width="300"> |

[View detailed table](https://raw.githack.com/gha3mi/forbenchmark/main/benchmarks/matmul/results/matmul_mv_nvfortran.html) -->