[![GitHub](https://img.shields.io/badge/GitHub-ForBenchmark-blue.svg?style=social&logo=github)](https://github.com/gha3mi/forbenchmark)
[![Version](https://img.shields.io/github/release/gha3mi/forbenchmark.svg)](https://github.com/gha3mi/forbenchmark/releases/latest)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/forbenchmark/)
[![License](https://img.shields.io/github/license/gha3mi/forbenchmark?color=green)](https://github.com/gha3mi/forbenchmark/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/forbenchmark/actions/workflows/CI_test.yml/badge.svg)](https://github.com/gha3mi/forbenchmark/actions/workflows/CI_test.yml)

<!-- <img alt="ForBenchmark" src="https://github.com/gha3mi/forbenchmark/raw/main/media/logo.png" width="750"> -->

**ForBenchmark**: A Fortran library for benchmarking (with support for coarrays).

ForBenchmark can measure various time metrics including elapsed wall time, CPU time, OpenMP (OMP) time, MPI time, and date_and_time. Furthermore, it can compute flops based on a provided function.

## Benchmark Results

- [dot_product](https://github.com/gha3mi/forbenchmark/tree/main/benchmarks/dot)
- [matmul](https://github.com/gha3mi/forbenchmark/tree/main/benchmarks/matmul)

This list will be updated. Feel free to add new benchmarks.

## Simple Usage

```fortran
use forbenchmark

type(benchmark) :: bench

! initialize the benchmark
call bench%init(nmarks)

! start benchmark
call bench%start_benchmark(imark, method)
! loop over nloops
do nl = 1, bench%nloops

    ! call your function or subroutine or ...

end do
! stop benchmark
call bench%stop_benchmark()

! finalize the benchmark
call bench%finalize()
```

## Procedures

```fortran
! initialize the benchmark
call bench%init(nmarks, title, filename, nloops, timer)
```

```fortran
! start benchmark
call bench%start_benchmark(imark, method, description, argi, argr)
```

```fortran
! stop benchmark
call bench%stop_benchmark(flops)
```

```fortran
! finalize the benchmark
call bench%finalize()
```

Refer to `example/demo.f90` for a complete demonstration.

## fpm dependency

To use `ForBenchmark` as a dependency in your fpm project, include the following line in your `fpm.toml` file:

```toml
[dependencies]
forbenchmark = {git="https://github.com/gha3mi/forbenchmark.git"}
```

## How to Run the Demo

**Clone the repository:**

Clone the `ForBenchmark` repository from GitHub using:

```shell
git clone https://github.com/gha3mi/forbenchmark.git
cd forbenchmark
```

**Run the demo:**

For non-coarray benchmarking:

```shell
fpm run --example demo --profile release
```

For coarray programs benchmarking using the Intel Fortran compiler:

```shell
fpm run --example demo  --profile release --compiler ifx --flag "-coarray -coarray-num-images=4 -DUSE_COARRAY"
```

```shell
fpm run --example demo  --profile release --compiler ifort --flag "-coarray -coarray-num-images=4 -DUSE_COARRAY"
```

After execution, the results will be displayed in the terminal and stored in the 'results' folder. See `results/demo.data`, `results/demo_im1.data` for instance.

**Visualizing and exporting demo results:**

To generate visual representations and export benchmarking metrics, execute the following commands in Python. Specify the relevant data file as an argument:

```shell
python results/export.py demo_<compiler>.data
```

This command will produce graphical representations and an HTML table showcasing benchmarking metrics:

| Elapsed Time | Performance |
|--------------|-------------|
| <img alt="demo_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_gfortran_time.png" width="300"> | <img alt="demo_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_gfortran_perf.png" width="300"> |

| Speedup | Average Weighted Speedup |
|---------|--------------------------|
| <img alt="demo_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_gfortran_speedup.png" width="300"> | <img alt="demo_speedup" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_gfortran_speedup_avg.png" width="300"> |

For Coarray benchmarks, execute:

```shell
python results/export_co.py demo_co.data
python results/export_im.py demo_im1.data
```
The outputs can be found in the `results` directory.

## TODO
- [ ] Add [sphinx-gallery](https://sphinx-gallery.github.io/stable/auto_examples/.index.html)
- [ ] Add settings to set units.
- [ ] Add benchmarks for matmul_co, dot_co, ...
- [ ] Add MPI module.

## API documentation

The most up-to-date API documentation for the main branch is available
[here](https://gha3mi.github.io/forbenchmark/).
To generate the API documentation for `ForBenchmark` using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing

Contributions to `ForBenchmark` are welcome!
If you find any issues or would like to suggest improvements, please open an issue.
