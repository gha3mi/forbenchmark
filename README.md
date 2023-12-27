[![GitHub](https://img.shields.io/badge/GitHub-ForBenchmark-blue.svg?style=social&logo=github)](https://github.com/gha3mi/forbenchmark)
[![Version](https://img.shields.io/github/release/gha3mi/forbenchmark.svg)](https://github.com/gha3mi/forbenchmark/releases/latest)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/forbenchmark/)
[![License](https://img.shields.io/github/license/gha3mi/forbenchmark?color=green)](https://github.com/gha3mi/forbenchmark/blob/main/LICENSE)
<!-- [![Build](https://github.com/gha3mi/forbenchmark/actions/workflows/CI_test.yml/badge.svg)](https://github.com/gha3mi/forbenchmark/actions/workflows/CI_test.yml) -->

<!-- <img alt="ForBenchmark" src="https://github.com/gha3mi/forbenchmark/raw/main/media/logo.png" width="750"> -->

**ForBenchmark**: A Fortran library for benchmarking (with support for coarrays).

## Usage

```fortran
use forbenchmark

type(benchmark) :: bench

! initialize the benchmark
call bench%init()

! start benchmark
call bench%start_benchmark(method)
! loop over nloops
do nl = 1, bench%nloops

    ! call your function or subroutine or ...

end do
! stop benchmark
call bench%stop_benchmark()

! finalize the benchmark
call bench%finalize()
```

See `example/demo.f90` for a complete example.

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

For serial benchmarking:

```shell
fpm run --example demo
```

For coarray programs benchmarking using the Intel Fortran compiler:

```shell
fpm run --example demo --compiler ifx --flag "-coarray -coarray-num-images=4 -DUSE_COARRAY"
```

```shell
fpm run --example demo --compiler ifort --flag "-coarray -coarray-num-images=4 -DUSE_COARRAY"
```

After execution, the results will be displayed in the terminal and stored in the 'results' folder. See `results/demo.data`, `results/demo_im1.data` for instance.

**Visualizing demo results:**

To visualize benchmarking results, execute the following command in Python, passing the relevant data file as an argument:

`python results/plot.py demo.data`

The output includes graphical representations of benchmarking metrics:

<img alt="demo_elapsed_time" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_time.png" width="250"> <img alt="demo_performance" src="https://github.com/gha3mi/forbenchmark/raw/main/results/demo_perf.png" width="250">


## TODO
- [ ] Add MPI module.
- [ ] Add plot_co.py similar to plot.py.
- [ ] Add CI_test.yml

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
