# ForBenchmark
# Seyed Ali Ghasemi

import sys
import os
current_dir = os.path.dirname(os.path.abspath(__file__))
scripts_dir = os.path.abspath(os.path.join(current_dir, '../../scripts'))
sys.path.append(scripts_dir)
import forbenchmark_plot_coarray as fpd
import forbenchmark_html as fh

# Read the benchmark data from the file
[benchmark_data, argi, file_path] = fpd.read_benchmark_data('results')

# Customization specific to each benchmark
# Calculate custom 'x_data'
x_data = argi.iloc[:, 0]  # Customizing x-axis data based on the benchmark requirements


# Set plot settings
fpd.set_plot_settings(fig_size=(7, 6), dpi=600, colormap='prism')

# Plot the elapsed time
fpd.plot_elapsed_time_max(file_path, benchmark_data, x_data,
                                     title='dot_product Benchmark - Max. Elapsed Time',
                                     xlabel='Number of Elements',
                                     ylabel='Max. Elapsed Time [s]')

# Plot the elapsed time
fpd.plot_elapsed_time_avg(file_path, benchmark_data, x_data,
                                     title='dot_product Benchmark - Avg. Elapsed Time',
                                     xlabel='Number of Elements',
                                     ylabel='Avg. Elapsed Time [s]')

# Plot the performance
fpd.plot_performance_tot(file_path, benchmark_data, x_data,
                                   title='dot_product Benchmark - Performance',
                                   xlabel='Number of Elements',
                                   ylabel='Performance [GFLOPS]')

# Plot the performance
fpd.plot_speedup_max(file_path, benchmark_data,
                                   title='dot_product Benchmark - Max. Speedup',
                                   xlabel='Number of Elements',
                                   ylabel='Max. Speedup [-]')

fh.generate_html(file_path, benchmark_data,
                  title='dot_product Benchmark')