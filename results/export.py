# ForBenchmark
# Seyed Ali Ghasemi

import sys
import os
current_dir = os.path.dirname(os.path.abspath(__file__))
scripts_dir = os.path.abspath(os.path.join(current_dir, './results'))
sys.path.append(scripts_dir)
import forbenchmark_plot_default as fpd
import forbenchmark_html as fh

# Read the benchmark data from the file
[benchmark_data, argi, file_path] = fpd.read_benchmark_data('results')

# Customization specific to each benchmark
# Calculate custom 'x_data'
x_data = argi.iloc[:, 0]  # Customizing x-axis data based on the benchmark requirements


# Set plot settings
fpd.set_plot_settings(fig_size=(6, 6), dpi=600, colormap='prism')

# Plot the elapsed time
fpd.plot_elapsed_time(file_path, benchmark_data, x_data,
                                     title='Demo Benchmark - Elapsed Time',
                                     xlabel='Number of Elements',
                                     ylabel='Elapsed Time [s]')

# Plot the performance
fpd.plot_performance(file_path, benchmark_data, x_data,
                                   title='Demo Benchmark - Performance',
                                   xlabel='Number of Elements',
                                   ylabel='Performance [GFLOPS]')

fpd.plot_speedup(file_path, benchmark_data, x_data,
                                   title='Demo Benchmark - Speedup',
                                   xlabel='Number of Elements',
                                   ylabel='Speedup [-]',
                                   bar_width=0.12)


fpd.plot_speedup_avg(file_path, benchmark_data, x_data, 
                                   title='Demo Benchmark - Average Speedup',
                                   xlabel='Methods',
                                   ylabel='Average Speedup [-]')

fh.generate_html(file_path, benchmark_data,
                  title='Demo Benchmark')