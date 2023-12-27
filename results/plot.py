# ForBenchmark
# Seyed Ali Ghasemi

import forbenchmark_plot_default as fpd

# Read the benchmark data from the file
[benchmark_data, argi, file_path] = fpd.read_benchmark_data('results')

# Customization specific to each benchmark
# Calculate custom 'x_data'
x_data = argi.iloc[:, 0]  # Customizing x-axis data based on the benchmark requirements


# Set plot settings
fpd.set_plot_settings(fig_size=(7, 6), dpi=600, colormap='prism')

# Plot the elapsed time
fpd.plot_elapsed_time(file_path, benchmark_data, x_data,
                                     title='Demo Benchmark - Elapsed Time',
                                     xlabel='Order of Matrix',
                                     ylabel='Elapsed Time [s]')

# Plot the performance
fpd.plot_performance(file_path, benchmark_data, x_data,
                                   title='Demo Benchmark - Performance',
                                   xlabel='Order of Matrix',
                                   ylabel='Performance [GFLOPS]')