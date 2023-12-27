# ForBenchmark
# Seyed Ali Ghasemi

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
import os
import sys



# Global variables for plot configurations
FIG_SIZE = None  # Tuple containing figure size (width, height) in inches
DPI = None  # Dots per inch for saving figures
COLORMAP = None  # Colormap for differentiating plot colors



def set_plot_settings(fig_size, dpi, colormap):
    """
    Set global constants for plot configurations.

    Args:
        fig_size (tuple): Tuple containing figure size (width, height) in inches
        dpi (int): Dots per inch for saving figures
        colormap (str or matplotlib colormap): Colormap for differentiating plot colors
    """
    global FIG_SIZE, DPI, COLORMAP
    FIG_SIZE = fig_size
    DPI = dpi
    COLORMAP = colormap



def read_benchmark_data(directory):
    """
    Read benchmark data from a file.

    Args:
        directory (str): Directory containing the benchmark data file

    Returns:
        benchmark_data (DataFrame): Benchmark data in a Pandas DataFrame
        arg_data (DataFrame): Subset of benchmark data containing argument values
        file_path (str): Full path of the benchmark data file
    """

    # Checking command-line arguments
    if len(sys.argv) != 2:
        print("Usage: python plot.py <file_name>")
        sys.exit(1)

    # Fetching file name from command-line argument
    file_name = sys.argv[1]  # Get the file name from command-line argument

    file_path = os.path.join(directory, file_name)

    with open(file_path, 'r') as file:
        # Skip unnecessary lines at the beginning and end of the file
        lines = file.readlines()[12:-1]

    # Split lines and create a DataFrame, converting values to numeric where possible
    data_rows = [line.split() for line in lines]

    # Infer the number of arguments based on the length of the first line of data
    num_arguments = len(data_rows[0]) - 4  # Assuming the first 4 columns are fixed

    # Define column names based on the inferred number of arguments
    columns = ['method', 'elapsed_time', 'gflops', 'nloops'] + [f'arg{i}' for i in range(1, num_arguments + 1)]

    # Convert data to a DataFrame and apply numeric conversion to appropriate columns
    benchmark_data = pd.DataFrame(data_rows, columns=columns).apply(pd.to_numeric, errors='ignore')

    # Subset of benchmark data containing argument values
    arg_data = benchmark_data.iloc[:, 4:]

    return benchmark_data, arg_data, file_path



def plot_benchmark_data(file_path, benchmark_data, x_data, title, xlabel, ylabel, data_column):
    """
    Plot benchmark data for a specific column.

    Args:
        benchmark_data (DataFrame): Benchmark data in a Pandas DataFrame
        x_data (Series or array-like): Data for the x-axis
        file_path (str): File path for saving the plot
        title (str): Title of the plot
        xlabel (str): Label for the x-axis
        ylabel (str): Label for the y-axis
        data_column (str): Column name from benchmark data to be plotted

    Returns:
        plt (matplotlib.pyplot): Matplotlib pyplot object for the generated plot
    """

    # Set 'x_data' column in the benchmark dataset
    benchmark_data['x_data'] = x_data

    unique_methods = benchmark_data['method'].unique()
    markers = {method: plt.Line2D.filled_markers[idx % len(plt.Line2D.filled_markers)]
               for idx, method in enumerate(unique_methods)}
    num_unique_methods = len(unique_methods)
    cmap = plt.cm.get_cmap(COLORMAP, num_unique_methods)
    colors = {method: mpl.colors.rgb2hex(cmap(i)[:3]) for i, method in enumerate(unique_methods)}

    plt.figure(figsize=FIG_SIZE)
    plt.title(title)

    # Plot each method's data with different markers and colors
    for method, group in benchmark_data.groupby('method'):
        plt.plot(group['x_data'], group[data_column], label=method, marker=markers[method], color=colors[method])

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.legend(loc='upper left', bbox_to_anchor=(1, 1), ncol=1)
    plt.gca().xaxis.set_major_formatter(plt.ScalarFormatter(useMathText=True))
    plt.gca().ticklabel_format(axis='x', style='sci', scilimits=(-2, 2))
    plt.grid(True)
    plt.tight_layout()

    # Add 'Generated using ForBenchmark' text to the bottom right corner of the figure
    plt.figtext(0.98, 0.005, 'ForBenchmark', horizontalalignment='right',
                verticalalignment='bottom', fontsize=8, color='gray')

    if data_column == 'elapsed_time':
        output_filename = f'{os.path.splitext(file_path)[0]}_time.png'
    elif data_column == 'gflops':
        output_filename = f'{os.path.splitext(file_path)[0]}_perf.png'
    plt.savefig(output_filename, dpi=DPI)

    return plt



def plot_elapsed_time(file_path, benchmark_data, x_data, title, xlabel, ylabel):
    """
    Plot elapsed time data from benchmark data.

    Args:
        benchmark_data (DataFrame): Benchmark data in a Pandas DataFrame
        x_data (Series or array-like): Data for the x-axis
        file_path (str): File path for saving the plot
        title (str): Title of the plot
        xlabel (str): Label for the x-axis
        ylabel (str): Label for the y-axis

    Returns:
        plt (matplotlib.pyplot): Matplotlib pyplot object for the generated plot
    """
    return plot_benchmark_data(file_path, benchmark_data, x_data, title, xlabel, ylabel, 'elapsed_time')



def plot_performance(file_path, benchmark_data, x_data, title, xlabel, ylabel):
    """
    Plot performance (GFLOPS) data from benchmark data.

    Args:
        benchmark_data (DataFrame): Benchmark data in a Pandas DataFrame
        x_data (Series or array-like): Data for the x-axis
        file_path (str): File path for saving the plot
        title (str): Title of the plot
        xlabel (str): Label for the x-axis
        ylabel (str): Label for the y-axis

    Returns:
        plt (matplotlib.pyplot): Matplotlib pyplot object for the generated plot
    """
    return plot_benchmark_data(file_path, benchmark_data, x_data, title, xlabel, ylabel, 'gflops')
