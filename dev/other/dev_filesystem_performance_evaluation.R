
source("dev/dev_filesystem_test_functions.R")

test_results <- run_example_tests(
  test_configs = list(
    "small" = list(rows = 100, cols = 100, char_cols = 2),
    "medium" = list(rows = 1000, cols = 100, char_cols = 5),
    "large" = list(rows = 10000, cols = 100, char_cols = 10),
    "extra_large" = list(rows = 100000, cols = 100, char_cols = 20),
    "big_data" = list(rows = 1e6, cols = 100, char_cols = 50)
  )
)

# Access the summary data.table for plotting
summary_dt <- test_results$summary_dt
results <- test_results$results

plot_summary_dt(summary_dt)
