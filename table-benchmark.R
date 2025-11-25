# Date: 25 November 2025
# Author: Ahmet Akkoc, ahmet@zitelab.dk
# Adapted from Many Gimond (2019): https://mgimond.github.io/rug_2019_12/Index.html

# ==============================================================================
# 1. INSTALL AND LOAD PACKAGES
# ==============================================================================

# Install the necessary package for writing Excel files if you don't have it
# install.packages("writexl")

library(dplyr)
library(data.table)
library(tibble)
library(microbenchmark)
library(writexl)

# Initialize an empty list to store all benchmark results
results_list <- list() 

# ==============================================================================
# 2. DATA SETUP (As per the original tutorial)
# ==============================================================================

# Create the data.frame from mtcars (Base R)
df <- mtcars[, c(1:2, 4, 9)]
df$car <- rownames(mtcars)
rownames(df) <- NULL

# Create a tibble (Tidyverse)
tb <- as_tibble(df)

# Create a data.table
dt <- as.data.table(df)

# Set the number of iterations for the benchmark
N_TIMES <- 100 
OUTPUT_FILENAME <- "R_Benchmark_Results.xlsx"

# ==============================================================================
# 3. BENCHMARKING OPERATIONS
# ==============================================================================

# Helper function to clean and summarize microbenchmark results
clean_results <- function(bm_result, title) {
  # Convert microbenchmark object to a summary data frame
  summary_df <- summary(bm_result) %>%
    select(expr, min, mean, median) %>%
    # Format the expr column for clean table output
    rename(Operation = expr, Min_ns = min, Mean_ns = mean, Median_ns = median)
  
  # Add units to the title
  cat(paste("Completed:", title, "\n"))
  return(summary_df)
}


# --- Benchmark 1: Selecting Columns ---
bm_select <- microbenchmark(
  baseR      = df[ , c("mpg", "hp")],
  tidyverse  = select(tb, mpg, hp),
  datatable  = dt[ , .(mpg, hp)],
  times = N_TIMES
)
results_list[["1_Select_Columns"]] <- clean_results(bm_select, "Selecting Columns")


# --- Benchmark 2: Filtering Rows ---
bm_filter <- microbenchmark(
  baseR      = df[df$mpg > 20, ],
  tidyverse  = filter(tb, mpg > 20),
  datatable  = dt[mpg > 20, ],
  times = N_TIMES
)
results_list[["2_Filter_Rows"]] <- clean_results(bm_filter, "Filtering Rows")


# --- Benchmark 3: Sorting a Table ---
bm_sort <- microbenchmark(
  baseR      = df[order(df$mpg), ],
  tidyverse  = arrange(tb, mpg),
  datatable  = dt[order(mpg), ],
  times = N_TIMES
)
results_list[["3_Sorting_Table"]] <- clean_results(bm_sort, "Sorting Table")


# --- Benchmark 4: Creating a New Column (Mutation) ---
# Note: Base R operation is wrapped to ensure the column is removed afterwards.
bm_mutate <- microbenchmark(
  baseR      = { df$gpm <- 1 / df$mpg; df$gpm <- NULL },
  tidyverse  = mutate(tb, gpm = 1 / mpg),
  datatable  = dt[ , gpm := 1 / mpg], # In-place modification
  times = N_TIMES
)
results_list[["4_Create_Column"]] <- clean_results(bm_mutate, "Creating Column")

# Clean up data.table after in-place modification
dt[, gpm := NULL] 


# --- Benchmark 5: Summarizing by Group ---
bm_aggregate <- microbenchmark(
  baseR      = aggregate(hp ~ cyl, data = df, FUN = mean),
  tidyverse  = tb %>% group_by(cyl) %>% summarise(mean_hp = mean(hp), .groups = 'drop'),
  datatable  = dt[ , .(mean_hp = mean(hp)), by = cyl],
  times = N_TIMES
)
results_list[["5_Grouped_Summary"]] <- clean_results(bm_aggregate, "Grouped Summary")


# ==============================================================================
# 4. EXPORT TO EXCEL
# ==============================================================================

# The write_xlsx function takes the list of data frames and creates a sheet 
# for each element, using the element name as the sheet name.
write_xlsx(results_list, path = OUTPUT_FILENAME)

cat(paste("\n✅ Success! All benchmark results have been saved to:", OUTPUT_FILENAME, "\n"))