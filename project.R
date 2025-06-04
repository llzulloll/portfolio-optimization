################################################################################
################################################################################
#
#                 QUANTITATIVE INVESTING 101 PROJECT R FILE
#
# ------------------------------------------------------------------------------
# Use this file to backtest your team's set of factors (Growth, Value,
# Quality, or Sentiment) and create your team's factor stock selection model.
#
# Most of the code to perform the backtests has been provided to you. However,
# you will be required to create the factor distributions, the summary table,
# the time series plots of the factor rank correlations.
#
# DON'T BE INTIMIDATED! You can do this, just put in a little bit of effort to
# understand the code provided and do some light googling to figure out how to
# create plots. ChatGPT or any other Large Language Model is fair game as well,
# use them as much as you want.
#
# We use the data.table constantly at work, so we are having you all use it
# for this project. An introduction to data.table can be found here:
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# The section that will be most useful for the project is the "Aggregations"
# section to create the summary table.
#
# You'll be interpreting the results and stating your conclusions in your
# final submission along with the distribution plots, summary table, and
# time series correlation plots produced from this script.
#
# After the first lecture, you'll return to this script to create and backtest
# your multi-factor stock selection model and create a summary table of it's
# performance as well.
################################################################################
################################################################################

################################################################################
# SETUP
################################################################################

# Library required packages
library(data.table)

# Read in dataset
dataset <- as.data.table(read.csv("dataset.csv"))

# Convert date column to a date type
## If this code does not work, look at the format of the date column in the
## dataset.csv file. You may need to change the format string to match the
## way the date is formatted in your `dataset` object
dataset[, date := as.Date(date, tryFormats = c("%m/%d/%y", "%Y-%m-%d"))]

print(dataset)

################################################################################
################################################################################
#
#                             FACTOR BACKTESTS
#
################################################################################
################################################################################

################################################################################
# DISTRIBUTION OF FACTORS
# ------------------------------------------------------------------------------
# Create a histogram for each factor over the entire sample
#
# You can use the hist() function for your histograms:
# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/hist
#
# To create a histogram of a specific column in a dataset, we use the dataset
# name combined with a $ and the column name:
# hist(dataset$column)
#
# *** Include the plots in your final submission document ***
# USE THE 'Export' BUTTON UNDER THE 'Plots' PANE IN RSTUDIO TO SAVE THE PLOT
################################################################################

# *** WRITE YOUR CODE TO CREATE THE PLOTS HERE, COMMENT OUT CODE BELOW ***
# Hint: Try using the following prompt in ChatGPT:

# You are a world class R developer. I have a data.table object called
# dataset with the following column names:
# date, company_id, {insert your factor column names}
# I need to create a nice looking distribution plot for each of my factors.
# Please provide R code to create these distributions.

# Factor 1 (Book/Price, ROE, Sales Growth, or Up/Down Ratio)
# dist_factor_1 <-

# Factor 2 (FCF Yield, Debt/IC, or 3M Return Momentum)
# dist_factor_2 <-

# Factor 3 (NTM EPS/Price, Return Volatility, or 12M Return Momentum)
# dist_factor_3 <-
library(ggplot2)

# Plot for Book-to-Price Distribution
ggplot(dataset, aes(x = book_to_price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Book-to-Price Distribution", x = "Book-to-Price")

# Plot for Free Cash Flow (FCF) Yield Distribution
ggplot(dataset, aes(x = fcf_yield)) +
  geom_histogram(fill = "salmon", color = "black", bins = 30) +
  labs(title = "Free Cash Flow Yield Distribution", x = "FCF Yield")

# Plot for NTM EPS/Price Distribution
ggplot(dataset, aes(x = eps_to_price_ntm)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "NTM EPS/Price Distribution", x = "NTM EPS/Price")

################################################################################
# COMPUTE MEDIAN 3M AND 12M RETURN EACH MONTH
# ------------------------------------------------------------------------------
# We use the median stock return as the "benchmark" for our factor backtests.
# A stock index is usually market-cap weighted. This will cause the returns
# of the index to be heavily influenced by the largest stocks. To negate this
# effect, we use the median return in the universe as our benchmark since this
# is the return of the "typical" stock. 50% of the returns in the universe
# were higher than this return and 50% of the returns in the universe were
# lower.
#
# In data.table, you create a new column using ":=". You can use a
# function, combine other columns together, and a lot more:
#   dataset[, newColumn := someFunction(oldColumn)]
#   dataset[, column3 := column1 + column2]
################################################################################

# Calculate the median Forward 3 Month return for each month
dataset[, benchmark_return_3m := median(forward_return_3m), by = "date"]

# Calculate the median Forward 12 Month return for each month
dataset[, benchmark_return_12m := median(forward_return_12m), by = "date"]

dataset
# Don't forget to take a look at the dataset now! Type 'dataset' into your
# R console without the quotes or type View(dataset) to see the whole thing.
# What do you notice about the benchmark returns? How come they're the same
# for every stock each month?

# Also try calculating the median Forward 3 and 12 month return for the
# most recent date in Excel. Do you get the same number as the last one
# in your dataset?


################################################################################
# COMPUTE THE RELATIVE RETURN FOR EACH STOCK (STOCK RETURN - BENCHMARK RETURN)
# ------------------------------------------------------------------------------
# Here we are computing the relative return of each stock compared to the
# "benchmark" or "typical" stock as dicussed above.
################################################################################

# Calculate the relative 3 month return for every stock
dataset[, relative_return_3m := forward_return_3m - benchmark_return_3m]

# Calculate teh relative 12 month return for every stock
dataset[, relative_return_12m := forward_return_12m - benchmark_return_12m]

# Take a look at the dataset at this point. Now we have 2 new relative return
# columns for each stock.
dataset

################################################################################
# FUNCTION TO COMPUTE DECILES FOR EACH FACTOR
# ------------------------------------------------------------------------------
# Use the given function compute_tiles(factor, num_tiles = 10) to decile each
# factor. You'll need to do the analysis on each factor one at a time.
################################################################################

# Compute quantiles for each factor value. The number of quantiles is determined
# by the `num_tiles` argument.

# A more intuitive way of thinking about this function is it sorts stocks by
# their factor values (order all stocks based on their Book to Price ratio).
# Then it creates equal sized groups. The number of groups equals `num_tiles`.

compute_tiles <- function(factor, num_tiles = 10) {
  # Save number of observations
  num_observations <- sum(!is.na(factor))

  # Check to make sure we have observations
  if (num_observations == 0L) {
    rep(NA_integer_, length(factor))
  } else {
    # Compute rank order
    rank_order <- rank(
      x = factor,
      na.last = "keep",
      ties.method = "first"
    )

    # Compute percentile
    percentile <- (rank_order - 1) / num_observations

    # Scale up by num_tiles
    scaled_percentile <- percentile * num_tiles

    # Add 1 so lowest tile is 1 instaed of 0
    scaled_percentile <- scaled_percentile + 1

    # Turn tile into integers
    tiles <- as.integer(floor(scaled_percentile))

    # Return tiles
    return(tiles)
  }
}

compute_tiles

################################################################################
# THE REST OF THE FACTOR BACKTEST ANALYSIS MUST BE DONE ONE FACTOR AT A TIME
# START WITH BookToPrice, CREATE THE PLOTS AND SUMMARY TABLES DESCRIBED BELOW
# AND SAVE THEM FOR YOUR FINAL SUBMISSION.
#
# ONCE YOU GET YOUR PLOTS AND SUMMARY TABLE CORRECTLY CREATED FOR YOUR FIRST
# FACTOR, YOU WILL RE-RUN EVERYTHING FOR YOUR SECOND AND THIRD FACTOR, RESAVING
# THE PLOTS AND SUMMARY TABLE.
#
# YOU CAN CHANGE THE FACTOR BEING ANALYZED BY CHANGING THE current_factor VALUE
# BELOW.
################################################################################

# Set the factor to be analyzed here
# book_to_price
# fcf_yield
# eps_to_price_ntm
current_factor <- "book_to_price"


################################################################################
# COMPUTE TILE BY DATE
################################################################################

# This will create those groups we mentioned earlier for every month
dataset_with_tiles <- copy(dataset)[,
  tile := compute_tiles(factor = get(current_factor), num_tiles = 10),
  by = "date"
]

# Make sure you take a look at the dataset at this point to see the tiles added
print(dataset_with_tiles)

################################################################################
# COMPUTE MEDIAN RELATIVE RETURN AND HIT RATE FOR EACH TILE ON EACH DATE
# ------------------------------------------------------------------------------
# Here we are computing the return and hit rate for each tile for each month.
# We use the median here again to negate the effects of large stocks or skewness
# in the returns. The hit rate as the percent of stocks in the tile that
# outperform the benchmark. A stock that has a higher return than the
# benchmark is said to have "outperformed".
#
# Remember our "benchmark" is the median stock return in our investment
# universe.
# Check out the Introduction to data.table given at the top of this file for
# more information on the .N part of the code chunk below.
################################################################################

# Compute the median forward return and hit rate for each tile on each date
# This will tell us how the "typical" stock in each group did relative
# to the "typical" stock in our investment universe

tile_performance <- dataset_with_tiles[,
  .(
    tile_median_return_3m = median(relative_return_3m),
    tile_median_return_12m = median(relative_return_12m),
    tile_hit_rate_3m = sum(relative_return_3m > 0) / .N,
    tile_hit_rate_12m = sum(relative_return_12m > 0) / .N
  ),
  by = c("date", "tile")
][order(date, tile)]

print(tile_performance)
################################################################################
# COMPUTE LONG - SHORT RETURN, SAVE AS TILE = 11
################################################################################

# Function to compute Long - Short Returns and Hit Rates. Don't worry about
# how this code works other than it computes the return we would get if
# we bought the stocks in the highest group and sold short the stocks in
# the lowest group every month.

compute_long_short <- function(dataset) {
  # 'dataset' is a dataset containing the median return and hit rate for each
  # tile on each date.
  long <- dataset[, max(tile)]
  short <- dataset[, min(tile)]
  dataset[,
    .(
      tile = c(
        tile,
        tile[tile == long] + tile[tile == short]
      ),
      tile_median_return_3m = c(
        tile_median_return_3m,
        tile_median_return_3m[tile == long] - tile_median_return_3m[tile == short]
      ),
      tile_hit_rate_3m = c(
        tile_hit_rate_3m,
        (tile_hit_rate_3m[tile == long] - tile_hit_rate_3m[tile == short]) / 2 + 0.5
      ),
      tile_median_return_12m = c(
        tile_median_return_12m,
        tile_median_return_12m[tile == long] - tile_median_return_12m[tile == short]
      ),
      tile_hit_rate_12m = c(
        tile_hit_rate_12m,
        (tile_hit_rate_12m[tile == long] - tile_hit_rate_12m[tile == short]) / 2 + 0.5
      )
    ),
    by = "date"
  ]
}

# Compute Long - Short Returns and Hit Rates
tile_performance <- compute_long_short(tile_performance)

print(tile_performance)
################################################################################
# CREATE SUMMARY TABLE OF THE 3MO AND 12MO RETURNS AND HIT RATES FOR EACH TILE
# ------------------------------------------------------------------------------
# Take the average 3m return, average 12m return, average 3m hit rate, and
# average 12m hit rate by tile.
#
# Check out the "Aggregations" section of the Introduction to data.table page:
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
#
# You can use the code chunk where we compute tile_performance as a guide.
# What's the difference between what we wanted to do there versus now? Do we
# want to compute the average return and hit rate by date and tile? Or just by
# date? Or just by tile?
#
# *Take a screenshot of your summary table to include in your final submission*
################################################################################

# *** WRITE YOUR CODE TO CREATE THE SUMMARY TABLE HERE, COMMENT OUT CODE BELOW ***
# factor_summary <-

# Compute the average 3-month and 12-month returns and hit rates for each tile
factor_summary <- tile_performance[, .(
  avg_3m_return = mean(tile_median_return_3m, na.rm = TRUE),
  avg_12m_return = mean(tile_median_return_12m, na.rm = TRUE),
  avg_3m_hit_rate = mean(tile_hit_rate_3m, na.rm = TRUE),
  avg_12m_hit_rate = mean(tile_hit_rate_12m, na.rm = TRUE)
), by = tile]

# Print the summary table to verify
print(factor_summary)






# Print the summary table
# print(factor_summary)
# ################################################################################
# # FACTOR RANK CORRELATIONS
# # ------------------------------------------------------------------------------
# # Now we will calculate the percentile ranking for each factor to understand
# # their relationship with one another. We want to look at the correlation of the
# # percentile rankings between each factor pair. This will help us understand
# # if one two factors produce the same stocks to buy and sell.
# #
# # Use the function compute_percentile below and save the percentile ranks for
# # each factor. Call the columns 'rank_factor1', 'rank_factor2', and
# # 'rank_factor3' (you should replace factor1, factor2, and factor3 with
# # your actual factor names, e.g. rank_book_to_price).
# #
# # You can use the cor() function to compute the correlations on each date
# # similar to how we compute the median return for the investment universe
# # on each date. This is called the cross sectional rank correlation.
# # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor
# #
# # Finally, plot the correlation for each pair over time. Use the plot()
# # function to create the plots.
# # https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/plot
# #
# # * Include your correlations in your final submission with an interpretation
# # of what they mean. Think about how this will help or hurt the performance
# # of our stock selection model we will create later. We'll go over this more
# # in the second lecture. *
# ################################################################################
# 
# # Function to compute the percentile values for each factor
# # We need to compute the percentile values to compute rank correlations and
# # later to create a weighted average ranking for our stock selection model.
 compute_percentiles <- function(factor, flip = FALSE) {
   # Save number of observations
   num_observations <- sum(!is.na(factor))
   # Check to make sure we have observations
   if (num_observations == 0L) {
     rep(NA_integer_, length(factor))
   } else {
     # Compute rank order
     rank_order <- rank(
       x = factor,
       na.last = "keep",
       ties.method = "first"
     )
     # Compute percentile
     percentile <- (rank_order - 1) / num_observations
     # Flip to make higher worse if required
     if (flip) percentile <- 1 - percentile
     # Return percentiles
     return(percentile)
  }
}


# Compute percentile ranks for each factor and store them in new columns
percentiles <- copy(dataset)[, .(
  rank_book_to_price = compute_percentiles(book_to_price),
  rank_fcf_yield = compute_percentiles(fcf_yield),
  rank_eps_to_price_ntm = compute_percentiles(eps_to_price_ntm)
), by = date]

# Compute correlations between the factor percentile ranks on each date
correlations <- percentiles[, .(
  cor_book_to_price_fcf_yield = cor(rank_book_to_price, rank_fcf_yield, use = "complete.obs"),
  cor_book_to_price_eps_to_price_ntm = cor(rank_book_to_price, rank_eps_to_price_ntm, use = "complete.obs"),
  cor_fcf_yield_eps_to_price_ntm = cor(rank_fcf_yield, rank_eps_to_price_ntm, use = "complete.obs")
), by = date]

# Print the correlations to check the output
print(correlations)

# Plot correlation between book_to_price and fcf_yield over time
plot(correlations$date, correlations$cor_book_to_price_fcf_yield,
     type = "l", col = "blue", xlab = "Date", ylab = "Correlation",
     main = "Correlation between Book-to-Price and FCF Yield")

# Plot correlation between book_to_price and eps_to_price_ntm over time
plot(correlations$date, correlations$cor_book_to_price_eps_to_price_ntm,
     type = "l", col = "red", xlab = "Date", ylab = "Correlation",
     main = "Correlation between Book-to-Price and EPS to Price (NTM)")

# Plot correlation between fcf_yield and eps_to_price_ntm over time
plot(correlations$date, correlations$cor_fcf_yield_eps_to_price_ntm,
     type = "l", col = "green", xlab = "Date", ylab = "Correlation",
     main = "Correlation between FCF Yield and EPS to Price (NTM)")

################################################################################
# EXTRA CREDIT: COMPUTE AND PLOT INFORMATION COEFFICENTS (IC)
# ------------------------------------------------------------------------------
# Use the compute_percentiles function above and the explanation of ICs in the
# project pdf to compute and plot the IC for each factor over time. There is
# less guidance for this part since it's extra credit.
################################################################################

# *** WRITE YOUR CODE HERE ***
# Load necessary packages
install.packages("zoo")
library(zoo)

# Compute 12-period moving averages for each IC column, ensuring trailing NAs are handled
ic_results[, `:=`(
  ic_book_to_price_3m_ma = rollmean(ic_book_to_price_3m, 12, fill = NA, align = "right", na.rm = TRUE),
  ic_fcf_yield_3m_ma = rollmean(ic_fcf_yield_3m, 12, fill = NA, align = "right", na.rm = TRUE),
  ic_eps_to_price_ntm_3m_ma = rollmean(ic_eps_to_price_ntm_3m, 12, fill = NA, align = "right", na.rm = TRUE),
  ic_book_to_price_12m_ma = rollmean(ic_book_to_price_12m, 12, fill = NA, align = "right", na.rm = TRUE),
  ic_fcf_yield_12m_ma = rollmean(ic_fcf_yield_12m, 12, fill = NA, align = "right", na.rm = TRUE),
  ic_eps_to_price_ntm_12m_ma = rollmean(ic_eps_to_price_ntm_12m, 12, fill = NA, align = "right", na.rm = TRUE)
)]

# Function to create a bar plot with an overlayed line plot
plot_ic_with_ma <- function(ic_column, ma_column, title) {
  # Define positions for each bar to align with line plot
  bar_positions <- barplot(ic_column, main = title, ylim = c(-0.2, .4), col = "blue", 
                           border = NA, ylab = "Information Coefficient (IC)")
  
  # Overlay line plot for moving average, ensuring alignment with bar positions
  lines(bar_positions, ma_column, type = "l", col = "black", lwd = 2)
}

# Plot each IC with moving average overlay
par(mfrow = c(3, 2))  # Arrange plots in a 3x2 grid

plot_ic_with_ma(ic_results$ic_book_to_price_3m, ic_results$ic_book_to_price_3m_ma, "Book to Price IC (3M)")
plot_ic_with_ma(ic_results$ic_book_to_price_12m, ic_results$ic_book_to_price_12m_ma, "Book to Price IC (12M)")

plot_ic_with_ma(ic_results$ic_fcf_yield_3m, ic_results$ic_fcf_yield_3m_ma, "FCF Yield IC (3M)")
plot_ic_with_ma(ic_results$ic_fcf_yield_12m, ic_results$ic_fcf_yield_12m_ma, "FCF Yield IC (12M)")

plot_ic_with_ma(ic_results$ic_eps_to_price_ntm_3m, ic_results$ic_eps_to_price_ntm_3m_ma, "EPS to Price IC (3M)")
plot_ic_with_ma(ic_results$ic_eps_to_price_ntm_12m, ic_results$ic_eps_to_price_ntm_12m_ma, "EPS to Price IC (12M)")


################################################################################
################################################################################
#
#        CREATE A MULTI-FACTOR STOCK SELECTION MODEL TO MANAGE A FACTOR
#              PORTFOLIO WITH A 12 MONTH AVERAGE HOLDING PERIOD
#
################################################################################
################################################################################

################################################################################
# DESCRIPTION
# ------------------------------------------------------------------------------
# Every group will create a stock selection model to pick stocks based on
# their factor type. For example, the Value team will use their value
# factors to create a Value Portfolio. You are basically creating a new
# "factor" that is a combinations of the factors we have backtested already.
# Then we backtest that new factor to see how our "model" performs. You will
# follow the same exact process that we used in the factor backtests above.
# Copy and paste the code as necessary under each prompt below.
################################################################################

# optimize weight for highest return 
# # Install and load the GenSA package for simulated annealing
# install.packages("GenSA")
# library(GenSA)
# 
# # Define bounds for weights
# lower_bounds <- c(0, 0, 0)
# upper_bounds <- c(1, 1, 1)
# 
# # Optimize weights using GenSA
# optimized_weights <- GenSA(
#   par = c(1/3, 1/3, 1/3),  # Initial weights
#   fn = objective_function, # Objective function
#   lower = lower_bounds,    # Lower bounds
#   upper = upper_bounds,    # Upper bounds
#   control = list(maxit = 1000)  # Maximum number of iterations
# )
# 
# Normalize weights to sum to 1
# optimized_weights$par / sum(optimized_weights$par)
# 0.1672250 0.6804372 0.1523377




# Compute the percentile ranks for all 12 factors
dataset[,
        `:=`(
          PercBookToPrice = compute_percentiles(book_to_price),
          PercFCFYield = compute_percentiles(fcf_yield),
          PercEPStoPriceNTM = compute_percentiles(eps_to_price_ntm),
          PercROE = compute_percentiles(roe),
          PercDebtToIC = compute_percentiles(debt_to_ic),
          PercReturnVol = compute_percentiles(return_vol, flip = TRUE),  # Flip because lower volatility is better
          PercSalesGrowth = compute_percentiles(sales_growth),
          PercEPSGrowth = compute_percentiles(eps_growth),
          PercEPSTrend = compute_percentiles(eps_trend),
          PercUpDownRevisions = compute_percentiles(up_down_revisions),
          PercMomentum3M = compute_percentiles(momentum_3m),
          PercMomentum12M = compute_percentiles(momentum_12m)
        ),
        by = "date"
]








# Install and load necessary packages
install.packages("randomForest")
library(randomForest)

# Prepare the data
factor_data <- dataset[, .(
  PercBookToPrice,
  PercFCFYield,
  PercEPStoPriceNTM
)]
response <- dataset$forward_return_12m

# Remove rows with missing values
complete_cases <- complete.cases(factor_data, response)
factor_data <- factor_data[complete_cases, ]
response <- response[complete_cases]

# Fit the Random Forest model
rf_model <- randomForest(
  x = factor_data,
  y = response,
  ntree = 500,
  importance = TRUE
)

# Get variable importance
importance_values <- importance(rf_model, type = 1)
print(importance_values)

# Derive weights from importance values
optimized_weights <- importance_values / sum(importance_values)

# Display the optimized weights
print(optimized_weights)

# Recalculate ModelScore with optimized weights
dataset[, ModelScore := (PercBookToPrice * optimized_weights[1]) +
          (PercFCFYield * optimized_weights[2]) +
          (PercEPStoPriceNTM * optimized_weights[3])]


  current_factor <- "ModelScore"
  
  
  # COMPUTE SUMMARY TABLES
  
  dataset_with_tiles <- copy(dataset)[,
                                      tile := compute_tiles(factor = get(current_factor), num_tiles = 10),
                                      by = "date"
  ]
  
  tile_performance <- dataset_with_tiles[,
                                         .(
                                           tile_median_return_12m = median(relative_return_12m),
                                           tile_hit_rate_12m = sum(relative_return_12m > 0) / .N
                                         ),
                                         by = c("date", "tile")
  ][order(date, tile)]
  
  compute_long_short <- function(dataset) {
    long <- dataset[, max(tile)]
    short <- dataset[, min(tile)]
    dataset[,
            .(
              tile = c(
                tile,
                tile[tile == long] + tile[tile == short]
              ),
              tile_median_return_12m = c(
                tile_median_return_12m,
                tile_median_return_12m[tile == long] - tile_median_return_12m[tile == short]
              ),
              tile_hit_rate_12m = c(
                tile_hit_rate_12m,
                (tile_hit_rate_12m[tile == long] - tile_hit_rate_12m[tile == short]) / 2 + 0.5
              )
            ),
            by = "date"
    ]
  }
  
  
  tile_performance <- compute_long_short(tile_performance)
  
    factor_summary <- tile_performance[, .(
    avg_12m_return = mean(tile_median_return_12m, na.rm = TRUE),
    avg_12m_hit_rate = mean(tile_hit_rate_12m, na.rm = TRUE)
  ), by = tile]
  
  print(factor_summary)
  
  # 

  
  
  
  
  
  
  # ALL 12 Factors 
  # Install and load necessary packages
  # install.packages("randomForest")  # Uncomment if not already installed
  library(randomForest)
  library(data.table)
  library(dplyr)
  
  # Ensure your dataset is a data.table
  dataset <- as.data.table(dataset)
  
  # Prepare the data with all 12 factors
  factor_data <- dataset[, .(
    PercBookToPrice,
    PercFCFYield,
    PercEPStoPriceNTM,
    PercROE,
    PercDebtToIC,
    PercReturnVol,
    PercSalesGrowth,
    PercEPSGrowth,
    PercEPSTrend,
    PercUpDownRevisions,
    PercMomentum3M,
    PercMomentum12M
  )]
  
  # Prepare the response variable
  response <- dataset$forward_return_12m
  
  # Remove rows with missing values
  complete_cases <- complete.cases(factor_data, response)
  factor_data <- factor_data[complete_cases, ]
  response <- response[complete_cases]
  
  # Fit the Random Forest model using IncNodePurity
  set.seed(123)  # Set seed for reproducibility
  rf_model <- randomForest(
    x = factor_data,
    y = response,
    ntree = 500,
    importance = TRUE
  )
  
  # Get variable importance using IncNodePurity
  importance_values <- importance(rf_model, type = 2)  # type = 2 for IncNodePurity
  print("Variable Importance from Random Forest (IncNodePurity):")
  print(importance_values)
  
  # Create a named vector for optimized weights
  factor_names <- c(
    "book_to_price",
    "fcf_yield",
    "eps_to_price_ntm",
    "roe",
    "debt_to_ic",
    "return_vol",
    "sales_growth",
    "eps_growth",
    "eps_trend",
    "up_down_revisions",
    "momentum_3m",
    "momentum_12m"
  )
  
  # Ensure all importance values are positive
  importance_scores <- importance_values[, "IncNodePurity"]
  importance_scores[importance_scores < 0] <- 0  # Set negative values to zero
  
  # Derive weights from importance values
  optimized_weights <- importance_scores / sum(importance_scores)
  names(optimized_weights) <- factor_names
  
  # Display the optimized weights
  print("Optimized Weights Based on Variable Importance (IncNodePurity):")
  print(optimized_weights)
  
  # Recalculate ModelScore with optimized weights
  dataset[, ModelScore := (
    PercBookToPrice * optimized_weights["book_to_price"] +
      PercFCFYield * optimized_weights["fcf_yield"] +
      PercEPStoPriceNTM * optimized_weights["eps_to_price_ntm"] +
      PercROE * optimized_weights["roe"] +
      PercDebtToIC * optimized_weights["debt_to_ic"] +
      PercReturnVol * optimized_weights["return_vol"] +
      PercSalesGrowth * optimized_weights["sales_growth"] +
      PercEPSGrowth * optimized_weights["eps_growth"] +
      PercEPSTrend * optimized_weights["eps_trend"] +
      PercUpDownRevisions * optimized_weights["up_down_revisions"] +
      PercMomentum3M * optimized_weights["momentum_3m"] +
      PercMomentum12M * optimized_weights["momentum_12m"]
  )]
  
  current_factor <- "ModelScore"
  
  # Define the compute_tiles function if not already defined
  compute_tiles <- function(factor, num_tiles = 10) {
    # Handle NA values appropriately
    N <- sum(!is.na(factor))
    rank <- frank(factor, ties.method = "average", na.last = "keep")
    tile <- ceiling(rank / (N / num_tiles))
    return(tile)
  }
  
  # COMPUTE SUMMARY TABLES
  
  # Assign tiles based on ModelScore within each date
  dataset_with_tiles <- copy(dataset)[,
                                      tile := compute_tiles(factor = get(current_factor), num_tiles = 10),
                                      by = "date"
  ]
  
  # Calculate tile performance
  tile_performance <- dataset_with_tiles[,
                                         .(
                                           tile_median_return_12m = median(relative_return_12m, na.rm = TRUE),
                                           tile_hit_rate_12m = sum(relative_return_12m > 0, na.rm = TRUE) / .N
                                         ),
                                         by = c("date", "tile")
  ][order(date, tile)]
  
  # Define a function to compute long-short performance
  compute_long_short <- function(dataset) {
    long_tile <- max(dataset$tile, na.rm = TRUE)
    short_tile <- min(dataset$tile, na.rm = TRUE)
    long_short_data <- dataset[tile %in% c(long_tile, short_tile), ]
    
    # Calculate long-short median return and hit rate
    long_short_summary <- long_short_data[, .(
      tile = "Long-Short",
      tile_median_return_12m = tile_median_return_12m[tile == long_tile] - tile_median_return_12m[tile == short_tile],
      tile_hit_rate_12m = (tile_hit_rate_12m[tile == long_tile] - tile_hit_rate_12m[tile == short_tile]) / 2 + 0.5
    ), by = "date"]
    
    # Combine with original dataset
    combined_dataset <- rbind(dataset, long_short_summary, fill = TRUE)
    return(combined_dataset)
  }
  
  # Compute long-short performance
  tile_performance <- compute_long_short(tile_performance)
  
  # Calculate average performance metrics for each tile
  factor_summary <- tile_performance[, .(
    avg_12m_return = mean(tile_median_return_12m, na.rm = TRUE),
    avg_12m_hit_rate = mean(tile_hit_rate_12m, na.rm = TRUE)
  ), by = tile]
  
  # Print the factor summary
  print("Factor Performance Summary:")
  print(factor_summary)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ALL 12 Factors 
  
  
  
  
  
  
  
  
  # Compute the percentile ranks for all 12 factors
  dataset[,
          `:=`(
            PercBookToPrice = compute_percentiles(book_to_price),
            PercFCFYield = compute_percentiles(fcf_yield),
            PercEPStoPriceNTM = compute_percentiles(eps_to_price_ntm),
            PercROE = compute_percentiles(roe),
            PercDebtToIC = compute_percentiles(debt_to_ic),
            PercReturnVol = compute_percentiles(return_vol, flip = TRUE),  # Flip because lower volatility is better
            PercSalesGrowth = compute_percentiles(sales_growth),
            PercEPSGrowth = compute_percentiles(eps_growth),
            PercEPSTrend = compute_percentiles(eps_trend),
            PercUpDownRevisions = compute_percentiles(up_down_revisions),
            PercMomentum3M = compute_percentiles(momentum_3m),
            PercMomentum12M = compute_percentiles(momentum_12m)
          ),
          by = "date"
  ]
  
  
  
  objective_function_return_risk <- function(weights) {
    # Normalize weights
    weights <- weights / sum(weights)
    
    # Calculate ModelScore
    dataset[, ModelScore := 
              (weights[1] * PercBookToPrice) +
              (weights[2] * PercFCFYield) +
              (weights[3] * PercEPStoPriceNTM) +
              (weights[4] * PercROE) +
              (weights[5] * PercDebtToIC) +
              (weights[6] * PercReturnVol) +
              (weights[7] * PercSalesGrowth) +
              (weights[8] * PercEPSGrowth) +
              (weights[9] * PercEPSTrend) +
              (weights[10] * PercUpDownRevisions) +
              (weights[11] * PercMomentum3M) +
              (weights[12] * PercMomentum12M)
    ]
    # Assign tiles
    dataset[, tile := compute_tiles(ModelScore), by = date]
    
    # Calculate long-short returns
    long_short_returns <- dataset[, .(
      date = date,
      long_short_return = mean(forward_return_12m[tile == 10], na.rm = TRUE) - 
        mean(forward_return_12m[tile == 1], na.rm = TRUE)
    ), by = date]
    
    # Compute mean and standard deviation
    mean_return <- mean(long_short_returns$long_short_return, na.rm = TRUE)
    sd_return <- sd(long_short_returns$long_short_return, na.rm = TRUE)
    
    # Calculate Return/Risk Ratio
    return_risk_ratio <- mean_return / sd_return
    
    # Return negative ratio for minimization
    return(-return_risk_ratio)
  }
  
  
  objective_function_risk <- function(weights) {
    # Ensure weights sum to 1
    weights <- weights / sum(weights)
    
    # Calculate ModelScore using the provided weights
    dataset[, ModelScore := 
              (weights[1] * PercBookToPrice) +
              (weights[2] * PercFCFYield) +
              (weights[3] * PercEPStoPriceNTM) +
              (weights[4] * PercROE) +
              (weights[5] * PercDebtToIC) +
              (weights[6] * PercReturnVol) +
              (weights[7] * PercSalesGrowth) +
              (weights[8] * PercEPSGrowth) +
              (weights[9] * PercEPSTrend) +
              (weights[10] * PercUpDownRevisions) +
              (weights[11] * PercMomentum3M) +
              (weights[12] * PercMomentum12M)
    ]
    # Assign tiles (deciles) based on ModelScore within each date
    dataset[, tile := compute_tiles(ModelScore), by = date]
    
    # Calculate long and short returns for each date
    long_short_returns <- dataset[tile %in% c(1, 10), .(
      long_return = mean(forward_return_12m[tile == 10], na.rm = TRUE),
      short_return = mean(forward_return_12m[tile == 1], na.rm = TRUE)
    ), by = date]
    
    # Calculate long-short return for each date
    long_short_returns[, long_short_return := long_return - short_return]
    
    # Compute mean and standard deviation of long-short returns
    mean_return <- mean(long_short_returns$long_short_return, na.rm = TRUE)
    sd_return <- sd(long_short_returns$long_short_return, na.rm = TRUE)
    
    sharpe_ratio <- (mean_return +.045) / sd_return
    
    # Return negative Sharpe Ratio for minimization
    return(-sharpe_ratio)
  }
  
  # Optimize weights for all 12 factors
  risk_optimized_weights <- optim(
    par = rep(1/12, 12),  # Start with equal weights for all 12 factors
    fn = objective_function_return_risk,
    method = "L-BFGS-B",
    lower = 0,  # Ensure weights are non-negative
    upper = 1   # Ensure weights are at most 1
  )
  
  # Get optimized weights
  optimized_weights <- risk_optimized_weights$par / sum(risk_optimized_weights$par)
  names(optimized_weights) <- c("book_to_price", "fcf_yield", "eps_to_price_ntm", "roe", 
                                "debt_to_ic", "return_vol", "sales_growth", "eps_growth", 
                                "eps_trend", "up_down_revisions", "momentum_3m", "momentum_12m")
  
  # Normalize weights to sum to 1
  optimized_weights <- optimized_weights / sum(optimized_weights)
  print(optimized_weights)
  
  # Check if weights sum to 1
  cat("Sum of weights:", sum(optimized_weights), "\n")

  # Update ModelScore using optimized weights
  dataset[,
          FullModelScore := 
            (PercBookToPrice * optimized_weights["book_to_price"]) +
            (PercFCFYield * optimized_weights["fcf_yield"]) +
            (PercEPStoPriceNTM * optimized_weights["eps_to_price_ntm"]) +
            (PercROE * optimized_weights["roe"]) +
            (PercDebtToIC * optimized_weights["debt_to_ic"]) +
            (PercReturnVol * optimized_weights["return_vol"]) +
            (PercSalesGrowth * optimized_weights["sales_growth"]) +
            (PercEPSGrowth * optimized_weights["eps_growth"]) +
            (PercEPSTrend * optimized_weights["eps_trend"]) +
            (PercUpDownRevisions * optimized_weights["up_down_revisions"]) +
            (PercMomentum3M * optimized_weights["momentum_3m"]) +
            (PercMomentum12M * optimized_weights["momentum_12m"])
  ]
  
  current_factor <- "FullModelScore"
  
  
  # COMPUTE SUMMARY TABLES
  
  dataset_with_tiles <- copy(dataset)[,
                                      tile := compute_tiles(factor = get(current_factor), num_tiles = 10),
                                      by = "date"
  ]
  
  tile_performance <- dataset_with_tiles[,
                                         .(
                                           tile_median_return_12m = median(relative_return_12m),
                                           tile_hit_rate_12m = sum(relative_return_12m > 0) / .N
                                         ),
                                         by = c("date", "tile")
  ][order(date, tile)]
  
  compute_long_short <- function(dataset) {
    long <- dataset[, max(tile)]
    short <- dataset[, min(tile)]
    dataset[,
            .(
              tile = c(
                tile,
                tile[tile == long] + tile[tile == short]
              ),
              tile_median_return_12m = c(
                tile_median_return_12m,
                tile_median_return_12m[tile == long] - tile_median_return_12m[tile == short]
              ),
              tile_hit_rate_12m = c(
                tile_hit_rate_12m,
                (tile_hit_rate_12m[tile == long] - tile_hit_rate_12m[tile == short]) / 2 + 0.5
              )
            ),
            by = "date"
    ]
  }
  
  tile_performance <- compute_long_short(tile_performance)
  
  factor_summary <- tile_performance[, .(
    avg_12m_return = mean(tile_median_return_12m, na.rm = TRUE),
    avg_12m_hit_rate = mean(tile_hit_rate_12m, na.rm = TRUE)
  ), by = tile]
  
  print(factor_summary)
  
  
  
  
  
  
  
  
  