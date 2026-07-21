library(tidyverse)
library(poweRlaw)

data <- read_csv("backing.csv", comment = "#", show_col_types = FALSE)

# 1. Install and load the package
# install.packages("poweRlaw")

# 2. Setup sample coordinate data (e.g., Rank X vs Frequency Y)
x_rank <- data$value
y_freq <- data$backers

# 3. Reconstruct individual event observations for the package
# poweRlaw requires a vector of individual values, not summarized counts
raw_data <- rep(x_rank, times = y_freq)

# 4. Initialize a discrete power law object
# Use 'conpl$new' instead if your data contains decimals (continuous)
m_pl <- displ$new(raw_data)

# 5. Estimate the optimal lower threshold (xmin) and scaling exponent (alpha)
# This automatically determines where the power law behavior actually starts
est_xmin <- estimate_xmin(m_pl)

# 6. Apply the calculated parameters back to our model object
m_pl$setXmin(est_xmin$xmin)
m_pl$setPars(est_xmin$pars)

# 7. Print out the critical parameters
cat("Power law scaling exponent (alpha):", m_pl$getPars(), "\n")
cat("Lower threshold cutoff (xmin):", m_pl$getXmin(), "\n")

# 8. Plot the empirical data vs the fitted power law curve
# This creates a log-log plot of the Cumulative Distribution Function (CDF)
# plot(m_pl,
#     xlab = "Value (X)", ylab = "Complementary CDF P(X >= x)",
#     main = "poweRlaw Distribution Fit"
# )
# lines(m_pl, col = "firebrick", lwd = 2)

# emp_points <- poweRlaw:::plot_data(m_pl)

# # 4. Extract the fitted power law line coordinates
# fit_line <- poweRlaw:::line_data(m_pl)


emp_points <- plot(m_pl, draw = FALSE)
fit_line <- lines(m_pl, draw = FALSE)

gof_g <- 
    ggplot() +
    geom_point(
        data = emp_points, aes(x = x, y = y),
        color = "black", alpha = 0.6, size = 2
    ) +
    geom_line(
        data = fit_line, aes(x = x, y = y),
        color = "firebrick", linewidth = 1
    ) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
        title = "poweRlaw Distribution Fit (ggplot2)",
        subtitle = paste0(
            "Alpha: ", round(m_pl$getPars(), 2),
            " | Xmin: ", m_pl$getXmin()
        ),
        x = "Value (X)",
        y = "Complementary CDF P(X >= x)"
    ) +
    theme_minimal()

ggsave("goodnessoffit.png", width = 8, height = 6, plot = gof_g)

bs_gof <- bootstrap_p(m_pl, no_of_sims = 500, threads = 12)

# 2. Extract the calculated p-value
cat("Estimated p value: ",bs_gof$p)

# 3. Extract the empirical Kolmogorov-Smirnov statistic
cat("Estimated gooddness of fit: ",bs_gof$gof)
