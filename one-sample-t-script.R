#####SETUP#####
library(tidyverse)
library(broom)

#####DEFINE HYPOTHESIS#####
### null hypothesis: mean of my data equals `null_mean`
null_mean <- 0

### one-sided or two-sided test? what's the alpha?
# two-sided test alternative: mean of my data does not equal `null_mean`
# one-sided lesser alternative: mean of my data is less than `null_mean`
# one-sided greater alternative: mean of my data is greater than `null_mean`
two_sided <- TRUE
less_than <- TRUE
alpha <- 0.05

#####INPUT DATA#####
### input your data (ideally, read from a file)
my_data <- rnorm(25)

### name your data (e.g. what should appear on a graph axis?)
data_title <- "Age (years)"

### view raw data
print(my_data)

#####VALIDATE#####
### ignore the code below until you reach the ASSUMPTIONS section
# confirm validity
if (!all(is.numeric(my_data))) {
  stop("Your data must be entirely made up of numbers", call. = FALSE)
}
if (sum(is.na(my_data)) > 0) {
  stop("Your data has missing values.", call. = FALSE)
}

# convert to tibble for some code
my_df <- tibble(x = my_data)

#####ASSUMPTIONS#####
### check for outliers using a boxplot
g_outlier <- ggplot(my_df, aes(x = x)) +
  geom_boxplot() +
  xlab(data_title) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
ggsave("outlier-boxplot.png", g_outlier, width = 6, height = 2.5)

### check normality using a quantile-quantile plot
g_qq <- ggplot(my_df, aes(sample = x)) +
  stat_qq_line() +
  stat_qq() +
  xlab("Theoretical") +
  ylab("Sample") +
  theme_bw()
ggsave("normality-qqplot.png", g_qq, width = 5, height = 5)

#####VISUALIZE DATA#####
### general setup
g <- ggplot(my_df, aes(x = x)) +
  xlab(data_title) +
  theme_bw()

### histogram
g_hist <- g +
  geom_histogram(bins = 30, fill = "white", color = "black") +
  ylab("Frequency")
ggsave("histogram-data.png", g_hist, width = 6, height = 2.5)

### density plot
g_dens <- g +
  geom_density() +
  ylab("Density")
ggsave("density-data.png", g_dens, width = 6, height = 2.5)

### boxplot
g_box <- g +
  geom_boxplot() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
ggsave("boxplot-data.png", g_box, width = 6, height = 2.5)

#####STATISTICAL TEST#####
# conduct one-sample t-test
t_calc <- t.test(x = my_data,
                 mu = null_mean,
                 conf.level = alpha,
                 alternative = ifelse(two_sided, "two.sided",
                                      ifelse(less_than, "less", "greater")))

t_vals <- tidy(t_calc)

# output in 2 formats
print(t_calc)
print(t_vals)

#####VISUALIZE STATISTICAL TEST#####

