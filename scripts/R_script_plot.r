# 1. Clear workspace
rm(list = ls())

# 2. Load required libraries
library(ggplot2)
library(dplyr)

# 3. Load dataset
data <- read.csv(
  "Summer Olympics medal count, participation, hosting - Sheet2.csv",
  stringsAsFactors = FALSE
)

# 4. Inspect data structure
str(data)
head(data)

# ------------------------------------------------
# 5. Histogram (Required Supplementary Graphic)
# ------------------------------------------------
ggplot(data, aes(x = total_summer)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 25,
    fill = "lightblue",
    color = "black"
  ) +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Total Summer Olympic Medal Counts",
    x = "Total Summer Olympic Medals",
    y = "Density"
  ) +
  theme_minimal()

# ------------------------------------------------
# 6. Scatter Plot (Main Visualisation)
# ------------------------------------------------
ggplot(data, aes(x = no_hosted, y = total_summer)) +
  geom_point(color = "darkblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Hosting the Olympics and Total Medal Count",
    x = "Number of Times Hosted",
    y = "Total Summer Olympic Medals"
  ) +
  theme_minimal()
# ------------------------------------------------
# 7. Correlation Test (Pearson)
# ------------------------------------------------
correlation_test <- cor.test(
  data$no_hosted,
  data$total_summer,
  method = "pearson"
)

print(correlation_test)
# ------------------------------------------------
# 8. Decision Based on p-value
# ------------------------------------------------
alpha <- 0.05

if (correlation_test$p.value < alpha) {
  decision <- "Reject the null hypothesis"
} else {
  decision <- "Fail to reject the null hypothesis"
}

print(decision)