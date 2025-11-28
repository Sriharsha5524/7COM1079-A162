library(ggplot2)
library(dplyr)

dataset <- read.csv("Summer Olympics medal count, participation, hosting - Sheet2.csv")

dataset$host_group <- ifelse(dataset$no_hosted > 0, "Host", "Non-host")
dataset$host_group <- factor(dataset$host_group)

summary(dataset)

ggplot(dataset, aes(x = host_group, y = total_summer, fill = host_group)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Total Summer Olympic Medals: Host vs Non-host Countries",
    x = "Hosting Status",
    y = "Total Summer Medals"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = total_summer)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Total Summer Olympic Medals",
    x = "Total Summer Medals",
    y = "Frequency"
  ) +
  theme_minimal()

table(dataset$host_group)

t_test_result <- t.test(total_summer ~ host_group, data = dataset)

t_test_result


dataset %>% 
  group_by(host_group) %>% 
  summarise(
    mean_medals = mean(total_summer, na.rm = TRUE),
    sd_medals = sd(total_summer, na.rm = TRUE),
    count = n()
  )

