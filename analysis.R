
# PROJECT: Online Shopping Frequency and Consumer Satisfaction
# STEP 4: Data Analysis and Graph Generation

# 1. Load required package
library(ggplot2)

# 2. Load dataset
online_shopping <- read.csv(
  "online_shopping_satisfaction.csv"
)

# 3. View dataset
View(online_shopping)

# 4. Calculate descriptive statistics
summary_statistics <- data.frame(
  Average_Age = mean(online_shopping$Age),
  Average_Monthly_Orders = mean(online_shopping$Monthly_Orders),
  Average_Order_Value_PHP = mean(online_shopping$Average_Order_Value_PHP),
  Average_Monthly_Spending_PHP = mean(online_shopping$Monthly_Spending_PHP),
  Average_Delivery_Satisfaction = mean(online_shopping$Delivery_Satisfaction),
  Average_Overall_Satisfaction = mean(online_shopping$Overall_Satisfaction)
)

print("SUMMARY STATISTICS")
print(summary_statistics)

# 5. Shopping frequency summary
frequency_summary <- as.data.frame(
  table(online_shopping$Shopping_Frequency)
)

colnames(frequency_summary) <- c(
  "Shopping_Frequency",
  "Number_of_Consumers"
)

print("SHOPPING FREQUENCY SUMMARY")
print(frequency_summary)

# 6. Preferred platform summary
platform_summary <- as.data.frame(
  table(online_shopping$Preferred_Platform)
)

colnames(platform_summary) <- c(
  "Preferred_Platform",
  "Number_of_Consumers"
)

print("PREFERRED PLATFORM SUMMARY")
print(platform_summary)

# 7. Product category summary
category_summary <- as.data.frame(
  table(online_shopping$Product_Category)
)

colnames(category_summary) <- c(
  "Product_Category",
  "Number_of_Consumers"
)

print("PRODUCT CATEGORY SUMMARY")
print(category_summary)

# 8. Average monthly spending by platform
spending_summary <- aggregate(
  Monthly_Spending_PHP ~ Preferred_Platform,
  data = online_shopping,
  FUN = mean
)

print("AVERAGE MONTHLY SPENDING BY PLATFORM")
print(spending_summary)

# 9. Average satisfaction by shopping frequency
satisfaction_summary <- aggregate(
  Overall_Satisfaction ~ Shopping_Frequency,
  data = online_shopping,
  FUN = mean
)

print("AVERAGE SATISFACTION BY SHOPPING FREQUENCY")
print(satisfaction_summary)

# 10. Create figures folder
if (!dir.exists("figures")) {
  dir.create("figures")
}

# GRAPH 1: Shopping Frequency Distribution
graph1 <- ggplot(
  online_shopping,
  aes(x = Shopping_Frequency)
) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Online Shopping Frequency",
    x = "Shopping Frequency",
    y = "Number of Consumers"
  ) +
  theme_minimal()

ggsave(
  "figures/shopping_frequency.png",
  graph1,
  width = 8,
  height = 5
)

# GRAPH 2: Preferred Online Shopping Platform
graph2 <- ggplot(
  online_shopping,
  aes(x = Preferred_Platform)
) +
  geom_bar(fill = "darkorange") +
  labs(
    title = "Preferred Online Shopping Platforms",
    x = "Platform",
    y = "Number of Consumers"
  ) +
  theme_minimal()

ggsave(
  "figures/preferred_platform.png",
  graph2,
  width = 8,
  height = 5
)

# GRAPH 3: Average Monthly Spending by Platform
graph3 <- ggplot(
  spending_summary,
  aes(
    x = Preferred_Platform,
    y = Monthly_Spending_PHP
  )
) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Average Monthly Spending by Platform",
    x = "Platform",
    y = "Average Monthly Spending (PHP)"
  ) +
  theme_minimal()

ggsave(
  "figures/monthly_spending_by_platform.png",
  graph3,
  width = 8,
  height = 5
)

# GRAPH 4: Overall Satisfaction by Shopping Frequency
graph4 <- ggplot(
  satisfaction_summary,
  aes(
    x = Shopping_Frequency,
    y = Overall_Satisfaction
  )
) +
  geom_col(fill = "mediumpurple") +
  labs(
    title = "Consumer Satisfaction by Shopping Frequency",
    x = "Shopping Frequency",
    y = "Average Satisfaction (1-5)"
  ) +
  theme_minimal()

ggsave(
  "figures/satisfaction_by_frequency.png",
  graph4,
  width = 8,
  height = 5
)

# 11. Export summary files
write.csv(
  summary_statistics,
  "summary_statistics.csv",
  row.names = FALSE
)

write.csv(
  frequency_summary,
  "shopping_frequency_summary.csv",
  row.names = FALSE
)

write.csv(
  platform_summary,
  "platform_summary.csv",
  row.names = FALSE
)

write.csv(
  category_summary,
  "product_category_summary.csv",
  row.names = FALSE
)

write.csv(
  spending_summary,
  "spending_by_platform.csv",
  row.names = FALSE
)

write.csv(
  satisfaction_summary,
  "satisfaction_by_frequency.csv",
  row.names = FALSE
)

# 12. Display all graphs
print(graph1)
print(graph2)
print(graph3)
print(graph4)

# 13. Completion message
print("Analysis and graph generation completed successfully!")

