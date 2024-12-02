# Install necessary packages (if not already installed)
install.packages(c("dplyr", "psych", "janitor"))

# Load libraries
library(dplyr)
library(psych)
library(janitor)

# Example data frame
data <- data.frame(
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  polaffiliation = sample(c("Democrat", "Republican", "Independent"), 100, replace = TRUE)
)

# Clean column names for consistency
data <- data %>% clean_names()

# Table 1: Mean and Standard Deviation (requires numerical data for means)
# Note: This step calculates only counts for categorical variables.
gender_counts <- data %>% 
  group_by(gender) %>% 
  summarise(count = n())

polaffiliation_counts <- data %>% 
  group_by(polaffiliation) %>% 
  summarise(count = n())

# Table 2: Descriptive Statistics using the `describe` function
# For gender and polaffiliation (categorical), this provides frequencies.
gender_descriptive <- describe(table(data$gender))
polaffiliation_descriptive <- describe(table(data$polaffiliation))

# Table 3: Summary Statistics
gender_summary <- summary(data$gender)
polaffiliation_summary <- summary(data$polaffiliation)

# Combine the tables into a final output
cat("Descriptive Table for Gender and Political Affiliation\n\n")

cat("\nMean and Standard Deviation:\n")
print(gender_counts)
print(polaffiliation_counts)

cat("\nDescriptive Statistics:\n")
print(gender_descriptive)
print(polaffiliation_descriptive)

cat("\nSummary Statistics:\n")
print(gender_summary)
print(polaffiliation_summary)

# Contingency Table (Cross-tabulation)
cat("\nContingency Table (Gender vs. Political Affiliation):\n")
contingency_table <- table(data$gender, data$polaffiliation)
print(contingency_table)

# Optional: Save results to a CSV file
write.csv(contingency_table, "contingency_table.csv")


# Install necessary packages
install.packages(c("dplyr", "ggplot2"))

# Load libraries
library(dplyr)
library(ggplot2)

# Example data frame
data <- data.frame(
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  polaffiliation = sample(c("Democrat", "Republican", "Independent"), 100, replace = TRUE)
)

# Table 2: Contingency Table
contingency_table <- table(data$gender, data$polaffiliation)

# Print the contingency table
cat("Contingency Table (Gender vs. Political Affiliation):\n")
print(contingency_table)

# Perform Chi-squared Test
chi_test <- chisq.test(contingency_table)

# Print the results of the Chi-squared test
cat("\nChi-squared Test Results:\n")
print(chi_test)

# Convert contingency table to a data frame for plotting
contingency_df <- as.data.frame(as.table(contingency_table))
colnames(contingency_df) <- c("Gender", "Political_Affiliation", "Frequency")

# Visualize the contingency table with a heatmap
ggplot(contingency_df, aes(x = Political_Affiliation, y = Gender, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Contingency Table Heatmap",
       x = "Political Affiliation",
       y = "Gender",
       fill = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


