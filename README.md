# Statistical Data Analysis in R Programming Environment


---

---

---
title: "Tourism Dataset Analysis"
author: "Saad Ahmed Jamal"
---

# Load Required Libraries
```{r message=FALSE}
# Load necessary libraries for data manipulation, visualization, and statistical analysis
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(psych)       # For descriptive statistics
library(corrplot)    # For correlation matrix visualization
#library(Hmisc)
library(tidyverse)
library(car)
#install.packages(c("psych" ))

```


## Load the Dataset
```{r}
# Load the dataset from the "Tourism.xlsx" file
tourism <- read_excel("../exercise/databases/Tourism.xlsx")

# Display the first few rows of the dataset to understand its structure
head(tourism)

# Check the structure of the dataset to identify data types for each column
str(tourism)

# Summarize the dataset to get an overview of the data
summary(tourism)
```
## Part 1: Data Cleaning
## Check for Missing Values
```{r}
# Count the number of missing values in each column of the dataset
colSums(is.na(tourism))
```
#Remove Rows with Missing Values
```{r}
# Remove rows with missing values to ensure a clean dataset
tourism <- na.omit(tourism)
```
#Check for Outliers in Numeric Variables
```{r}
# Create a boxplot for numeric variables to identify potential outliers
numeric_cols <- select(tourism, where(is.numeric))
boxplot(numeric_cols, main = "Boxplot for Numeric Variables", las = 2)
```
## Part 2: Descriptive Statistics
## Summary Statistics for Numeric Variables
```{r}
# Generate descriptive statistics for numeric variables in the dataset
describe(tourism)
```
## Visualize Distribution of Total Expenditure
```{r}

# Scatterplot
ggplot(tourism, aes( x = tourism$Expenditure_for_organiser, y = tourism$Expenditure_on_transport)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot Linearity Check", x = "Organiser Expenditure", y = "Expenditure on Transport") +
  theme_minimal()

ggplot(tourism, aes( x = tourism$Expenditure_for_organiser, y = tourism$Private_expenditure)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot Linearity Check", x = "Organiser Expenditure", y = "Expenditure Private") +
  theme_minimal()



correlation_matrix <- cor(tourism$Expenditure_for_organiser, tourism$Private_expenditure)
print(correlation_matrix)

```
## Historgram for Total Expenditure and Bar Chart for Accommodation Type
```{r}
# Create a histogram to visualize the distribution of total expenditure
ggplot(tourism, aes(x = Total_expenditure)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Expenditure", x = "Total Expenditure", y = "Frequency")


# Create a bar chart to display the frequency of each accommodation type
ggplot(tourism, aes(x = Accommodation_type)) +
  geom_bar(fill = "orange") +
  labs(title = "Accommodation Type Distribution", x = "Accommodation Type", y = "Count")


```

## Part 3: Relationships Between Variables
## Correlation Matrix for Numeric Variables
```{r}
# Compute and visualize the correlation matrix for numeric variables
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "circle", title = "Correlation Matrix")
```


# Cross-tabulation: Accommodation Type and Purpose of Trip
```{r}
# Create a contingency table to analyze the relationship between accommodation type and purpose of trip
table(tourism$Accommodation_type, tourism$Purpose_of_trip)
```

## Part 4: Hypothesis Testing
## Perform T-Test
```{r}
# Assumptions Testing: Normality Test and Homogeneity Test
# group 1 - private, group 2 - business
group1 <- tourism[tourism$Accommodation_type=="Private",]  
group2 <- tourism[tourism$Accommodation_type!="Private",]  

# Now performing Normality test for the two groups
shapiro.test(group1$Total_expenditure)
shapiro.test(group2$Total_expenditure)
# Since p-value is below 0.05, hence the groups are not normally distributted


# Now performing Homogeneity test for the two groups
leveneTest(tourism$Total_expenditure ~ tourism$Accommodation_type, data = tourism)
# Since p-value is above 0.05, hence the groups are homogeneous ie. variance is equal among the groups.

# Perform a T-Test to compare total expenditure by accommodation type
if ("Total_expenditure" %in% colnames(tourism) && "Accommodation_type" %in% colnames(tourism)) {
    tourism$Accommodation_type <- as.factor(tourism$Accommodation_type)  # Ensure it's a factor
    t_test_result <- t.test(Total_expenditure ~ Accommodation_type, data = tourism)
    print(t_test_result)  # Display the T-Test results
} else {
    stop("T-Test cannot be performed: Missing columns.")
}
```
## Perform ANOVA
```{r}
# Shapiro Wilk Test: Normality Check
shapiro.test(tourism$Total_expenditure[0:5000])
shapiro.test(tourism$Expenditure_on_commodities[0:5000])

ks.test(tourism$Total_expenditure, "pnorm", mean=mean(tourism$Total_expenditure), sd=sd(tourism$Total_expenditure))


# Levene Test: Homogeneity Check
library(car)
#install.package
leveneTest(tourism$Total_expenditure ~ tourism$Purpose_of_trip, data = tourism)
#group

# Perform ANOVA to compare total expenditure by purpose of trip
if ("Total_expenditure" %in% colnames(tourism) && "Purpose_of_trip" %in% colnames(tourism)) {
    tourism$Purpose_of_trip <- as.factor(tourism$Purpose_of_trip)  # Ensure it's a factor
    anova_result <- aov(Total_expenditure ~ Purpose_of_trip, data = tourism)
    print(summary(anova_result))  # Display the ANOVA results
} else {
    stop("ANOVA cannot be performed: Missing columns.")
}


# Man Witney wilcox test
wilcox.test(tourism$Total_expenditure ~ tourism$Accommodation_type, data = tourism)

```
## Perform Fisher's Exact Test with Simulated p-values
```{r}
# Perform Fisher's Exact Test with simulated p-values to assess the association between accommodation type and purpose of trip
if ("Accommodation_type" %in% colnames(tourism) && "Purpose_of_trip" %in% colnames(tourism)) {
    fisher_test_result <- tryCatch(
        fisher.test(table(tourism$Accommodation_type, tourism$Purpose_of_trip), simulate.p.value = TRUE, B = 1e5),
        error = function(e) NULL
    )
    if (!is.null(fisher_test_result)) {
        print(fisher_test_result)  # Display Fisher's Exact Test results
    } else {
        warning("Fisher's Exact Test with simulation could not be performed.")
    }
} else {
    fisher_test_result <- NULL
    warning("Columns 'Accommodation_type' or 'Purpose_of_trip' are missing in the dataset.")
}
```
## Part 5: Save Outputs
## Save Histogram for Total Expenditure
```{r}
# Save the histogram of total expenditure as a PNG file
histogram_plot <- ggplot(tourism, aes(x = Total_expenditure)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Total Expenditure", y = "Density")
ggsave("total_expenditure_histogram.png", plot = histogram_plot, width = 8, height = 6)
```
## Save Scatterplot for Nights Spent vs Total Expenditure
```{r}
# Save the scatterplot of nights spent vs total expenditure as a PNG file
scatterplot <- ggplot(tourism, aes(x = Nights_spent, y = Total_expenditure)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Nights Spent vs Total Expenditure", x = "Nights Spent", y = "Total Expenditure")
ggsave("scatterplot_nights_vs_expenditure.png", plot = scatterplot, width = 8, height = 6)

```
## Save Correlation Matrix as a CSV File
```{r}
# Save the correlation matrix to a CSV file
write.csv(cor_matrix, "correlation_matrix.csv")

```
## Save T-Test Results to a Text File
```{r}
# Save the T-Test results to a text file
t_test_results <- capture.output(print(t_test_result))
writeLines(t_test_results, "t_test_results.txt")

```
## Save ANOVA Results to a Text File
```{r}
# Save the ANOVA results to a text file
anova_results <- capture.output(summary(anova_result))
writeLines(anova_results, "anova_results.txt")

```
## Save Fisher's Exact Test Results to a Text File
```{r}
# Save the Fisher's Exact Test results to a text file
fisher_test_results <- capture.output(print(fisher_test_result))
writeLines(fisher_test_results, "fisher_test_results.txt")

```
## Finish!
