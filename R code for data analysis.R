## Part 1: Data Cleaning
# Install and load necessary packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("writexl")

library(readxl)
library(tidyverse)
library(writexl)

# Read the data
data <- read_excel("C:/Users/Administrator/Desktop/Case study麦可思/data.xlsx")
student_info <- read_excel("C:/Users/Administrator/Desktop/Case study麦可思/student_info.xlsx")

# Merge data
data <- left_join(data, student_info, by = c("serialno" = "student_id"))

# Remove samples from the class of 2022
data <- data %>%
  filter(graduation_year != 2022)

# Check if samples from the class of 2022 are completely removed
remaining_2022 <- data %>%
  filter(graduation_year == 2022)
print(remaining_2022) 
# A tibble: 0 × 9
# ℹ 9 variables: serialno <dbl>, area_code <dbl>, current_status <chr>,
#   multianwser_300_1 <chr>, multianwser_300_2 <chr>,
#   multianwser_300_3 <chr>, college <chr>, major <chr>, graduation_year <dbl>

# Rename columns and replace single-choice question options
data <- data %>%
  rename(Current_Status = current_status) %>%
  mutate(
    Current_Status = recode(Current_Status,
                            `1` = "Employed",
                            `2` = "Further Study",
                            `3` = "Unemployed, Seeking Job",
                            `4` = "Unemployed, Preparing for Study",
                            `5` = "Prefer not to disclose")
  )

# Replace variable names for multiple-choice questions and clean options
data <- data %>%
  rename(
    Problem_Solving = multianwser_300_1,
    Teamwork = multianwser_300_2,
    Communication = multianwser_300_3
  ) %>%
  mutate(across(c(Problem_Solving, Teamwork, Communication), ~ ifelse(!is.na(.), 1, 0)))

# Export the cleaned dataset
write_xlsx(data, "cleaned_data.xlsx")

## Part 2: Data Analysis
# 1. Number of samples collected from each college
samples_per_college <- data %>%
  group_by(college) %>%
  summarize(Sample_Count = n())
print(samples_per_college)
# A tibble: 5 × 2
# college  Sample_Count
# <chr>       <int>
#  1 College A   1190
#  2 College B    872
#  3 College C   1130
#  4 College D   1354
#  5 College E    439

# 2. Relationship between colleges and majors
college_major_relation <- data %>%
  select(college, major) %>%
  distinct()
print(college_major_relation)
# A tibble: 18 × 2
# college  major
# <chr>    <chr>   
#  1 College B   Major 6   
#  2 College A   Major 5   
#  3 College C   Major 10  
#  4 College A   Major 3   
#  5 College C   Major 9   
#  6 College C   Major 12  
#  7 College D   Major 13  
#  8 College E   Major 18  
#  9 College A   Major 4   
# 10 College B   Major 7   
# 11 College A   Major 2   
# 12 College D   Major 15  
# 13 College D   Major 16  
# 14 College C   Major 11  
# 15 College A   Major 1   
# 16 College B   Major 8   
# 17 College E   Major 17  

# 3. Employment and further study rates
employment_rate <- data %>%
  filter(Current_Status != "Prefer not to disclose") %>%
  summarize(
    Employment_Rate = mean(Current_Status == "Employed"),
    Further_Study_Rate = mean(Current_Status == "Further Study")
  )
print(employment_rate)
# A tibble: 1 × 2
# Employment_Rate Further_Study_Rate
# <dbl>             <dbl>
#  1  0.220            0.527

# 4. College where graduates' problem-solving skills have improved the most
improved_college <- data %>%
  group_by(college) %>%
  summarize(
    Problem_Solving_Improvement = mean(Problem_Solving, na.rm = TRUE)
  ) %>%
  arrange(desc(Problem_Solving_Improvement))
print(improved_college)
# A tibble: 5 × 2
# college  Problem_Solving_Improvement
# <chr>       <dbl>
#  1 College C       0.664
#  2 College A       0.644
#  3 College B       0.643
#  4 College D       0.643
#  5 College E       0.642

# Part 3: Data Insights

# 1. Employment and further study rates by major
library(dplyr)
library(ggplot2)
library(scales)

major_employment_rate <- data %>%
  filter(Current_Status != "Prefer not to disclose") %>%
  group_by(major) %>%
  summarize(
    Employment_Rate = mean(Current_Status == "Employed"),
    Further_Study_Rate = mean(Current_Status == "Further Study")
  )

# Select top 5 majors by employment rate
top5_employment_rate <- major_employment_rate %>%
  arrange(desc(Employment_Rate)) %>%
  top_n(5, Employment_Rate)

# Select top 5 majors by further study rate
top5_graduation_rate <- major_employment_rate %>%
  arrange(desc(Further_Study_Rate)) %>%
  top_n(5, Further_Study_Rate)

print(top5_employment_rate)
print(top5_graduation_rate)

# A tibble: 5 × 3
# major    Employment_Rate Further_Study_Rate
# <chr>        <dbl>           <dbl>
#  1 Major 4     0.260           0.468
#  2 Major 1     0.252           0.509
#  3 Major 5     0.251           0.531
#  4 Major 9     0.246           0.512
#  5 Major 6     0.245           0.471
# > print(top5_graduation_rate)
# A tibble: 5 × 3
# major    Employment_Rate Further_Study_Rate
# <chr>        <dbl>           <dbl>
#  1 Major 8     0.178           0.585
#  2 Major 2     0.212           0.581
#  3 Major 10    0.203           0.567
#  4 Major 18    0.239           0.550
#  5 Major 7     0.222           0.549

# 2. Employment rate plot
ggplot(top5_employment_rate, aes(x = reorder(major, -Employment_Rate), y = Employment_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = scales::percent(Employment_Rate, accuracy = 0.1)), vjust = -0.5, color = "black") +
  labs(
    title = "Top 5 Majors by Employment Rate",
    x = "Major",
    y = "Employment Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black", size = 0.5, arrow = arrow(type = "closed", length = unit(0.15, "inches")))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# 3. Further study rate plot
ggplot(top5_graduation_rate, aes(x = reorder(major, -Further_Study_Rate), y = Further_Study_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = scales::percent(Further_Study_Rate, accuracy = 0.1)), vjust = -0.5, color = "black") +
  labs(
    title = "Top 5 Majors by Further Study Rate",
    x = "Major",
    y = "Further Study Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black", size = 0.5, arrow = arrow(type = "closed", length = unit(0.15, "inches")))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# 4. Employment and further study rates by college
college_employment_rate <- data %>%
  filter(Current_Status != "Prefer not to disclose") %>%
  group_by(college) %>%
  summarize(
    Employment_Rate = mean(Current_Status == "Employed"),
    Further_Study_Rate = mean(Current_Status == "Further Study")
  )

print(college_employment_rate)
# A tibble: 5 × 3
# college  Employment_Rate Further_Study_Rate
# <chr>         <dbl>             <dbl>
#  1 College A     0.231             0.521
#  2 College B     0.214             0.537
#  3 College C     0.224             0.516
#  4 College D     0.210             0.530
#  5 College E     0.223             0.544

# Reshape data for dual bar chart
college_employment_rate_long <- college_employment_rate %>%
  pivot_longer(cols = c("Employment_Rate", "Further_Study_Rate"), names_to = "Indicator", values_to = "Percentage")

# Plot
ggplot(college_employment_rate_long, aes(x = reorder(college, -Percentage), y = Percentage, fill = Indicator)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)), 
            position = position_dodge(width = 0.7), vjust = -0.5, color = "black", size = 3) +  # Adjust font size
  labs(
    title = "Employment and Further Study Rates by College",
    x = "College",
    y = "Percentage",
    fill = "Indicator"
  ) +
  scale_fill_manual(values = c("Employment_Rate" = "lightgray", "Further_Study_Rate" = "steelblue")) +  # Light gray and steel blue
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),  # Adjust title font size
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust X-axis labels font size
    axis.text.y = element_text(size = 8),  # Adjust Y-axis labels font size
    axis.title = element_text(size = 10),  # Adjust axis titles font size
    legend.title = element_text(size = 10),  # Adjust legend title font size
    legend.text = element_text(size = 8),  # Adjust legend text font size
    panel.grid = element_blank(),  # Remove background grid
    axis.line = element_line(color = "black", size = 0.5, arrow = arrow(type = "closed", length = unit(0.15, "inches")))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
