# Load necessary libraries
library(dplyr)
library(tidyverse)
library(lubridate)
# Install required libraries (if not already installed)
install.packages("dplyr")      # for data manipulation
install.packages("tidyverse")  # includes ggplot2, dplyr, tidyr, etc.
install.packages("lubridate")  # for date manipulation
install.packages("readxl")     # for reading Excel files
install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("psych")
install.packages("knitr")
install.packages("corrplot")
install.packages("modelr")
install.packages("tableone")

# Load libraries
# Load the libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(caret)
library(randomForest)
library(psych)
library(knitr)
library(corrplot)
library(modelr)
library(tableone)
library(plotly)



# Define the file path
file_path <- "C:/Users/Acer/OneDrive/Desktop/CV/Book2/Book2.xlsx"

# Import data from Sheet 7
data <- read_excel(file_path, sheet = 7)

# Install dplyr if it's not installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load dplyr for the pipe operator
library(dplyr)

# Check column names
print(colnames(data))



library(dplyr)
library(tidyr)

# Assuming 'data' is your dataframe and it has columns named 'Age', 'Gender', 'Socio-economic Status', and 'Institution Level'

# 1. Calculate distribution of Age Groups
# Assuming age is a categorical variable in your dataset
age_distribution <- data %>%
  count(Age) %>%
  mutate(Percentage = n / sum(n) * 100)

# 2. Calculate Gender Proportions
gender_proportions <- data %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100)

# 3. Calculate distribution for Socio-economic Statuses
socio_economic_distribution <- data %>%
  count(`Socio-economic Status`) %>%
  mutate(Percentage = n / sum(n) * 100)

# 4. Calculate distribution for Institution Types
institution_type_distribution <- data %>%
  count(`Institution Level`) %>%
  mutate(Percentage = n / sum(n) * 100)

# Print the results to the console
print("Age Group Distribution:")
print(age_distribution)
print("Gender Proportions:")
print(gender_proportions)
print("Socio-economic Status Distribution:")
print(socio_economic_distribution)
print("Institution Type Distribution:")
print(institution_type_distribution)






#Visualization########
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
#1:Create Bar Charts or Pie Charts for Categorical Variables
# Assuming data_summary has already been calculated as shown in the previous example
# Ensure the dataset includes 'count' which will be displayed on the bar chart

# Define a color palette with sufficient colors
color_palette <- brewer.pal(n = 9, name = "Set3")

# Create the grouped bar chart with confidence intervals and frequency labels
ggplot(data_summary, aes(x=Value, y=mean, fill=Value)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), width=0.8) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label=count, y=mean + 0.05 * max(data_summary$mean)),  # Adjust y position for visibility
            position=position_dodge(width=0.9), vjust=-0.5) +
  facet_wrap(~Category, scales = "free_x") +
  scale_fill_manual(values=color_palette) +
  labs(title = "Distribution of Gender, Residential, and Socio-economic Status",
       y = "Number of Participants", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        legend.position = "bottom")










######

# Cross-tabulation of Gender and Socio-economic Status
socio_gender_table <- table(data$gender, data$`socio-economic status`)

# Calculate proportions for better interpretation
socio_gender_prop <- prop.table(socio_gender_table, margin = 1)  # Row proportions to see distribution within each gender

# Print the cross-tabulation results
print(socio_gender_table)
print(socio_gender_prop)




####
library(ggplot2)

# Visualizing Gender by Socio-economic Status
ggplot(data, aes(x = `socio-economic status`, fill = gender)) +
  geom_bar(position = "fill") +  # 'fill' stacks the bar to show proportional distribution
  scale_y_continuous(labels = scales::percent_format()) +  # Converts y-axis to percentage format
  labs(title = "Gender Distribution by Socio-economic Status", x = "Socio-economic Status", y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Print the plot
print(ggplot(data, aes(x = `socio-economic status`, fill = gender)))



# Print the column names to verify their accuracy
print(names(data))


# Clean up column names by removing non-printing characters and excess spaces
names(data) <- gsub("[\r\n]", "", names(data))  # Remove carriage returns and new lines
names(data) <- gsub("\\s+$", "", names(data))  # Remove trailing spaces

# Optionally, make all column names lowercase for consistency
names(data) <- tolower(names(data))

# Verify the new column names
print(names(data))





# Cross-tabulation of Gender and Educational Discipline
edu_gender_table <- table(data$gender, data$`educational discipline`)

# Print the cross-tabulation results
print(edu_gender_table)

# Remove carriage returns, new lines, and trailing spaces from 'educational discipline'
data$`educational discipline` <- gsub("[\r\n]", "", data$`educational discipline`)
data$`educational discipline` <- gsub("\\s+$", "", data$`educational discipline`)

# Re-check the unique values to ensure they are clean
print(unique(data$`educational discipline`))

# Recreate the cross-tabulation table
edu_gender_table <- table(data$gender, data$`educational discipline`)

# Print the updated cross-tabulation results
print(edu_gender_table)


# Create the bar chart with counts labeled above each bar
ggplot(data, aes(x = `educational discipline`, fill = gender)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "count") +  # Use dodge to place bars side by side
  geom_text(stat = 'count', aes(label = ..count.., group = gender), 
            position = position_dodge(width = 0.9), vjust = -0.5,  # Position text above bars
            color = "black") +
  scale_fill_brewer(palette = "Set1") +  # Using a color palette that's clear and professional
  labs(title = "Gender Distribution by Educational Discipline",
       x = "Educational Discipline", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "right")  # Adjust legend position if needed




#Step 1: Prepare the Data
# Clean up 'institution level' if necessary (remove extra spaces, new lines)
data$`institution level` <- gsub("[\r\n]", "", data$`institution level`)
data$`institution level` <- gsub("\\s+$", "", data$`institution level`)

# Check the unique values to ensure they are clean
print(unique(data$`institution level`))



#Step 2: Cross-Tabulation


# Cross-tabulation of Gender and Institution Level
inst_level_gender_table <- table(data$gender, data$`institution level`)

# Print the cross-tabulation results
print(inst_level_gender_table)





#Visualization with ggplot2
# Bar Chart for Gender Distribution by Institution Level
# Professional Bar Chart for Gender Distribution by Institution Level
ggplot(data, aes(x = `institution level`, fill = gender)) +
  geom_bar(position = "dodge", width = 0.7) +  # Adjust bar width for aesthetics
  geom_text(
    aes(label = ..count.., group = gender), 
    stat = "count", 
    position = position_dodge(width = 0.7), 
    vjust = -0.5, 
    color = "black",
    size = 3.5  # Adjust text size for readability
  ) +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +  # Custom color palette for clarity and print quality
  labs(
    title = "Gender Distribution by Institution Level",
    x = "Institution Level", 
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "plain"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Adjust for better alignment and readability
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )
# Gender Distribution and Residential Status
#Step 1: Prepare the Data
# Remove any extraneous characters and spaces from 'residential status'
data$`residential status` <- gsub("[\r\n]", "", data$`residential status`)
data$`residential status` <- gsub("\\s+$", "", data$`residential status`)

# Check the unique values to ensure they are clean
print(unique(data$`residential status`))

#Step 2: Cross-Tabulation
# Cross-tabulation of Gender and Residential Status
res_status_gender_table <- table(data$gender, data$`residential status`)

# Print the cross-tabulation results to verify
print(res_status_gender_table)



#Create the Visualization

# Professional Bar Chart for Gender Distribution by Residential Status
ggplot(data, aes(x = `residential status`, fill = gender)) +
  geom_bar(position = "dodge", width = 0.7) +  # Adjust bar width for aesthetics
  geom_text(
    aes(label = ..count.., group = gender), 
    stat = "count", 
    position = position_dodge(width = 0.7), 
    vjust = -0.5, 
    color = "black",
    size = 3.5  # Adjust text size for readability
  ) +
  scale_fill_manual(values = c("Male" = "#17becf", "Female" = "#e377c2")) +  # Custom color palette for clarity
  labs(
    title = "Gender Distribution by Residential Status",
    x = "Residential Status", 
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "plain"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Adjust for better alignment and readability
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

#Gender Distribution with Study year
#Step 1: Data Preparation
# Print the column names to verify their accuracy
print(names(data))




# Rename the column
names(data)[names(data) == "studing year"] <- "studying year"

# Verify the change
print(names(data))

# Clean up 'studying year'
data$`studying year` <- gsub("[\r\n]", "", data$`studying year`)
data$`studying year` <- gsub("\\s+$", "", data$`studying year`)

# Check the unique values
print(unique(data$`studying year`))



###Visualization
# Stacked Bar Chart for Gender Distribution by Residential Status and Studying Year
# Stacked Bar Chart with Improved X-Axis Label Readability


library(ggplot2)

# Faceted bar chart for Gender Distribution by Residential Status and Studying Year
ggplot(data, aes(x = `residential status`, fill = gender)) +
  geom_bar(stat = "count", position = "stack", width = 0.7) +  # Use stack for clarity
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_stack(vjust = 0.5),  # Place labels inside the stacked bars
    color = "white", 
    size = 3.5  # Adjust text size
  ) +
  facet_grid(~`studying year`) +  # Facet by the corrected column name
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +  # Custom color palette
  labs(
    title = "Gender Distribution by Residential Status and Studying Year",
    x = "Residential Status", 
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels to 45 degrees for readability
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )














#Statistical Tests:



# Create a contingency table
gender_res_status <- table(data$gender, data$`residential status`)

# Print the contingency table
print(gender_res_status)

# Perform the Chi-square test
chi_test <- chisq.test(gender_res_status)

# Print the test results
print(chi_test)




# Effect Size
library(lsr)
cramer_v <- cramersV(gender_res_status)
print(paste("Cramér’s V:", cramer_v))





# Using Unicode for χ²
cat("χ²(2) = 10.67, p = 0.0048, Cramér’s V = 0.15 (small to medium effect size).\n")


# Interpretation of Effect Size




# Correct the curly apostrophe with a regular apostrophe
text <- "The effect size (Cramér's V = 0.15) suggests that while the association between gender and residential status is statistically significant, the strength of this relationship is small to medium. This indicates that gender accounts for some, but not all, of the variation in residential status."

# Print the text
cat(text)




# Clean column names by removing spaces or special characters
colnames(inst_data) <- gsub(" ", "_", colnames(inst_data))  # Replace spaces with underscores
colnames(inst_data) <- gsub("[^[:alnum:]_]", "", colnames(inst_data))  # Remove non-alphanumeric characters

# Verify the cleaned column names
colnames(inst_data)




# Install required packages if not already installed
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}
library(gt)

# Data for Institution Level vs. Socio-Economic Status
inst_data <- data.frame(
  `Institution Level` = c(rep("Public", 3), rep("Private", 3)),
  `Socio-Economic Status` = rep(c("Lower Class", "Middle Class", "Upper Class"), 2),
  Count = c(120, 80, 50, 60, 100, 90),
  `Proportion (%)` = c(30.0, 20.0, 12.5, 15.0, 25.0, 22.5),
  `95% CI` = c("(25.7 – 34.3)", "(16.1 – 24.4)", "(9.5 – 16.0)",
               "(11.7 – 18.8)", "(21.0 – 29.5)", "(18.8 – 26.7)")
)

# Generate the APA-style table with the correct column names
inst_table <- inst_data %>%
  gt() %>%
  tab_header(
    title = "Table 1: Institution Level vs. Socio-Economic Status"
  ) %>%
  cols_label(
    InstitutionLevel = "Institution Level",
    SocioEconomicStatus = "Socio-Economic Status",
    Count = "Count",
    Proportion = "Proportion (%)",
    X95CI = "95% CI"
  )

# Display the table
inst_table


# Save the APA-style table for Institution Level as an image or HTML
gtsave(inst_table, "Institution_Level_Socio_Status_Table.html")



# Load necessary libraries
library(officer)
library(flextable)
library(readr)

# Read the HTML table file
html_file <- "C:/Users/Acer/OneDrive/Desktop/Final Zamal Sir project 2024 Quota/Quota movement 2024/Institution_Level_Socio_Status_Table.html"

# Note: Read the HTML table into a character vector
html_table <- read_lines(html_file)

# Since the HTML file is not easily converted directly into a flextable,
# you can either manually convert the HTML table into a data frame or,
# if you have the table data in a data frame, proceed with the following:

# For illustration, assume you already have the data in `inst_data`
# (use the real data you have)
inst_data <- data.frame(
  InstitutionLevel = c("Public", "Private", "Public", "Private"),
  SocioEconomicStatus = c("Low", "High", "Medium", "High"),
  Count = c(10, 20, 30, 40),
  Proportion = c(25, 50, 75, 100),
  X95CI = c("10-20", "15-25", "20-30", "25-35")
)

# Convert the data into a flextable
ft_table <- flextable(inst_data)

# Create a Word document
doc <- read_docx()

# Add title to the Word document
doc <- doc %>%
  body_add_par("Institution Level vs. Socio-Economic Status Table", style = "heading 1")

# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(ft_table)

# Save the Word document to the desired location
output_file <- "C:/Users/Acer/OneDrive/Desktop/Institution_Level_Socio_Status_Table.docx"
print(doc, target = output_file)

# Notify the user
cat("Word document created at: ", output_file, "\n")




gtsave(apa_table_inst, "Institution_Level_Socio_Status_Table.png")

gtsave(apa_table_inst, "C:/Users/YourUsername/Documents/Institution_Level_Socio_Status_Table.html")

getwd()

install.packages("webshot2")

# Install webshot2 to save tables as images
install.packages("webshot2")

# Now you can save the table as an image (PNG)
gtsave(apa_table_inst, "Institution_Level_Socio_Status_Table.png")

# Or save it as an HTML file
gtsave(apa_table_inst, "Institution_Level_Socio_Status_Table.html")


# Clean the column names
colnames(inst_data) <- gsub("\\.", " ", colnames(inst_data))  # Replace periods with spaces

# Check the cleaned column names
colnames(inst_data)



# Clean column names
colnames(inst_data) <- gsub("\\s+", " ", colnames(inst_data))  # Replace multiple spaces with a single space
colnames(inst_data) <- trimws(colnames(inst_data))  # Remove leading and trailing spaces
colnames(inst_data) <- gsub(" ", "_", colnames(inst_data))  # Replace spaces with underscores for consistency

# Check the cleaned column names
print(colnames(inst_data))





# Load required library
library(gt)
# Generate APA-style table for Institution Level vs. Socio-Economic Status
apa_table_inst <- inst_data %>%
  gt() %>%
  tab_header(
    title = "Table 1: Institution Level vs. Socio-Economic Status"
  ) %>%
  cols_label(
    Institution_Level = "Institution Level",
    Socio_Economic_Status = "Socio-Economic Status",
    Count = "Count",
    Proportion = "Proportion (%)",
    X95_CI = "95% CI"
  ) %>%
  fmt_number(
    columns = vars(Proportion),
    decimals = 1
  )

# Save the APA-style table as an HTML or PNG file
gtsave(apa_table_inst, "Institution_Level_Socio_Status_Table.html")  # Save as HTML
gtsave(apa_table_inst, "Institution_Level_Socio_Status_Table.png")   # Save as PNG































































































# Data for Educational Discipline vs. Gender
colnames(edu_data)




edu_data <- data.frame(
  `Educational Discipline` = c(
    "Arts, Humanities, and Social Sci.", "Arts, Humanities, and Social Sci.",
    "Biological Science", "Biological Science",
    "Engineering Faculty", "Engineering Faculty",
    "Physical and Mathematical Sci.", "Physical and Mathematical Sci."
  ),
  Gender = c(rep("Female", 4), rep("Male", 4)),
  Count = c(56, 100, 63, 160, 50, 83, 32, 84),
  `Proportion (%)` = c(35.9, 64.1, 28.3, 71.7, 37.6, 62.4, 27.6, 72.4),
  `95% CI` = c(
    "(28.5 – 43.9)", "(56.1 – 71.5)", "(22.7 – 34.5)", "(65.5 – 77.3)",
    "(29.9 – 45.8)", "(54.2 – 70.1)", "(20.1 – 36.2)", "(63.8 – 79.9)"
  )
)

edu_table <- edu_data %>%
  gt() %>%
  tab_header(
    title = "Table 2: Educational Discipline vs. Gender"
  ) %>%
  cols_label(
    `Educational.Discipline` = "Educational Discipline",
    Gender = "Gender",
    Count = "Count",
    `Proportion....` = "Proportion (%)",
    `X95..CI` = "95% CI"
  )



edu_table

# Save the APA-style table for Educational Discipline as an image or HTML
gtsave(edu_table, "Educational_Discipline_Gender_Table.html")




library(officer)

# Create a Word document
doc <- read_docx()

# Add the image to the Word document with alignment
doc <- doc %>%
  body_add_img(src = "Educational_Discipline_Gender_Table.png", width = 6, height = 4) %>%
  body_add_par("", style = "normal")  # Optional: add a new line after the image

# Save the Word document
print(doc, target = "Educational_Discipline_Gender_Table.docx")







# Create a Word document
doc <- read_docx()

# Create a formatting parameter for center alignment
align_center <- fp_align(horizontal = "center")

# Add the image to the Word document with alignment
doc <- doc %>%
  body_add_img(src = "Educational_Discipline_Gender_Table.png", width = 6, height = 4, align = align_center)

# Save the Word document
print(doc, target = "Educational_Discipline_Gender_Table.docx")






































# Effect Size (Cramér's V)
## Install required package if not already installed
if (!requireNamespace("lsr", quietly = TRUE)) {
  install.packages("lsr")
}
library(lsr)

# Data for Institution Level vs. Socio-Economic Status
inst_soc_status_table <- matrix(c(120, 80, 50, 60, 100, 90), nrow = 2, byrow = TRUE)
rownames(inst_soc_status_table) <- c("Public", "Private")
colnames(inst_soc_status_table) <- c("Lower Class", "Middle Class", "Upper Class")

# Perform the Chi-square test
chi_test_inst <- chisq.test(inst_soc_status_table)

# Calculate Cramér's V for Institution Level vs. Socio-Economic Status
cramers_v_inst <- cramersV(inst_soc_status_table)
cat("Cramér's V for Institution Level vs. Socio-Economic Status:", cramers_v_inst, "\n")

# Data for Educational Discipline vs. Gender
edu_gender_table <- matrix(c(56, 100, 63, 160, 50, 83, 32, 84), nrow = 4, byrow = TRUE)
rownames(edu_gender_table) <- c(
  "Arts, Humanities, and Social Sci.",
  "Biological Science",
  "Engineering Faculty",
  "Physical and Mathematical Sci."
)
colnames(edu_gender_table) <- c("Female", "Male")

# Perform the Chi-square test
chi_test_edu <- chisq.test(edu_gender_table)

# Calculate Cramér's V for Educational Discipline vs. Gender
cramers_v_edu <- cramersV(edu_gender_table)
cat("Cramér's V for Educational Discipline vs. Gender:", cramers_v_edu, "\n")

## Install required package if not already installed
if (!requireNamespace("lsr", quietly = TRUE)) {
  install.packages("lsr")
}
library(lsr)

# Data for Institution Level vs. Socio-Economic Status
inst_soc_status_table <- matrix(c(120, 80, 50, 60, 100, 90), nrow = 2, byrow = TRUE)
rownames(inst_soc_status_table) <- c("Public", "Private")
colnames(inst_soc_status_table) <- c("Lower Class", "Middle Class", "Upper Class")

# Perform the Chi-square test
chi_test_inst <- chisq.test(inst_soc_status_table)

# Calculate Cramér's V for Institution Level vs. Socio-Economic Status
cramers_v_inst <- cramersV(inst_soc_status_table)
cat("Cramér's V for Institution Level vs. Socio-Economic Status:", cramers_v_inst, "\n")

# Data for Educational Discipline vs. Gender
edu_gender_table <- matrix(c(56, 100, 63, 160, 50, 83, 32, 84), nrow = 4, byrow = TRUE)
rownames(edu_gender_table) <- c(
  "Arts, Humanities, and Social Sci.",
  "Biological Science",
  "Engineering Faculty",
  "Physical and Mathematical Sci."
)
colnames(edu_gender_table) <- c("Female", "Male")

# Perform the Chi-square test
chi_test_edu <- chisq.test(edu_gender_table)

# Calculate Cramér's V for Educational Discipline vs. Gender
cramers_v_edu <- cramersV(edu_gender_table)
cat("Cramér's V for Educational Discipline vs. Gender:", cramers_v_edu, "\n")


# Install required package if not already installed
if (!requireNamespace("lsr", quietly = TRUE)) {
  install.packages("lsr")
}
library(lsr)

# Data for Institution Level vs. Socio-Economic Status
inst_soc_status_table <- matrix(c(120, 80, 50, 60, 100, 90), nrow = 2, byrow = TRUE)
rownames(inst_soc_status_table) <- c("Public", "Private")
colnames(inst_soc_status_table) <- c("Lower Class", "Middle Class", "Upper Class")

# Perform the Chi-square test
chi_test_inst <- chisq.test(inst_soc_status_table)

# Calculate Cramér's V for Institution Level vs. Socio-Economic Status
cramers_v_inst <- cramersV(inst_soc_status_table)
cat("Cramér's V for Institution Level vs. Socio-Economic Status:", cramers_v_inst, "\n")

# Data for Educational Discipline vs. Gender
edu_gender_table <- matrix(c(56, 100, 63, 160, 50, 83, 32, 84), nrow = 4, byrow = TRUE)
rownames(edu_gender_table) <- c(
  "Arts, Humanities, and Social Sci.",
  "Biological Science",
  "Engineering Faculty",
  "Physical and Mathematical Sci."
)
colnames(edu_gender_table) <- c("Female", "Male")

# Perform the Chi-square test
chi_test_edu <- chisq.test(edu_gender_table)

# Calculate Cramér's V for Educational Discipline vs. Gender
cramers_v_edu <- cramersV(edu_gender_table)
cat("Cramér's V for Educational Discipline vs. Gender:", cramers_v_edu, "\n")








library(gt)
library(dplyr)

# Create a summary table
summary_table <- tibble(
  Comparison = c("Institution Level vs. Socio-Economic Status", "Educational Discipline vs. Gender"),
  Chi_Square_P_Value = c(0.2633, 0.4689),
  Cramers_V = c(0.2594, 0.0934)
)

# Create a gt table
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Summary Table: Chi-Square Test and Cramér's V"
  ) %>%
  cols_label(
    Comparison = "Comparison",
    Chi_Square_P_Value = "Chi-Square P-Value",
    Cramers_V = "Cramér's V"
  )

# Display the table
gt_table





# Step 1: Save the gt table as an image
gtsave(gt_table, "Summary_Table.png")

# Step 2: Create a Word document
library(officer)

doc <- read_docx()

# Add the image to the Word document
doc <- doc %>%
  body_add_img(src = "Summary_Table.png", width = 6, height = 4, align = "center")

# Save the Word document
print(doc, target = "Chi_Square_Cramers_V_Summary_Table.docx")










#T test
# Load necessary libraries
library(dplyr)
# Check the column names of your dataset
colnames(analysis_data)




# Gender Encoding: 1 for Male, 2 for Female
analysis_data <- analysis_data %>%
  mutate(gender = recode(gender, "Male" = 1, "Female" = 2))

# Check the unique values in the gender column
unique(analysis_data$gender)


# If 'age' is a numerical variable, categorize it into age ranges
analysis_data <- analysis_data %>%
  mutate(age_range = case_when(
    age >= 18 & age <= 22 ~ "18-22",
    age >= 23 & age <= 27 ~ "23-27",
    age >= 28 & age <= 32 ~ "28-32",
    age >= 33 & age <= 37 ~ "33-37",
    TRUE ~ "38+"
  ))

# View the new age_range column
head(analysis_data$age_range)







# Perform ANOVA to compare actual_sleep_hours across different age ranges
anova_result <- aov(actual_sleep_hours ~ age_range, data = analysis_data)

# View the summary of ANOVA results
summary(anova_result)




# Load required libraries
library(flextable)
library(officer)

# Example ANOVA Table (replace with your actual data)
anova_table <- data.frame(
  Source = c("Age Range", "Residuals"),
  Df = c(2, 620),
  Sum_Sq = c(6, 3355),
  Mean_Sq = c(3.172, 5.412),
  F_value = c(0.586, NA),
  p_value = c(0.557, NA)
)

# Create a flextable
ft <- flextable(anova_table)

# Apply bold formatting to the headers and align columns
ft <- ft %>%
  compose(j = "Source", value = as_paragraph(as_chunk("Source", props = fp_text(bold = TRUE)))) %>%
  compose(j = "Df", value = as_paragraph(as_chunk("Df", props = fp_text(bold = TRUE)))) %>%
  compose(j = "Sum_Sq", value = as_paragraph(as_chunk("Sum_Sq", props = fp_text(bold = TRUE)))) %>%
  compose(j = "Mean_Sq", value = as_paragraph(as_chunk("Mean_Sq", props = fp_text(bold = TRUE)))) %>%
  compose(j = "F_value", value = as_paragraph(as_chunk("F_value", props = fp_text(bold = TRUE)))) %>%
  compose(j = "p_value", value = as_paragraph(as_chunk("p_value", props = fp_text(bold = TRUE)))) %>%
  align(align = "center", part = "all")

# Save the table to a Word document
doc <- read_docx()
doc <- doc %>% body_add_flextable(ft)

# Save the Word document
file_path <- "APA_ANOVA_Table.docx"
print(doc, target = file_path)

# Inform the user about the saved file path
cat("Your APA formatted table has been saved to:", file_path)









# Assuming 'gender' is encoded as 1 for Male and 2 for Female
# Perform T-test to compare actual_sleep_hours between genders
t_test_result <- t.test(actual_sleep_hours ~ gender, data = analysis_data)

# View T-test results
t_test_result


install.packages("cardx")
library(cardx)

install.packages("gtsummary")
library(gtsummary)


t_test_result <- t.test(actual_sleep_hours ~ gender, data = analysis_data)




# Create a summary table using gtsummary and add t-test
t_test_table <- tbl_summary(
  data = analysis_data,
  by = gender,
  include = actual_sleep_hours,
  statistic = list(all_continuous() ~ "{mean} ({sd})")
) %>%
  modify_header(
    all_stat_cols() ~ "**Mean (SD)**",
    by = "**Group**"
  ) %>%
  add_p(
    test = list(actual_sleep_hours ~ "t.test")
  )

# Display the table
t_test_table









































































###Logistic Regression
# Load necessary libraries
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}
library(gt)

# Convert categorical predictors to factors
data$gender_binary <- ifelse(data$gender == "Male", 1, 0)  # Male = 1, Female = 0
data$`residential status` <- as.factor(data$`residential status`)
data$`institution level` <- as.factor(data$`institution level`)

# Fit logistic regression model
logit_model <- glm(gender_binary ~ `residential status` + `institution level`, 
                   data = data, 
                   family = binomial)

# Summary of the model
summary(logit_model)

# Extract odds ratios and confidence intervals
logit_odds <- exp(coef(logit_model))
logit_ci <- exp(confint(logit_model))
logit_pvalues <- coef(summary(logit_model))[, "Pr(>|z|)"]

# Combine results into a dataframe
logit_results <- data.frame(
  Predictor = names(logit_odds),
  OR = logit_odds,
  CI_Lower = logit_ci[, 1],
  CI_Upper = logit_ci[, 2],
  P_Value = logit_pvalues
)


# Print the results
print(logit_results)
# Save as CSV for easy access
write.csv(logit_results, "Logistic_Regression_Results.csv", row.names = FALSE)
# APA-style table using gt
apa_logit_table <- logit_results %>%
  gt() %>%
  tab_header(
    title = "Table 1: Logistic Regression Results"
  ) %>%
  cols_label(
    Predictor = "Predictor",
    OR = "Odds Ratio (OR)",
    CI_Lower = "95% CI (Lower)",
    CI_Upper = "95% CI (Upper)",
    P_Value = "P-Value"
  ) %>%
  fmt_number(
    columns = vars(OR, CI_Lower, CI_Upper, P_Value),
    decimals = 3
  )

# Save APA-style logistic regression table
gtsave(apa_logit_table, "Logistic_Regression_APA_Table.html")
gtsave(apa_logit_table, "Logistic_Regression_APA_Table.png")





















# Install packages if not already installed
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")

# Load libraries
library(flextable)
library(officer)


# Logistic regression summary as a data frame
logit_summary <- data.frame(
  Term = c("(Intercept)", 
           "Residential Status: On Campus", 
           "Residential Status: With Family", 
           "Institution Level: Public"),
  Estimate = c(-0.02347, 0.22071, -0.52821, 1.00194),
  Std_Error = c(0.23820, 0.21599, 0.25633, 0.20529),
  Z_Value = c(-0.099, 1.022, -2.061, 4.881),
  P_Value = c(0.9215, 0.3069, 0.0393, 1.06e-06),
  Significance = c("", "", "*", "***")
)

# Add significance explanation
logit_summary$Significance_Notes <- c(
  "‘***’ 0.001; ‘**’ 0.01; ‘*’ 0.05; ‘.’ 0.1; ‘ ’ 1"
)


# Create a flextable
logit_table <- flextable(logit_summary) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    Std_Error = "Std. Error",
    Z_Value = "Z Value",
    P_Value = "P Value",
    Significance = "Signif. Code"
  ) %>%
  add_footer_lines("Notes: Significance codes based on conventional thresholds.") %>%
  align(align = "center", part = "all") %>%
  autofit()




# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_par("Summary of Logistic Regression Model", style = "heading 1") %>%
  body_add_flextable(value = logit_table) %>%
  body_add_par("Generated for Lancet Submission", style = "Normal")  # Corrected style name

# Save the document
output_path <- "Logit_Model_Summary.docx"
print(doc, target = output_path)

# Notify the user
cat("The Word document has been saved as:", output_path, "\n")











# Install and load required packages
if (!requireNamespace("gt", quietly = TRUE)) {
  install.packages("gt")
}
library(gt)

# Ensure categorical variables are properly set
data$gender_binary <- ifelse(data$gender == "Male", 1, 0)  # Binary encoding for gender
data$`residential status` <- as.factor(data$`residential status`)
data$`institution level` <- as.factor(data$`institution level`)

# Fit logistic regression model
logit_model <- glm(gender_binary ~ `residential status` + `institution level`, 
                   data = data, 
                   family = binomial)

# Extract odds ratios and confidence intervals
logit_odds <- exp(coef(logit_model))  # Odds ratios
logit_ci <- exp(confint(logit_model)) # Confidence intervals
logit_pvalues <- coef(summary(logit_model))[, "Pr(>|z|)"]  # P-values

# Combine results into a dataframe
logit_results <- data.frame(
  Predictor = names(logit_odds),
  Odds_Ratio = logit_odds,
  CI_Lower = logit_ci[, 1],
  CI_Upper = logit_ci[, 2],
  P_Value = logit_pvalues
)

# Create an APA-style regression table
apa_table <- logit_results %>%
  gt() %>%
  tab_header(
    title = "Table 1: Logistic Regression Results"
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Odds_Ratio = "Odds Ratio (OR)",
    CI_Lower = "95% CI (Lower)",
    CI_Upper = "95% CI (Upper)",
    P_Value = "P-Value"
  ) %>%
  fmt_number(
    columns = vars(Odds_Ratio, CI_Lower, CI_Upper, P_Value),
    decimals = 3
  )

# Save APA-style logistic regression table
gtsave(apa_table, "APA_Regression_Table.html")  # Save as HTML
gtsave(apa_table, "APA_Regression_Table.png")   # Save as PNG









#Encode Categorical Variables
data$gender_binary <- ifelse(data$gender == "Male", 1, 0)  # Binary coding for gender
data$`residential status` <- as.factor(data$`residential status`)
data$`studing year` <- as.factor(data$`studing year`)

 


print(colnames(data))


#colnames(data) <- gsub(" ", "_", colnames(data)) # Replace spaces with underscores
colnames(data) <- gsub("[^a-zA-Z0-9_]", "", colnames(data)) # Remove special characters
colnames(data) <- tolower(colnames(data)) # Convert to lowercase

# Print the cleaned column names
print(colnames(data))




# Rename columns for better readability
data <- data %>%
  rename(
    age = age,
    gender = gender,
    residential_status = residentialstatus,
    socioeconomic_status = socioeconomicstatus,
    institution_level = institutionlevel,
    educational_discipline = educationaldiscipline,
    studying_year = studyingyear,
    awareness_status = awarenessstatus,
    self_rated_mental_health = selfratedmentalhealth,
    quota_stressor = quotastressor,
    stress_symptoms = stresssymptomsduetoquotamovement,
    stress_impact_on_academics = perceivedimpactofstressonacademicperformance,
    mental_health_help = mentalhealthhelpseekingbehaviour,
    university_support = perceivedadequacyofuniversitysupport,
    desired_support = desiredmentalhealthsupport,
    stress_coping_strategies = stresscopingstrategiesforquotamovement,
    coping_effectiveness = howeffectivehavethesecopingmechanismsbeenforyou,
    sleep_quality = `22duringthepastmonthhowwouldyourateyouroverallsleepquality`,
    sleep_latency_minutes = `23duringthepastmonthhowlonginminuteshasitusuallytakenyoutofallasleepeachnightsleeplatency`,
    trouble_sleeping = `24duringthepastmonthhowoftenhaveyouhadtroublesleepingbecauseyou30minsleeplatency`,
    actual_sleep_hours = `24duringthepastmonthhowmanyhoursofactualsleepdidyougetatnightthismaybedifferentthanthenumberofhoursyouspendinbedsleepduration`,
    bedtime = `25duringthepastmonthwhenhaveyouusuallygonetobedatnightsleepefficiency`,
    wakeup_time = `26duringthepastmonthwhenhaveyouusuallygottenupinthemorningsleepefficiency`,
    trouble_sleeping_details = `5aduringhadtroublesleeping`,
    medicine_taken = `28takenmedicineprescribedoroverthecounter`,
    daytime_dysfunction = `29duringthepastmonthhowoftenhaveyouhadtroublestayingawakewhiledrivingeatingmealsorengaginginsocialactivitiesdaytimedysfunction`,
    enthusiasm_issues = `30duringthepastmonthhowmuchofaproblemhasitbeenforyoutokeepupenthusiasmtogetthingsdone`,
    gender_binary = gender_binary
  )

# Print the updated column names
print(colnames(data))





#: Run the Logistic Regression Model
logit_model <- glm(gender_binary ~ residential_status + studying_year, 
                   data = data, 
                   family = binomial)
summary(logit_model)



## Odds ratios
exp_coef <- exp(coef(logit_model))

# Confidence intervals
conf_int <- exp(confint(logit_model))

# Combine results into a table
logit_results <- data.frame(
  Predictor = names(exp_coef),
  Odds_Ratio = exp_coef,
  CI_Lower = conf_int[, 1],
  CI_Upper = conf_int[, 2]
)
print(logit_results)




#Calculate Odds Ratios and Confidence Intervals
# Calculate odds ratios (OR) and 95% confidence intervals (CI)
logit_or <- exp(coef(logit_model))
logit_ci <- exp(confint(logit_model))

# Prepare the result data frame
logit_results <- data.frame(
  Predictor = names(logit_or),
  Odds_Ratio = logit_or,
  CI_Lower = logit_ci[, 1],
  CI_Upper = logit_ci[, 2],
  P_value = round(summary(logit_model)$coefficients[, 4], 3)
)

# Print results
print(logit_results)



#Create the Flextable for the Word Document


# Now create the Word document
doc <- read_docx() %>%
  body_add_par("Logistic Regression Analysis of Gender and Predictors", style = "heading 1") %>%
  body_add_par("This report presents the results of the logistic regression analysis for the gender binary outcome.", style = "Normal") %>%
  body_add_flextable(value = logit_flextable) %>%
  body_add_par("Interpretation and Conclusion", style = "heading 2") %>%
  body_add_par("The following section presents the interpretation of the results and their implications.", style = "Normal") %>%
  body_add_par("Significant predictors include residential status and studying year, indicating that these variables are crucial for understanding gender outcomes in this context. Non-significant predictors may be due to limited variability or other underlying factors that were not captured in the model.", style = "Normal") %>%
  body_add_par("Limitations of the analysis include potential unmeasured confounders and sample size constraints. Further studies with larger sample sizes and additional control variables may help refine the findings.", style = "Normal") %>%
  body_add_par("The findings of this analysis contribute to understanding the factors influencing gender differences in mental health outcomes, specifically in the context of socio-economic and academic pressures.", style = "Normal")

# Save the document
print(doc, target = "Logistic_Regression_Report_Lancet.docx")




getwd()












#Visualize the results using ggplot2:
library(ggplot2)
library(dplyr)
# Extracting results from the logistic regression model
logit_summary <- summary(logit_model)



# Creating a data frame with odds ratios and confidence intervals
logit_results_df <- data.frame(
  Predictor = rownames(logit_summary$coefficients),
  Odds_Ratio = exp(logit_summary$coefficients[, "Estimate"]),
  CI_Lower = exp(logit_summary$coefficients[, "Estimate"] - 1.96 * logit_summary$coefficients[, "Std. Error"]),
  CI_Upper = exp(logit_summary$coefficients[, "Estimate"] + 1.96 * logit_summary$coefficients[, "Std. Error"])
)

# Remove the intercept row if present
logit_results_df <- logit_results_df[-1, ]

# Create the forest plot
ggplot(logit_results_df, aes(x = Predictor, y = Odds_Ratio, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_pointrange() +
  coord_flip() +  # Flips the coordinates for better readability
  labs(title = "Forest Plot of Logistic Regression Results",
       x = "Predictor",
       y = "Odds Ratio (with 95% CI)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjusts the x-axis text for better readability

library(ggplot2)
library(dplyr)

# Extracting results from the logistic regression model
logit_summary <- summary(logit_model)

# Creating a data frame with odds ratios and confidence intervals
logit_results_df <- data.frame(
  Predictor = rownames(logit_summary$coefficients),
  Odds_Ratio = exp(logit_summary$coefficients[, "Estimate"]),
  CI_Lower = exp(logit_summary$coefficients[, "Estimate"] - 1.96 * logit_summary$coefficients[, "Std. Error"]),
  CI_Upper = exp(logit_summary$coefficients[, "Estimate"] + 1.96 * logit_summary$coefficients[, "Std. Error"])
)

# Remove the intercept row if present
logit_results_df <- logit_results_df[-1, ]

# Create the forest plot
ggplot(logit_results_df, aes(x = Predictor, y = Odds_Ratio, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_pointrange() +
  coord_flip() +  # Flips the coordinates for better readability
  labs(title = "Forest Plot of Logistic Regression Results",
       x = "Predictor",
       y = "Odds Ratio (with 95% CI)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjusts the x-axis text for better readability

library(ggplot2)
library(dplyr)

# Extracting results from the logistic regression model
logit_summary <- summary(logit_model)

# Creating a data frame with odds ratios and confidence intervals
logit_results_df <- data.frame(
  Predictor = rownames(logit_summary$coefficients),
  Odds_Ratio = exp(logit_summary$coefficients[, "Estimate"]),
  CI_Lower = exp(logit_summary$coefficients[, "Estimate"] - 1.96 * logit_summary$coefficients[, "Std. Error"]),
  CI_Upper = exp(logit_summary$coefficients[, "Estimate"] + 1.96 * logit_summary$coefficients[, "Std. Error"])
)

# Remove the intercept row if present
logit_results_df <- logit_results_df[-1, ]

# Create the forest plot
ggplot(logit_results_df, aes(x = Predictor, y = Odds_Ratio, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_pointrange() +
  coord_flip() +  # Flips the coordinates for better readability
  labs(title = "Forest Plot of Logistic Regression Results",
       x = "Predictor",
       y = "Odds Ratio (with 95% CI)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjusts the x-axis text for better readability







#Customize
# Load necessary library for margin adjustment
library(grid)








library(grid)

# Create the forest plot with customized features
ggplot(logit_results_df, aes(x = Predictor, y = Odds_Ratio, ymin = CI_Lower, ymax = CI_Upper)) +
  geom_pointrange(color = "steelblue", size = 1.2) +  # Pointrange with color and size
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Add a reference line at OR=1
  coord_flip() +  # Flips the coordinates for better readability
  labs(
    title = "Forest Plot of Logistic Regression Results: Predicting Gender Outcomes",
    subtitle = "Odds Ratios and 95% Confidence Intervals",
    x = "Predictors",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with larger base size for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Adjust x-axis labels for better readability
    axis.text.y = element_text(size = 12),  # Adjust y-axis labels
    plot.title = element_text(face = "bold", size = 16),  # Bold and size adjustment for the title
    plot.subtitle = element_text(size = 14),  # Subtitle size adjustment
    plot.margin = unit(c(10, 10, 10, 10), "pt")  # Correct margin units
  ) +
  scale_x_discrete(limits = rev(logit_results_df$Predictor))  # Reverse x-axis to have the most important predictor at the top






#Coefficient Plot


library(ggplot2)
library(dplyr)

# Create the coefficient plot with customized features
ggplot(logit_results_df, aes(x = Odds_Ratio, y = reorder(Predictor, Odds_Ratio))) +
  geom_point(color = "steelblue", size = 3) +  # Points for the Odds Ratios
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "darkblue") +  # Confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a reference line at OR=1
  labs(
    title = "Coefficient Plot of Logistic Regression: Predicting Gender Outcomes",
    subtitle = "Odds Ratios with 95% Confidence Intervals",
    x = "Odds Ratio (95% CI)",
    y = "Predictors"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme for clarity
  theme(
    axis.text.x = element_text(size = 12, color = "black"),  # X-axis labels
    axis.text.y = element_text(size = 12, color = "black"),  # Y-axis labels
    plot.title = element_text(face = "bold", size = 16, color = "black"),  # Title formatting
    plot.subtitle = element_text(size = 14, color = "black"),  # Subtitle formatting
    plot.margin = unit(c(10, 10, 10, 10), "pt"),  # Adjust plot margins for better spacing
    panel.grid.major = element_blank(),  # Remove major grid lines for clean aesthetics
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_x_continuous(limits = c(0, max(logit_results_df$CI_Upper) + 1), expand = c(0, 0))  # Adjust X-axis limits




#Mindmap
install.packages("igraph")

library(igraph)













# Define nodes and edges
nodes <- c("Main Topic", "Subtopic 1", "Subtopic 2", "Subtopic 3", 
           "Point 1.1", "Point 1.2", "Point 2.1", "Point 2.2", 
           "Point 3.1", "Point 3.2")

edges <- c("Main Topic", "Subtopic 1", "Main Topic", "Subtopic 2", 
           "Main Topic", "Subtopic 3", "Subtopic 1", "Point 1.1", 
           "Subtopic 1", "Point 1.2", "Subtopic 2", "Point 2.1", 
           "Subtopic 2", "Point 2.2", "Subtopic 3", "Point 3.1", 
           "Subtopic 3", "Point 3.2")

# Create the graph
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)

# Plot the graph with a customized layout
plot(graph, layout = layout_with_fr, 
     vertex.size = 30, vertex.color = "lightblue", 
     edge.arrow.size = 0.5, vertex.label.cex = 1.2, 
     vertex.label.color = "black", edge.color = "gray")














# Define nodes based on your dataset structure
nodes <- c("Psychological Health of Bangladeshi University Students", 
           "Demographic Information", "Stress Factors", "Health Outcomes", 
           "Age", "Gender", "Socio-economic status", 
           "Quota stressor", "Stress symptoms", 
           "Self-rated mental health", "Sleep duration", "Coping strategies")

# Define the edges to connect the nodes (relationships between topics and subtopics)
edges <- c("Psychological Health of Bangladeshi University Students", "Demographic Information", 
           "Psychological Health of Bangladeshi University Students", "Stress Factors", 
           "Psychological Health of Bangladeshi University Students", "Health Outcomes", 
           "Demographic Information", "Age", 
           "Demographic Information", "Gender", 
           "Demographic Information", "Socio-economic status", 
           "Stress Factors", "Quota stressor", 
           "Stress Factors", "Stress symptoms", 
           "Health Outcomes", "Self-rated mental health", 
           "Health Outcomes", "Sleep duration", 
           "Health Outcomes", "Coping strategies")

# Create the graph from edges
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)

# Plot the graph with a customized layout
plot(graph, layout = layout_with_fr, 
     vertex.size = 30, vertex.color = "lightblue", 
     edge.arrow.size = 0.5, vertex.label.cex = 1.2, 
     vertex.label.color = "black", edge.color = "gray")







# Define nodes based on your dataset structure, excluding "Ethical Clearance"
nodes <- c("Psychological Health of Bangladeshi University Students", 
           "Demographic Information", "Stress Factors", "Health Outcomes", 
           "Age", "Gender", "Socio-economic status", 
           "Quota stressor", "Stress symptoms", 
           "Self-rated mental health", "Sleep duration", "Coping strategies")

# Define the edges to connect the nodes (relationships between topics and subtopics)
edges <- c("Psychological Health of Bangladeshi University Students", "Demographic Information", 
           "Psychological Health of Bangladeshi University Students", "Stress Factors", 
           "Psychological Health of Bangladeshi University Students", "Health Outcomes", 
           "Demographic Information", "Age", 
           "Demographic Information", "Gender", 
           "Demographic Information", "Socio-economic status", 
           "Stress Factors", "Quota stressor", 
           "Stress Factors", "Stress symptoms", 
           "Health Outcomes", "Self-rated mental health", 
           "Health Outcomes", "Sleep duration", 
           "Health Outcomes", "Coping strategies")

# Create the graph from edges
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)

# Plot the graph with a customized layout, ensuring no overlap
plot(graph, layout = layout_with_fr, 
     vertex.size = 30, vertex.color = "lightblue", 
     edge.arrow.size = 0.5, vertex.label.cex = 1.2, 
     vertex.label.color = "black", edge.color = "gray",
     vertex.label.dist = 2,  # Add distance to avoid overlap
     vertex.label.degree = 0.5)  # Adjust angle of labels to reduce overlap



































# Define nodes based on your dataset structure, shortening the "Psychological Health of Bangladeshi University Students" label
nodes <- c("Psych. Health",  # Shortened name for the main topic
           "Demographic Information", "Stress Factors", "Health Outcomes", 
           "Age", "Gender", "Socio-economic status", 
           "Quota stressor", "Stress symptoms", 
           "Self-rated mental health", "Sleep duration", "Coping strategies")

# Define the edges to connect the nodes (relationships between topics and subtopics)
edges <- c("Psych. Health", "Demographic Information", 
           "Psych. Health", "Stress Factors", 
           "Psych. Health", "Health Outcomes", 
           "Demographic Information", "Age", 
           "Demographic Information", "Gender", 
           "Demographic Information", "Socio-economic status", 
           "Stress Factors", "Quota stressor", 
           "Stress Factors", "Stress symptoms", 
           "Health Outcomes", "Self-rated mental health", 
           "Health Outcomes", "Sleep duration", 
           "Health Outcomes", "Coping strategies")

# Create the graph from edges
graph <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)

# Plot the graph with a customized layout, ensuring no overlap
plot(graph, layout = layout_with_fr(graph, niter = 200),  # Increase iterations for better layout
     vertex.size = 30, vertex.color = "lightblue", 
     edge.arrow.size = 0.5, vertex.label.cex = 1.2, 
     vertex.label.color = "black", edge.color = "gray",
     vertex.label.dist = 2,  # Add distance to avoid overlap
     vertex.label.degree = 0.5,  # Adjust angle of labels to reduce overlap
     main = "Mind Map: Psychological Health of Bangladeshi University Students", 
     submain = "Key Factors and Influences on Psychological Health", 
     margins = c(5, 5, 5, 5))  # Adjust margins for better spacing
















install.packages("ggraph")


library(igraph)
library(ggraph)

# Example: Define a graph object (you should use your actual data)
nodes <- data.frame(
  name = c(
    "Socio-economic Status",
    "Age",
    "Gender",
    "Demographic Information",
    "Stress Factors",
    "Stress Symptoms",
    "Quota Stressor",
    "Psych Health",  # Shortcut label
    "Health Outcomes",
    "Self-rated Mental Health",
    "Coping Strategies",
    "Sleep Duration"
  )
)

edges <- data.frame(
  from = c("Socio-economic Status", "Age", "Gender", "Demographic Information", 
           "Stress Factors", "Stress Factors", "Stress Factors", "Psych Health", 
           "Health Outcomes", "Health Outcomes"),
  to = c("Demographic Information", "Demographic Information", "Demographic Information", 
         "Psych Health", "Stress Symptoms", "Quota Stressor", "Psych Health", 
         "Health Outcomes", "Self-rated Mental Health", "Coping Strategies")
)

# Create a graph object
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# Generate a network plot
ggraph(graph, layout = "fr") +  # "fr" is Fruchterman-Reingold layout for better spacing
  geom_edge_link(arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
                 end_cap = circle(3, 'mm'), color = "gray70") +  # Edges
  geom_node_point(size = 8, color = "lightblue") +  # Nodes
  geom_node_text(aes(label = name), 
                 repel = TRUE,  # Repel labels to avoid overlap
                 size = 4, 
                 color = "black") +
  labs(title = "Psychological Health Network", subtitle = "Bangladeshi University Students") +
  theme_void() +  # Minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkgray")
  )
















####Day two
##Define the file path
file_path <- "C:/Users/Acer/OneDrive/Desktop/CV/Book2/Book2.xlsx"
 # Import data from Sheet 7
data <- read_excel(file_path, sheet = 7)
# Check column names
 print(colnames(data))

 
 
 
 
 
 
 
 
 # Clean column names by replacing or removing unwanted characters
 colnames(data) <- gsub("[\r\n]", "", colnames(data))  # Remove newline characters
 colnames(data) <- gsub(" ", "_", colnames(data))  # Replace spaces with underscores
 colnames(data) <- gsub("\\(", "", colnames(data))  # Remove open parentheses
 colnames(data) <- gsub("\\)", "", colnames(data))  # Remove closing parentheses
 
 
 # View cleaned column names
 colnames(data)
 
 
 
 # Remove numeric prefixes and additional details from column names
 colnames(data) <- gsub("^\\d+[a-zA-Z]*", "", colnames(data))  # Remove numeric prefixes (e.g., "22During")
 colnames(data) <- gsub("_+", "_", colnames(data))  # Replace multiple underscores with a single one
 colnames(data) <- gsub("^_|_$", "", colnames(data))  # Remove leading and trailing underscores
 
 # Manually rename ambiguous columns for clarity
 colnames(data)[which(colnames(data) == "5b")] <- "Sleep_Trouble_Reason_1"
 colnames(data)[which(colnames(data) == "5c")] <- "Sleep_Trouble_Reason_2"
 # Repeat for other "5d", "5e", etc., as needed
 
 # View cleaned column names
 colnames(data)
 
 
 
 # Replace unnamed columns with placeholders
 colnames(data)[colnames(data) == ""] <- paste0("Unnamed_Column", seq_len(sum(colnames(data) == "")))
 
 # Clean column names
 colnames(data) <- gsub("^the_past_month,", "", colnames(data))  # Remove repetitive prefix
 colnames(data) <- gsub("[\"\\?]", "", colnames(data))           # Remove special characters
 colnames(data) <- gsub(" ", "_", colnames(data))                # Replace spaces with underscores
 colnames(data) <- gsub("-", "_", colnames(data))                # Replace hyphens with underscores
 colnames(data) <- tolower(colnames(data))                       # Convert to lowercase for consistency
 
 # Rename specific columns for clarity
 colnames(data) <- gsub("sleep_quality", "sleep_quality_rating", colnames(data))
 colnames(data) <- gsub("sleep_latency", "sleep_fall_time", colnames(data))
 colnames(data) <- gsub("30_min_sleep_latency", "sleep_delay_frequency", colnames(data))
 
 # View cleaned column names
 colnames(data)
 
 
 
 
 # Clean specific problematic column names
 colnames(data) <- gsub("^_", "", colnames(data))  # Remove leading underscores
 colnames(data) <- gsub("how_would_you_rate_your_overall_sleep_quality_rating", "sleep_quality_rating", colnames(data))
 colnames(data) <- gsub("how_long_in_minutes_has_it_usually_taken_you_to_fall_asleep_each_nightsleep_fall_time", "sleep_latency_minutes", colnames(data))
 colnames(data) <- gsub("how_often_have_you_had_trouble_sleeping_because_you_30_minsleep_fall_time", "trouble_sleep_frequency", colnames(data))
 colnames(data) <- gsub("how_many_hours_of_actual_sleep_did_you_get_at_night_this_may_be_different_than_the_number_of_hours_you_spend_in_bedsleep_duration", "actual_sleep_hours", colnames(data))
 colnames(data) <- gsub("when_have_you_usually_gone_to_bed_at_nightsleep_efficiency", "bedtime_efficiency", colnames(data))
 colnames(data) <- gsub("when_have_you_usually_gotten_up_in_the_morningsleep_efficiency", "wake_up_efficiency", colnames(data))
 colnames(data) <- gsub("taken_medicine_prescribed_or_over_the_counter", "medicine_use", colnames(data))
 colnames(data) <- gsub("how_often_have_you_had_trouble_staying_awake_while_driving,_eating_meals,_or_engaging_in_social_activitiesdaytime_dysfunction", "daytime_dysfunction_frequency", colnames(data))
 colnames(data) <- gsub("how_much_of_a_problem_has_it_been_for_you_to_keep_up_enthusiasm_to_get_things_done", "enthusiasm_problem", colnames(data))
 
 # Rename unnamed columns
 colnames(data)[grepl("^unnamed_column", colnames(data))] <- paste0("extra_column", seq_len(sum(grepl("^unnamed_column", colnames(data)))))
 
 # View the final cleaned column names
 colnames(data)
 
 
 
 
 
 #Address Double Underscore in
 
 colnames(data) <- gsub("mental_health_help__seeking_behaviour", "mental_health_help_seeking_behaviour", colnames(data))
 
 #Verifying extra column
 data %>% select(starts_with("extra_column")) %>% summary()
 
 
 
 #Unique
 data %>% select(starts_with("extra_column")) %>% summarise(across(everything(), ~ unique(.)))
 
 
 data %>% 
   select(starts_with("extra_column")) %>% 
   summarise(across(everything(), ~ sum(!is.na(.))))
 
 
 
 
 
 #Ready-to-Analyze Dataset
 
 analysis_data <- data %>% 
   select(age, gender, residential_status, socio_economic_status, 
          institution_level, educational_discipline)
 
 str(analysis_data)
 summary(analysis_data)
 
 #Convert to Factors
 analysis_data <- analysis_data %>%
   mutate(
     age = as.factor(age),
     gender = as.factor(gender),
     residential_status = as.factor(residential_status),
     socio_economic_status = as.factor(socio_economic_status),
     institution_level = as.factor(institution_level),
     educational_discipline = as.factor(educational_discipline)
   )
 
 
 
 
 
 #Verify the Structure Again
 str(analysis_data)
 summary(analysis_data)
 
 
 
 #Steps to Fix and Finalize the Data
 #1. Clean educational_discipline
 
 analysis_data <- analysis_data %>%
   mutate(
     educational_discipline = recode(
       educational_discipline,
       "Physical and Mathmatecial Sciences\r\n" = "Physical and Mathematical Sciences"
     )
   )
 
 
 
 #2. Verify Factor Levels
 levels(analysis_data$educational_discipline)
 
 
 
 ###Combine Two level
 analysis_data <- analysis_data %>%
   mutate(educational_discipline = recode(educational_discipline,
                                          "Physical and Mathmatecial Sciences" = "Physical and Mathematical Sciences"))
 ##Verify level
 levels(analysis_data$educational_discipline)
 
 
 
 
 analysis_data <- analysis_data %>%
   mutate(educational_discipline = recode(educational_discipline,
                                          "Physical and Mathmatecial Sciences" = "Physical and Mathematical Sciences",
                                          "Physical and Mathmatecial Sciences\r\n" = "Physical and Mathematical Sciences"))
 
 
 
 
 
 levels(analysis_data$educational_discipline)
 
 colnames(analysis_data)
 # Check column names in the original dataset
 colnames(data)
 
 
 
 
 
 
 # Add actual_sleep_hours column from the original dataset to analysis_data
 analysis_data <- data %>%
   select(age, gender, residential_status, socio_economic_status, 
          institution_level, educational_discipline, actual_sleep_hours)
 
 # Fit the multiple regression model again
 sleep_model <- lm(actual_sleep_hours ~ age + gender + residential_status + socio_economic_status + 
                     institution_level + educational_discipline, data = analysis_data)
 
 # Display the model summary
 summary(sleep_model)
 
 
 
 
 #Create and Export APA Table
 
 
 # Create the regression table
 regression_table <- data.frame(
   Predictor = c("(Intercept)", "Age (24-28)", "Age (29-33)", "Gender (Male)", 
                 "Residential Status (On Campus)", "Residential Status (With Family)", 
                 "Socio-economic Status (Middle Class)", "Socio-economic Status (Upper Class)", 
                 "Institution Level (Public)", "Educational Discipline (Biological Science)", 
                 "Educational Discipline (Engineering Faculty)", "Educational Discipline (Physical and Mathematical Sciences)", 
                 "Educational Discipline (Physical and Mathematical Sciences) (Additional)"),
   Estimate = c(5.523903, 0.232358, -0.300184, 0.519565, 0.161078, 0.274334, 0.715963, 
                0.801684, 0.003508, -0.390253, 0.095105, -0.892820, -2.801746),
   StdError = c(0.442341, 0.258193, 0.624694, 0.211941, 0.229438, 0.282876, 0.206220, 
                0.333640, 0.329236, 0.244285, 0.335724, 0.285510, 2.300931),
   tValue = c(12.488, 0.900, -0.481, 2.451, 0.702, 0.970, 3.472, 2.403, 0.011, 
              -1.598, 0.283, -3.127, -1.218),
   pValue = c("< 2e-16", "0.368", "0.631", "0.014", "0.483", "0.333", "0.000553", 
              "0.0166", "0.991", "0.110664", "0.777", "0.001849", "0.223825")
 )
 
 # Adjust formatting for APA-style presentation
 regression_table$Estimate <- round(regression_table$Estimate, 3)
 regression_table$StdError <- round(regression_table$StdError, 3)
 regression_table$tValue <- round(regression_table$tValue, 3)
 
 # Correct the pValue formatting
 regression_table$pValue <- ifelse(regression_table$pValue == "< 2e-16", "< .001", regression_table$pValue)
 
 # View the adjusted table
 regression_table
 
 
 
 
 
 
 
 
 install.packages("officer")
 
 
 # Load necessary library
 library(officer)
 
 # Create a Word document
 doc <- read_docx()
 
 # Add a title to the document
 doc <- doc %>% 
   body_add_par("Regression Results for Predicting Actual Sleep Hours", style = "heading 1")
 
 # Add the regression table to the document
 doc <- doc %>%
   body_add_table(regression_table, style = "table_template")
 
 # Save the document to a file
 print(doc, target = "APA_Regression_Results.docx")
 
 
 
 
 
 
 #Bar Plot or Coefficient Plot (Visualizing Regression Coefficients)
 
 # Load necessary libraries
 library(ggplot2)
 library(broom)
 
 # Extract regression coefficients and tidy them
 tidy_results <- tidy(sleep_model)
 
 # Plot the regression coefficients
 ggplot(tidy_results, aes(x = term, y = estimate, color = p.value < 0.05)) +
   geom_point(size = 3) +
   geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
   theme_minimal() +
   coord_flip() +
   labs(title = "Regression Coefficients for Predicting Actual Sleep Hours", 
        x = "Predictors", 
        y = "Coefficient Estimate") +
   scale_color_manual(values = c("black", "red")) +  # Color significant results in red
   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
 
 
 
 
 # Select only numeric columns for correlation matrix
 numeric_data <- analysis_data %>% select(age, socio_economic_status, average_sleep_duration)
 
 # Calculate the correlation matrix
 cor_matrix <- cor(numeric_data)
 
 # Plot the correlation matrix using corrplot
 library(corrplot)
 corrplot(cor_matrix, method = "circle", type = "upper", 
          tl.cex = 0.8, tl.col = "black", addCoef.col = "black")
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 