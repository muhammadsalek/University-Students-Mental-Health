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
install.packages("flextable")
install.packages("openxlsx")
install.packages("ResourceSelection")
install.packages("stargazer")
# Load packages
library(dplyr)        # for data manipulation
library(tidyverse)    # includes ggplot2, dplyr, tidyr, etc.
library(lubridate)    # for date manipulation
library(readxl)       # for reading Excel files
library(ggplot2)
library(caret)
library(randomForest)
library(psych)
library(knitr)
library(corrplot)
library(modelr)
library(tableone)
library(flextable)
library(openxlsx)
library(ResourceSelection)


#Data Import
data <- read.xlsx("E:\\Final Zamal Sir project 2024 Quota\\Book2.xlsx", sheet = "RowData")
####Prepare data####
head(data)

glimpse(data)
summary(data)

#### Convert necessary variables to factors####
data$Participation.Status <- as.factor(data$Participation.Status)
data$Gender <- as.factor(data$Gender)
data$Residential.status <- as.factor(data$Residential.status)
data$`Socio-economic.Status` <- as.factor(data$`Socio-economic.Status`)
data$`Self-Rated.Mental.Health` <- as.factor(data$`Self-Rated.Mental.Health`)




#Create the Logistic Regression Model
# Fit logistic regression model
model <- glm(Participation.Status ~ Gender + Residential.status + `Socio-economic.Status` + `Self-Rated.Mental.Health`, 
             data = data, family = binomial())

# Model summary to see coefficients and statistics
summary(model)



####Creating the Table in R####
#### Create a regression table with p-values included####
table <- tbl_regression(
  model, 
  exponentiate = FALSE,  # Report coefficients (log-odds) instead of odds ratios
  conf.int = TRUE,       # Include confidence intervals
  pvalue_fun = ~style_pvalue(.x, digits = 3)  # Format p-values with 3 decimal places
) %>%
  bold_labels() %>%         # Bold variable labels
  modify_header(label ~ "Variable", p.value ~ "P-value") %>%  # Rename headers
  modify_footnote(
    all_stat_cols() ~ "Significance codes: *p<0.05; **p<0.01; ***p<0.001"
  ) %>%
  modify_table_styling(
    columns = "p.value",  # Target the p-value column
    rows = p.value < 0.05,  # Conditional bold for p-values < 0.05
    text_format = "bold"   # Apply bold formatting
  )

# Convert gtsummary table to a flextable
flex_table <- as_flex_table(table)

# Create a Word document and add the flextable
read_docx() %>%
  body_add_flextable(value = flex_table) %>%
  print(target = "model_summary_with_bold_pvalues.docx")

# Success message
cat("The table with bold p-values (<0.05) has been saved as 'model_summary_with_bold_pvalues.docx'")







####Visualization####


# Load required libraries
library(ggplot2)
library(dplyr)
library(forcats)

# Data for the plots (replace these values with actual coefficients and CI if different)
data <- tibble::tibble(
  Variable = c(
    "Gender: Female", "Gender: Male",
    "Residential: On Campus", "Residential: With Family",
    "Socio-economic: Middle Class", "Socio-economic: Upper Class",
    "Self-Rated Mental Health: Excellent", "Self-Rated Mental Health: Good",
    "Self-Rated Mental Health: Poor", "Self-Rated Mental Health: Very Poor"
  ),
  Coefficient = c(3.2, 4.2, -0.21, -0.46, -0.40, 0.60, 0.83, 0.08, 0.09, 1.2),
  Lower_CI = c(-0.29, 0.71, -1.6, -1.9, -1.5, -0.94, -1.2, -0.95, -1.3, -0.48),
  Upper_CI = c(6.8, 7.8, 0.93, 0.94, 0.60, 2.6, 4.2, 1.2, 2.0, 4.1),
  P_value = c(0.049, 0.010, 0.739, 0.518, 0.435, 0.484, 0.515, 0.878, 0.915, 0.254)
)

# Create Coefficient Plot
coef_plot <- ggplot(data, aes(x = fct_reorder(Variable, Coefficient), y = Coefficient)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "darkgray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Coefficient Plot",
    x = "Variables",
    y = "Log Odds Ratio (Coefficient)"
  )

# Display Coefficient Plot
print(coef_plot)

# Create Forest Plot
forest_plot <- ggplot(data, aes(x = fct_reorder(Variable, Coefficient), y = Coefficient)) +
  geom_point(aes(color = P_value < 0.05), size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "darkgray"),
    labels = c("Significant", "Not Significant")
  ) +
  theme_minimal() +
  labs(
    title = "Forest Plot",
    x = "Variables",
    y = "Log Odds Ratio (Coefficient)",
    color = "Significance"
  )

# Display Forest Plot
print(forest_plot)




# Extract numeric predictors from your dataset
data_frame_with_predictors <- data %>%
  select_if(is.numeric)
install.packages("ggcorrplot")
####Correlation matrix####
library(ggcorrplot)
cor_matrix <- cor(data_frame_with_predictors)
ggcorrplot(cor_matrix, lab = TRUE)


library(ggcorrplot)

# Plot the correlation matrix
ggcorrplot(cor_matrix, 
           lab = TRUE, 
           type = "lower", 
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix of Predictors")






names(model$model)












# Assuming 'Gender' and 'Residential status' are your variables
table_data <- table(data$Gender, data$Residential_status)
chisq.test(table_data)




names(data)






















library(dplyr)

# Example for grouping by Gender and calculating the distribution of Awareness Status
data_summary <- data %>%
  group_by(Gender) %>%
  summarize(
    Total = n(),
    Aware = sum(Awareness.Status == "Yes", na.rm = TRUE),
    Not_Aware = sum(Awareness.Status == "No", na.rm = TRUE)
  ) %>%
  mutate(
    Aware_Percent = Aware / Total * 100,
    Not_Aware_Percent = Not_Aware / Total * 100
  )

# Chi-square test for Gender vs Awareness Status
gender_awareness_table <- table(data$Gender, data$Awareness.Status)
chisq_test_gender_awareness <- chisq.test(gender_awareness_table)

# Output the table and test result
data_summary
chisq_test_gender_awareness




library(knitr)
library(kableExtra)

# Corrected call to kable with proper header configuration
kable(data_summary, caption = "Distribution of Awareness by Gender") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 1, "Counts" = 2, "Percentages" = 2, " " = 1)) %>% # Adjust to ensure headers match columns
  footnote(general = "Chi-square test result: p-value = [insert p-value here]",
           general_title = "Note: ",
           footnote_as_chunk = TRUE,
           threeparttable = TRUE)











library(knitr)
library(kableExtra)

# Run chi-square test
gender_awareness_table <- table(data$Gender, data$Awareness.Status)
chisq_test_gender_awareness <- chisq.test(gender_awareness_table)

# Prepare the p-value string for insertion into the footnote
p_value_string <- format(chisq_test_gender_awareness$p.value, scientific = TRUE)

# Create the table with the chi-square test result included in the footnote
kable(data_summary, caption = "Distribution of Awareness by Gender") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 1, "Counts" = 2, "Percentages" = 2, " " = 1)) %>% # Ensure headers match columns
  footnote(general = paste("Chi-square test result: p-value =", p_value_string),
           general_title = "Note: ",
           footnote_as_chunk = TRUE,
           threeparttable = TRUE)

