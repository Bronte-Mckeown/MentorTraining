# This script creates demographic table for the quant sample

# Load necessary libraries
library(tidyverse)
library(flextable)
library(officer)

################################################################################

# Functions for creating prettier demographic level strings
# This is different from the one in utils.R
process_label_table <- function(key) {
  #  Split the string at '_'
  parts <- strsplit(key, "_")[[1]]
  # Combine the parts back into a single string with spaces
  label <- paste(parts, collapse = " ")
  # Capitalize each word
  label <- tools::toTitleCase(label)
  return(label)
}

# Call process_label_table() function on provided keys
get_processed_labels_table <- function(keys) {
  labels <- sapply(keys, process_label_table, USE.NAMES = TRUE)
  return(labels)
}

################################################################################

# Read in data
data <- read.csv("data/deriv/closedresponse_n_responders.csv")

################################################################################

# Gather the data into a long format
long_data <- data %>%
  gather(key = "Demographic_Level", value = "N") %>%
  separate(Demographic_Level, into = c("Demographic", "Level"), 
           sep = "_", extra = "merge", remove = TRUE)

# Calculate the percentage for each demographic group
long_data <- long_data %>%
  group_by(Demographic) %>%
  mutate(Percentage = round(N / sum(N)*100,digits = 0))%>%
  ungroup()

################################################################################

# List of prettier demographic strings for replacing below
pretty_demographics_table = c('seniority' =  'Seniority of Teachers',
                        'phase'= 'School Phase',
                        'schoolGovernance' = 'School Governance',
                        'region' = 'Region',
                        'schoolSize' = 'School Size',
                        'fsm' = 'Free School Meals',
                        'ofsted' = 'Ofsted Rating',
                        'subject' = 'Subject/Key Stage',
                        'funding' = 'Funding',
                        'fundingPhase' = 'Funding & Phase',
                        'age' = 'Age',
                        'gender' = 'Gender',
                        'experience' = 'Teacher Experience',
                        'seniorSplit' = 'Mentors vs Non-Mentors (leadership)')

# Replace demographic strings using named list
long_data <- long_data %>%
  mutate(Demographic = sapply(Demographic,
                              function(x) pretty_demographics_table[[x]]))

# Use get_processed_labels() to get prettier strings for levels
pretty_levels <- get_processed_labels_table(long_data$Level)
long_data$Level <- pretty_levels # Replace with pretty levels

################################################################################
# Set Demographic as factor and set levels for ordering table
long_data$Demographic <- factor(long_data$Demographic,
                                   levels = c("Seniority of Teachers",
                                              "School Phase",
                                              "Ofsted Rating",
                                              "School Governance",
                                              "School Size",
                                              "Subject/Key Stage",
                                              "Free School Meals",
                                              "Region",
                                              "Age", "Funding",
                                             "Funding & Phase","Gender",
                                             "Mentors vs Non-Mentors (leadership)",
                                              "Teacher Experience"))
levels(long_data$Demographic)

# Order data according to levels of Demographic
long_data <- long_data %>%
  arrange(Demographic)

################################################################################

# Convert to flextable object
ft <- flextable(long_data)
ft

# Identify the rows where the demographic group changes
demographic_changes <- which(long_data$Demographic != 
                            lag(long_data$Demographic,
                            default = first(long_data$Demographic)))

# Add horizontal lines where the demographic group changes
for (i in demographic_changes) {
  ft <- ft %>%
    hline(i = i-1, border = fp_border(width = 2, color = "black"))
}

# Merge Demographic column to stop repeating values
merged_ft <- merge_v(ft, j = c("Demographic"))
merged_ft

################################################################################

# Save the flextable to a Word document
doc <- read_docx() %>%
  body_add_flextable(merged_ft) %>%
  body_add_par("Demographic Groups", style = "Normal")

print(doc, target = "results/quant_demographicGroups_table.docx")
