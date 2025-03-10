# This script creates demographic table for the qual sample.

# Load necessary libraries
library(tidyverse)
library(flextable)
library(officer)

################################################################################
# Read in data
data <- read.csv("data/deriv/opentext.csv")

################################################################################
# Calculate total count for calculating percentages
total_count <- nrow(data)

# Summarize the data
summary_table <- data %>%
  # Group by phase and role
  group_by(phase, role) %>%
  # Calculate number of rows for each
  summarize(N = n()) %>%
  # Add percentage column
  mutate(Percentage = round((N / total_count) * 100, digits = 0))

################################################################################
# Convert to flextable object
ft <- flextable(summary_table)
ft

# Save the flextable to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("Demographic Groups", style = "Normal")

print(doc, target = "results/qual_demographicGroups_table.docx")
