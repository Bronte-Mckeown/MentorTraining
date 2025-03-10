# This script creates demographic table for the quant sample, split by seniority.
library(tidyverse)
library(flextable)
library(officer)

################################################################################
# Function to reorder factor levels
reorder_levels <- function(data, demog, levels) {
  data %>%
    mutate(Level = factor(Level, levels = levels)) %>%
    arrange(Demographic, Level)
}

################################################################################
# Read in data (sent by Teacher Tapp, edited to remove 'Other Teachers')
data <- read.csv("data/deriv/seniority_demographic_counts.csv")

# Split Subject and Key Stage using if/else
data <- data %>%
  mutate(demog_2 = ifelse(demog_2 == "teaching_subject" & value %in% c("KS2", "EYFS/KS1"), "key_stage", 
                          ifelse(demog_2 == "teaching_subject", "subject", demog_2)))%>%
  filter(demog_2 != "teaching_subject")

################################################################################
# # Transform the data to the desired format
# # Don't want to do it this way I don't think, but keeping in case you do
# demo_table <- data %>%
#   # Pivot the data to have one column per 'seniority' level
#   pivot_wider(names_from = seniority, values_from = num_teachers) %>%
#   # Fill in missing values with 0 (if applicable)
#   replace(is.na(.), 0) %>%
#   # Combine the levels into two groups: Teachers and Leaders
#   mutate(Teachers = `Classroom Teacher` + `Middle Leader`,
#          Leaders = `SLT (excl head)` + `Headteacher`) %>%
#   # Drop the original columns
#   select(-`Classroom Teacher`, -`Middle Leader`, -`SLT (excl head)`, -Headteacher) %>%
#   # Calculate the total number of teachers across the combined levels
#   rowwise() %>%
#   mutate(Total = sum(c_across(c("Teachers", "Leaders")), na.rm = TRUE)) %>%
#   group_by(demog_2) %>%
#   mutate(Overall_total = sum(Total)) %>%
#   # Calculate the percentage for Teachers and Leaders
#   mutate(Teachers = (Teachers / Overall_total) * 100,
#          Leaders = (Leaders / Overall_total) * 100) %>%
#   # Calculate the percentage for each row
#   mutate(Percentage = (Total / Overall_total) * 100) %>%
#   mutate(Percentage = round(Percentage, 0)) %>%
#   ungroup() %>%
#   # Round the Teachers and Leaders percentages
#   mutate(Teachers = round(Teachers, 0),
#          Leaders = round(Leaders, 0))

# Transform data into right format for table
demo_table <- data %>%
  # Pivot the data to have one column per 'seniority' level
  pivot_wider(names_from = seniority, values_from = num_teachers) %>%
  # Fill in missing values with 0 (e.g., for head teachers)
  replace(is.na(.), 0) %>%
  # Combine the levels into two groups: Teachers and Leaders
  mutate(Teachers = `Classroom Teacher` + `Middle Leader`,
         Leaders = `SLT (excl head)` + `Headteacher`) %>%
  # Drop the original columns
  select(-`Classroom Teacher`, -`Middle Leader`, -`SLT (excl head)`, -Headteacher) %>%
  # Calculate the total number of teachers across the combined levels
  rowwise() %>%
  mutate(Total = sum(c_across(c("Teachers", "Leaders")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(demog_2) %>%
  # Calculate the total number of Teachers and Leaders for each demog_2
  mutate(demog2_total = sum(Total)) %>%
  # Calculate the percentage for each row
  mutate(Percentage = round((Total / demog2_total) * 100)) %>%
  ungroup() %>%
  # Calculate the percentage for Teachers and Leaders out of the row Total
  mutate(Teachers = round((Teachers / Total) * 100),
         Leaders = round((Leaders / Total) * 100))

# View the table
demo_table

# Change Column names
colnames(demo_table) <- c("Demographic", 'Level', " Level Classroom/Middle leaders %", 
                           "Level Senior Leaders (inc. Heads) %", "Level N", "Total N", "Level %")


# Move Total N column after Demographic column
demo_table <- demo_table %>% relocate(c(`Total N`),
                                  .after=Demographic)

# Move Level N column after Level column
demo_table <- demo_table %>% relocate(c(`Level N`, `Level %`),
                                        .after=`Level`)


################################################################################
# List of prettier demographic strings for replacing below
pretty_demographics_table = c(
                              'phase'= 'School Phase',
                              'governance' = 'School Governance',
                              'teaching_region' = 'Region',
                              'school_size_quartile' = 'School Size',
                              'fsm_quartile' = 'Free School Meals',
                              'ofsted' = 'Ofsted Rating',
                              'subject' = 'Subject',
                              'key_stage' = 'Key Stage',
                              'school_funding' = 'Funding',
                              'funding_phase' = 'Funding & Phase',
                              'age_group' = 'Age',
                              'gender' = 'Gender',
                              'experience' = 'Teacher Experience')

# Replace demographic strings using named list
demo_table<- demo_table %>%
  mutate(Demographic = sapply(Demographic,
                              function(x) pretty_demographics_table[[x]]))

# Order levels of demog_2 variable
level_order <- list(
  'Ofsted Rating' = c('Requires Improvement or Inadequate', 'Good', 'Outstanding'),
  'School Phase' = c('Primary', 'Secondary'),
  'School Governance' = c('Independent', 'LA community', 'LA non-community', 'Small MAT (N<=3)', 'Large MAT (N>3)', 'Stand-alone Academy'),
  'School Size' = c('Q1 Small', 'Q2', 'Q3', 'Q4 Large'),
  'Subject' = c('Arts incl D&T', 'English', 'Humanities', 'Languages', 'Maths', 'Other incl PE', 'Science'),
  'Key Stage' = c('EYFS/KS1', 'KS2'),
  'Free School Meals' = c('Q1 Affluent', 'Q2', 'Q3', 'Q4 Deprived'),
  'Region' = c('East of England', 'London', 'Midlands', 'North West', 'South East', 'South West', 'Yorkshire and North East'),
  'Age' = c('Age in 20s', 'Age in 30s', 'Age in 40s', 'Age in 50s+'),
  'Funding' = c('Private School', 'State-funded school'),
  'Funding & Phase' = c('Private Primary', 'Private Secondary', 'State-funded Primary', 'State-funded Secondary'),
  'Gender' = c('Female', 'Male'),
  'Teacher Experience' = c('Less than 5 years', 'Between 5 and 10 years', 'Between 10 and 20 years', 'Over 20 years')
)

# Apply the reordering for each demographic (demog_2)
for (demog in names(level_order)) {
  demo_table <- demo_table %>%
    group_by(Demographic) %>%
    filter(Demographic == demog) %>%
    reorder_levels(demog, level_order[[demog]]) %>%
    bind_rows(demo_table %>%
                group_by(Demographic) %>%
                filter(Demographic != demog))
}

# Set Demographic as factor and set levels for ordering table
demo_table$Demographic <- factor(demo_table$Demographic,
                                  levels = c(
                                    "School Phase",
                                    "Ofsted Rating",
                                    "School Governance",
                                    "School Size",
                                    'Subject',
                                    'Key Stage',
                                    "Free School Meals",
                                    "Region",
                                    "Age", "Funding",
                                    "Funding & Phase","Gender",
                                    "Mentors vs Non-Mentors (leadership)",
                                    "Teacher Experience"))
levels(demo_table$Demographic)

# Order data according to levels of Demographic
demo_table<- demo_table %>%
  arrange(Demographic)

################################################################################

# Convert to flextable object
ft <- flextable(demo_table)
ft # Show

# Identify the rows where the demographic group changes
demographic_changes <- which(demo_table$Demographic != 
                               lag(demo_table$Demographic,
                                default = first(demo_table$Demographic)))

# Add horizontal lines where the demographic group changes
for (i in demographic_changes) {
  ft <- ft %>%
    hline(i = i-1, border = fp_border(width = 1, color = "black"))
}
ft

# Merge Demographic column to stop repeating values
# Bit annoying when Total N is the same but only happens once and is easy to adjust
# in doc after
merged_ft <- merge_v(ft, j = c("Demographic", "Total N"))
merged_ft

################################################################################
# Save the flextable to a Word document
doc <- read_docx() %>%
  body_add_flextable(merged_ft) %>%
  body_add_par("Demographic Groups by Seniority", style = "Normal")

print(doc, target = "results/quant_demographicGroups_bySeniority_table_rowAddsTo100.docx")
