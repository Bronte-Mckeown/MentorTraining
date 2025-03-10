library(tidyverse)
source("utils.R") # for user defined functions

################################################################################
## Read in data and perform basic checks

# Read in data 
# This data is the output of 'teacherTapp_qual_agreement.R' with some manual
# tweaks (recorded in OneDrive coding notes)
df <- read.csv("data/deriv/opentext_finalCodes_forAnalysis_withTweaks.csv",
               na.strings = c(" ", ""))

# Select code columns only for operations below
start_idx = 5
cds <- df[,start_idx:ncol(df)]

# Sanity checking
answer_missing(df) # in answer column (should be zero now)
coding_missing(cds) # if no codes selected (should be FALSE now)
unexpected <- check_values(cds) 
print (nrow(unexpected))  # check for unexpected values in code columns, should be zero

# Set NAs to 0 (for 2 new columns BM added after sanity checking final data sheet)
cds <- fill_na(cds)

################################################################################

## Removing un-used codes

# Check codes that were NEVER selected (less than 1) for removal
code_redundancy(cds, threshold = 1) # "time_other" and "training_online"

# Check codes that were selected once (less than 2)
code_redundancy(cds, threshold = 2) # "collab_mentees", "training_timetabled", and "communication_slt"

# For 'training_timetabled' responses, re-code to be 'time_protected' & 'time_training'
# just one response anyhow which already has time_training coded
cds <- cds %>%
  mutate(
    time_protected = ifelse(training_timetabled == 1, 1, time_protected),
    # time_training = ifelse(training_timetabled == 1, 1, time_training),
    training_timetabled = ifelse(training_timetabled == 1, 0, training_timetabled)
  )

# Drop columns where count == 0 (drops 3 columns)
cds2 <- cds %>%
  select(which(colSums(.) != 0))

# combine with first four columns
df2 <- cbind(df[,1:4], cds2) # combine new codes with id, demo, and answer cols

################################################################################

## Check time general column (if there is a 1 in general, shouldn't be in any of the others!)
# Identify rows with a 1 in 'time_general'
time_general_rows <- df2$time_general == 1

# vector of all the other time columns
time_columns <- c("time_meetings", "time_checkIns", "time_observations", "time_feedback", 
                  "time_collab", "time_admin", "time_training", "time_prep", "time_timeBack",
                  "time_additional", "time_protected", "time_ppa", "time_noncontact", "time_release")

# Select 'time_general' rows and columns from 'time_columns', check for any rows that have a '1'
rows_with_conflict <- apply(df2[time_general_rows, time_columns], 1, function(row) any(row == 1))

# Return rows that match the condition above
conflicting_rows <- df2[time_general_rows, ][rows_with_conflict, ]

# Update data frame by setting that row's time_general value to zero
df2[df2$user_id %in% conflicting_rows$user_id, "time_general"] <- 0

# Last thing, update documentation_systems column name for meta-code counts
names(df2)[names(df2) == "delivery_documentation_systems"] <- "delivery_documentationSystems"
names(df2)[names(df2) == "workload_management"] <- "workloadManagement"
names(df2)[names(df2) == "money_salary_general"] <- "moneySalary_general"
names(df2)[names(df2) == "money_salary_tlr"] <- "moneySalary_tlr"

# Save out for analysis (now not used for analysis, see below)
write.csv(df2, "data/deriv/opentext_finalCodes_forAnalysis_withTweaks_removedEmptyCodes_timeGenFix.csv",
          row.names = FALSE)

################################################################################
# on 8th August, CM and BM agreed to remove Money General code and recode
# responses to Money Salary

# read data frame back in (just to keep record)
df3 <- read.csv("data/deriv/opentext_finalCodes_forAnalysis_withTweaks_removedEmptyCodes_timeGenFix.csv")

# check how many are money general and money salary general
length(which(df3$money_general == 1))
length(which(df3$moneySalary_general == 1))

# update
df4 <- df3 %>%
  mutate(
    moneySalary_general = ifelse(money_general == 1, 1, moneySalary_general),
    money_general = ifelse(money_general == 1, 0, money_general)
  )

# check again to make sure it's worked
length(which(df4$money_general == 1))
length(which(df4$moneySalary_general == 1))

# Drop money general column
df5 <- select(df4, -c(money_general))

# Save out for analysis
write.csv(df5, "data/deriv/opentext_finalCodes_forAnalysis_withTweaks_removedEmptyCodes_timeGenFix_monGenFix.csv",
          row.names = FALSE)