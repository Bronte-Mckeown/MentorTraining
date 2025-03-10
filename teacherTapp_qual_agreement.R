# Load necessary libraries
library(irr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)

source("utils.R") # for user defined functions

################################################################################
# Read in data for rater 1 and 2 (and 3)
rater1 <- read.csv("data/deriv/bm_coding_clean.csv", na.strings = c(" ", ""))
rater2 <- read.csv("data/deriv/cm_coding_clean.csv", na.strings = c(" ", ""))
rater3 <- read.csv("data/deriv/et_coding_clean.csv", na.strings = c(" ", ""))

# Data cleaning / checking

# Check missing values in answer column
answer_missing(rater1)
answer_missing(rater2)
answer_missing(rater3)

# Remove rows with missing answers (2 rows)
rater1 <- rater1[!is.na(rater1$answer), ]
rater2 <- rater2[!is.na(rater2$answer), ]
rater3 <- rater3[!is.na(rater3$answer), ]

# Just select ratings for the rest of the analysis
start_idx = 5
ratings1 <- rater1[, start_idx:ncol(rater1)]
ratings2 <- rater2[, start_idx:ncol(rater2)]
ratings3 <- rater3[, start_idx:ncol(rater3)]

# Check for rows with no codes
coding_missing(ratings1) # should be FALSE
coding_missing(ratings2) # should be FALSE
coding_missing(ratings3) # should be TRUE

# Check for unexpected values (not 1, 0, or NA)
unexpected1 <- check_values(ratings1)
unexpected2 <- check_values(ratings2)
unexpected3 <- check_values(ratings3)

# Fill NA with zeros (unless whole row is NA)
ratings1 <- fill_na(ratings1)
ratings2 <- fill_na(ratings2)
ratings3 <- fill_na(ratings3)

# Identify codes that were used less than once/twice for each rater (just rater 1 and 2)
redundant_codes1 <- code_redundancy(ratings1, threshold = 1)
redundant_codes2 <- code_redundancy(ratings2, threshold = 1)

# Calculate column-wise agreement
column_agreement <- calculate_agreement(ratings1, ratings2, by = "column")
column_results <- data.frame(
  Code = colnames(ratings1),
  Code_Kappa = column_agreement$kappa,
  Code_Percent_Agreement = column_agreement$percent
)

################################################################################

## Visualise and summarise the agreement levels

# Figure out kappa agreement levels
code_agreement_levels <- agreement_levels(column_results$Code_Kappa, ncol(ratings1))

# Create pie charts
code_pie_chart <- create_pie_chart(code_agreement_levels,
                                   "Per-Code")

# Collate pie charts and save
all_pie <- code_pie_chart & 
  theme(legend.position = 'bottom')
all_pie <- all_pie +plot_annotation(
  title = "Inter-rater agreement: Cohen's Kappa"
)
all_pie

ggsave("results/teacherTapp_qual/manual_coding/cohensKappa_pieChart.png", dpi = 600, units = "cm",
       width = 20, height = 10)

# Calculate combined percentages for code and row agreement levels
combined_code_percentages <- calculate_combined_percentages(code_agreement_levels)

# Display combined percentages
print(combined_code_percentages)

################################################################################
# Calculate average Kappa
average_code_kappa <- mean(column_results$Code_Kappa)

################################################################################

## Select disagreement data for collaborative 2nd round coding between rater 1 and 2

# Identify rows without perfect agreement (WANT in final data frame)
# (This df may also include rows where 'noCode' is selected by one of the raters)
# which ultimately, we don't want but this dealt with below.
disagreement1 <- which(row_results$Percent_Agreement != 100) # 173 indices
agreement1 <- which(row_results$Percent_Agreement == 100) # 116 indices

# Identify rows where raters selected 1 for 'noCode' (DON'T want in final data frame)
# Even if disagreed on them, don't want them in final data frame below as already discussed them.
noCode_selected1 <- which(ratings1$noCode == 1) # 43
noCode_selected2 <- which(ratings2$noCode == 1) # 38
noCode_selected3 <- which(ratings3$noCode == 1) # 18

# Use union to keep ANY row indices where AT LEAST one rater selected 'noCode', 
# removing any duplicates
noCode_selected_combined <- sort(union(union(noCode_selected1, noCode_selected2),
                                       noCode_selected3))

# Keep row indices with < 100% agreement but remove rows with 'noCode' selected
disagreement_without_noCode <- setdiff(disagreement1, noCode_selected_combined)
agreement_without_noCode <- setdiff(agreement1, noCode_selected_combined)

# Create the final dataframe with the desired rows
disagree_df <- rater1[disagreement_without_noCode,]
agree_df <- rater1[agreement_without_noCode,]

# set all to NA for recoding between rater 1 and rater 2
disagree_df[,5:ncol(disagree_df)]<- NA

# save out to data/deriv folder
write.csv(disagree_df, "data/deriv/disagreement_corrected.csv")

################################################################################
##  Next steps

# select 'I don't know' rows (use rater1, although this is arbitrary) 
# using 'noCode_selected_combined'
noCode_df <- rater1[noCode_selected_combined,]

# add extra columns (unclear, money_cover, QA, workload_management)
# set all values to NA
newCols <- c("unclear", "money_cover", "QA", "workload_management")
noCode_df[ , newCols] <- NA
agree_df[ , newCols] <- NA

# move columns to front to match disagreement_corrected in OneDrive
noCode_df <- noCode_df %>% relocate(c(unclear, money_cover, QA, workload_management),
                       .after=noCode)

agree_df <- agree_df %>% relocate(c(unclear, money_cover, QA, workload_management),
                                    .after=noCode)

# change delivery_paperwork column to 'delivery_documentation_systems'
names(noCode_df)[names(noCode_df) == "delivery_paperwork"] <- "delivery_documentation_systems"
names(agree_df)[names(agree_df) == "delivery_paperwork"] <- "delivery_documentation_systems"

# save csv out and then use 'I don't know' sheet from the other day with you and Claire (OneDrive)
# to code these responses
write.csv(noCode_df, "data/deriv/noCodeRows_forReCoding.csv")

# should then be able to read that back in
# and read back in disagreements_corrected that you filled out with Claire (OneDrive)
# use rbind() to put these together
# this should then be all disagreements (worked through) and all 'don't know' codes (worked through)
# then use rbind() to add 'agreement rows' (from rater 1)
# this final dataframe should have 289 unique rows containing (1) 100% original agreement,
# (2) rows with initial disagree that have been collaborated on and (3) I dont know codes, re-coded through discussion
# columns should all be the same and in same order (but check before using rbind())

# read in re-coded 'noCodes' (BM did this on 24 June based on discussions with CM on 13 June)
noCodes_recoded <- read.csv("data/deriv/noCodeRows_reCoded_24_june.csv", na.strings = c(" ", "", "NA"))
noCodes_recoded <- noCodes_recoded[,2:61] # remove index col

# read in re-coded disagreement codes (BM and CM worked on this on 20 June)
disagreement_recoded <- read.csv("data/deriv/disagreement_corrected_nowAgreed.csv", na.strings = c(" ", "", "NA"))
disagreement_recoded <- disagreement_recoded[,2:60] # remove index col

# add 'time_timeBack' to other 2 dataframes after workload_management
agree_df[ , 'time_timeBack'] <- NA
agree_df <- agree_df %>% relocate(c(time_timeBack),
                                  .after=workload_management)

disagreement_recoded[ , 'time_timeBack'] <- NA
disagreement_recoded <- disagreement_recoded %>% relocate(c(time_timeBack),
                                  .after=workload_management)

all_coded <- rbind(agree_df, disagreement_recoded, noCodes_recoded)
all_coded_justIDs <- all_coded[,c(1, 5:60)]

# Worth pausing here to sanity check against original codes and original teacher tapp data
# Make sure the final sheet makes sense (check at least half the codes that they make sense)
# i.e., if the response is 'time', make sure that code is selected.

# read in original teacher tapp data
og_data <- read.csv('data/deriv/opentext.csv', na.strings = c(" ", ""))
og_data <- og_data[!is.na(og_data$answer), ]

# merge all_coded_justIDs and og_data on ID to end up with data in original order
final_coded_data <- inner_join(og_data, all_coded_justIDs, by = join_by(user_id))

# fill all NA to 0
final_coded_data[,5:60] <- fill_na(final_coded_data[,5:60])

# This is the sheet you can then save for further wrangling and analysis
# Save to csv
write.csv(final_coded_data, "data/deriv/opentext_finalCodes_forAnalysis.csv",
          row.names  = FALSE)