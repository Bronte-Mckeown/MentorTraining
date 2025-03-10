library(tidyverse)
library(binom)
library(patchwork)
source("utils.R")

################################################################################
## Collapse data down to the level of meta-codes
collapse_to_meta_codes <- function(data) {
  
  data %>%
    # column row to uniquely identify each row
    mutate(row = row_number()) %>%
    # reshape so that each original column (except row) is represented by a pair of code and value
    pivot_longer(cols = -row, names_to = "code", values_to = "value") %>%
    # code column is split into meta_code and sub_code
    separate(code, into = c("meta_code", "sub_code"), sep = "_", extra = "drop", fill = "right") %>%
    group_by(row, meta_code) %>%
    summarize(value = max(value, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = meta_code, values_from = value) %>%
    select(-row)
}

# Calculate counts, percent, and CIs per code, overall (no grouping)
counts_percent_overall <- function(codes){ 
  codes %>% 
  summarise(across(everything(), list(
    Percentage = ~ round(sum(.x, na.rm = TRUE) / sum(!is.na(.x)) * 100, 2),
    n = ~ sum(!is.na(.x)),
    x = ~ sum(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = "code", values_to = "value") %>%
  separate(code, into = c("code", "variable"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = "variable", values_from = "value")  %>%
  rowwise() %>%
  mutate(
    CI = list(binom.confint(x, n, conf.level = 0.95, methods = "asymptotic")),
    CI_Lower = CI$lower * 100,
    CI_Upper = CI$upper * 100
  ) %>%
  select(-CI)
}

# Calculate counts, percent, and CIs per code, by grouping variable
counts_percent_by_group <- function(data, group_var, start_idx) {
  
  # Use sym() to create symbol from string character
  group_var_sym <- sym(group_var)
  # Select index of grouping variable column for selecting subset of data
  group_idx <- match(group_var, names(data))
  # Select subset of data
  data <- data[,c(group_idx, start_idx:ncol(data))]
  
  result <- data %>% group_by(!!group_var_sym) %>%
    summarise(across(everything(), list(
      Percentage = ~ round(sum(.x, na.rm = TRUE) / sum(!is.na(.x)) * 100, 2),
      n = ~ sum(!is.na(.x)),
      x = ~ sum(.x, na.rm = TRUE)))) %>%
    ungroup()
  
  # Calculate 95% CI for each percentage
  result <- result %>%
    pivot_longer(cols = -one_of(group_var), names_to = "Code", values_to = "Value") %>%
    separate(Code, into = c("Variable", "Metric"), sep = "_(?=[^_]+$)", extra = "merge") %>%
    pivot_wider(names_from = Metric, values_from = Value) %>%
    rowwise() %>%
    mutate(
      CI = list(binom.confint(x, n, conf.level = 0.95, methods = "asymptotic")),
      CI_Lower = CI$lower * 100,
      CI_Upper = CI$upper * 100
    ) %>%
    select(-CI)
  
  return(result)
}

## Overall bar plot
overall_barplot <- function(data, x_label_list, x_title){
  
  fontsize = 7
  
  ggplot(data, aes(x = reorder(code,-Percentage), y = Percentage)) +
    geom_bar(position = "dodge", stat = "identity", color = "black", fill = "grey", size = 0.7) +
    geom_errorbar(aes(ymin = ifelse(CI_Lower < 0, 0, CI_Lower),
                      ymax = ifelse(CI_Upper > 100, 100, CI_Upper)), width = 0.25, 
                  position = position_dodge(0.9), color = "black") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title.x = element_text(face = "bold", size = fontsize),
      axis.title.y = element_text(face = "bold", size = fontsize),
      axis.text.x = element_text(angle = 45, hjust = 1, size = fontsize)
    ) +
    labs(
      title = "What would the ideal school-based support for ITE mentors look like? (open text)",
      x = x_title,
      y = "Percentage"
    )+
    scale_x_discrete(labels = x_label_list)
  
}

## Grouped bar plot 
grouped_barplot <- function(data, group_str, x_label_list, x_title){
  
  fontsize = 7
  
  group_var <- sym(group_str)
  
  ggplot(data, aes(x = Variable, y = Percentage, fill = !!group_var)) +
    geom_bar(position = "dodge", stat = "identity", color = "black", size = 0.7) +
    geom_errorbar(aes(ymin = ifelse(CI_Lower < 0, 0, CI_Lower),
                      ymax = ifelse(CI_Upper > 100, 100, CI_Upper)), width = 0.25, 
                  position = position_dodge(0.9), color = "black") +
    scale_fill_manual(values = c("#008BD6", "#B86BFF", "#00E9B1", "#038603")) +
    theme_minimal(base_size = 7) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold", size = fontsize),
      axis.title.y = element_text(face = "bold", size = fontsize),
      axis.text.x = element_text(angle = 45, hjust = 1, size = fontsize),
      legend.position = "top",
      legend.text = element_text(size = fontsize),
      legend.title=element_blank()
    ) +
    labs(
      title = "What would the ideal school-based support for ITE mentors look like? (open text)",
      x = x_title,
      y = "Percentage"
    )+
    scale_x_discrete(labels = x_label_list)
}

flexible_percentages <- function(data, col_list){
  
  percentage_results <- sapply(col_list, function(col) {
    mean(data[, col] == 1) * 100
  })
  
  # Convert the result to a data frame for better readability
  percentage_df <- data.frame(Column = col_list, Percentage = percentage_results)
  print(percentage_df)
  return(percentage_df)
  
}

################################################################################
## Read in data

# All data in 'df'
df <- read.csv("data/deriv/opentext_finalCodes_forAnalysis_withTweaks_removedEmptyCodes_timeGenFix_monGenFix.csv")

# Select code columns only for operations below in 'cds'
start_idx = 5
cds <- df[,start_idx:ncol(df)]

# change directory
setwd("results/teacherTapp_qual/manual_coding/")

################################################################################

## Create new 'role' and 'role/phase' groups

# Collapse role (combine classroom teacher and middle leader, and SLT and head teachers)
# there are only 3 head teachers!
df$collapsed_role <- ifelse(df$role == 'Classroom Teacher' | df$role == 'Middle Leader',
                            df$role,
                            'Senior Leadership (inc. headtechers)'
)

# Move collapsed role column to front after original role column
df <- df %>% relocate(c(collapsed_role),
                      .after=role)

# Combine phase AND role (4 groups for simplicity, but could make six)
df <- df %>%
  mutate(phase_role = case_when(
    (role == 'Classroom Teacher' | role == 'Middle Leader') & phase == 'Primary' ~ 'Primary Classroom/Middle Leader',
    (role == 'Classroom Teacher' | role == 'Middle Leader') & phase == 'Secondary' ~ 'Secondary Classroom/Middle Leader',
    (role == 'SLT (excl head)' | role == 'Headteacher') & phase == 'Primary' ~ 'Primary Senior Leadership',
    (role == 'SLT (excl head)' | role == 'Headteacher') & phase == 'Secondary' ~ 'Secondary Senior Leadership'
  ))

# Move column to front after original phase column
df <- df %>% relocate(c(phase_role),
                      .after=phase)

################################################################################
## Checking N of groups

# Phase; primary (83), secondary (206)
df %>% 
  group_by(phase) %>%
  summarise(N = length(phase))

# Role; classroom (68), head teacher (3), middle leader (158), SLT (60)
df %>% 
  group_by(role) %>%
  summarise(n_rows = length(role))

phase_role_N <- df %>%
  group_by(role, phase) %>%
  summarise(N = length(phase)) %>%
  ungroup() %>%
  mutate(Percentage = (N / sum(N)) * 100)
write.csv(phase_role_N, "phase_and_role_crossTab_N_289.csv", row.names = FALSE)

# Collapsed role
collapsed_role_N <-df %>% 
  group_by(collapsed_role) %>%
  summarise(N = length(phase)) %>%
  ungroup() %>%
  mutate(Percentage = (N / sum(N)) * 100)
write.csv(collapsed_role_N, "collapsed_role_N_289.csv")

# Phase and role combined
collapsed_phase_role_N <- df %>% 
  group_by(phase_role) %>%
  summarise(N = length(phase)) %>%
  ungroup() %>%
  mutate(Percentage = (N / sum(N)) * 100)
write.csv(collapsed_phase_role_N, "collapsed_phase_role_N_289.csv")

################################################################################
## Counts for all codes, overall

# For word clouds and plots, remove noCode, other, and unclear codes
cds_forPlotting <- dplyr::select(cds, -c(noCode, other_cantSay, unclear,
                                         other_notRelevant, 	
                                         other_dontKnow))

# Use counts_percent_overall to get counts, percent, and CI for all codes
all_counts_percent_overall <- counts_percent_overall(cds_forPlotting)

# Nicer names for word clouds
wordCloud_labels <- c(
                    "training_traineeRequirements"  = "Knowing-Mentee-Requirements",
                    "flexibility" = "Flexible",
                    "money_cover" = "Funding-For-Cover",
                    "QA" = "Quality-Assurance",                            
                    "workloadManagement" = "Workload-Management",
                    "time_timeBack" = "Time-Back",
                    "time_general"= "Time",                  
                    "time_additional" = "Additional-Time",
                    "time_protected" = "Protected-Time",             
                    "time_ppa" = "PPA-Time",                 
                    "time_noncontact" = "NonContact-Time",             
                    "time_release" = "Release-Time",                 
                    "time_meetings" = "Time-Meetings",              
                    "time_checkIns"  = "Time-Check-ins",                
                    "time_observations" = "Time-Observations",             
                    "time_feedback" = "Time-Feedback",               
                    "time_collab" = "Time-Collaboration",            
                    "time_admin"  = "Time-Admin",                   
                    "time_training" = "Time-Training-Research",             
                    "time_prep" = "Time-Meeting-Prepartion",
                    "money_funding" = "Funding",             
                    "moneySalary_general"= "Extra-Salary",         
                    "moneySalary_tlr" = "TLR",            
                    "support_general" = "Support",             
                    "recognition_general" = "Recognition",        
                    "recognition_slt" = "Recognition-SLT",               
                    "recognition_public"= "Public-Recognition",         
                    "recognition_celebration" = "Celebration",      
                    "collab_mentorGeneral" = "Mentor-Collaboration", 
                    "collab_mentorSupport" = "Mentor-Support-General",   
                    "collab_mentorIssues" = "Mentor-Support-Issues",       
                    "collab_mentorInterdepartmental" = "Interdepartmental-Mentor-Collaboration",
                    "collab_mentees"= "Collaboration-Mentees",
                    "collab_external" = "External-Collaboration",      
                    "collab_providers" = "Collaboration-Providers",             
                    "training_general" = "Training",              
                    "training_byMentors"  = "Mentor-to-Mentor-Training",          
                    "training_observationFeedback" = "Training-Observation-Feedback", 
                    "training_coaching" = "Training-Coaching",           
                    "training_internal" = "Internal-Training",              
                    "training_external"  = "External-Training",           
                    "training_needsBased" = "Needs-based-Training",          
                    "training_qualifications" = "Qualification",       
                    "communication_general" = "Communication",       
                    "communication_slt" = "Communication-SLT",       
                    "delivery_documentationSystems" = "Better-Documentation-Process",
                    "delivery_consistency" = "Consistency", 
                    "frustration_tone" = "Frustrated-Tone",         
                    "frustration_noSupport" = "Lack-of-Support"      
                  )


# Update code names for word clouds
all_counts_percent_overall <- all_counts_percent_overall %>% 
  mutate(code = wordCloud_labels[as.character(code)])

# select first two columns
code_wordcloud <- all_counts_percent_overall[,1:2]

# save out for word cloud
write.csv(code_wordcloud, "allCodes_for_wordcloud.csv")

################################################################################
## Counts/percentages by groups

# Percent by original groups
percent_by_phase <- counts_percent_by_group(df, "phase", start_idx = 7)
percent_by_role <- counts_percent_by_group(df, "role", start_idx = 7)

# Counts and percentages by collapsed role
percent_by_collapsed_role <- counts_percent_by_group(df, "collapsed_role", start_idx = 7)

# Counts and percentages by combined role and phase
percent_by_phase_role <- counts_percent_by_group(df, "phase_role", start_idx = 7)

# save percentages out
write.csv(percent_by_phase, "code_percents_by_phase.csv",
          row.names = FALSE)
write.csv(percent_by_role, "code_percents_by_role.csv",
          row.names = FALSE)
write.csv(percent_by_collapsed_role, "code_percents_by_collapsed_role.csv",
          row.names = FALSE)
write.csv(percent_by_phase_role, "code_percents_by_phase_collapsed_role.csv",
          row.names = FALSE)

################################################################################
# collapse codes to meta-codes
cds_collapsed <- collapse_to_meta_codes(cds)
cds_collapsed <- dplyr::select(cds_collapsed, -c(noCode, other, unclear))

# add first 6 columns back to meta-codes
df_collapsed <- cbind(df[,1:6], cds_collapsed)

################################################################################
meta_wordcloud_labels <-  c("QA" = "Quality Assurance", "collab" = "Collaboration",
                            "communication" = "Communication", "delivery" = "Delivery",
                            "flexibility" = "Flexibility", "frustration" = "Frustration",
                            "money" = "Funding", "moneySalary" = "Salary/TLR", "noCode" = "Misc",
                            "other" = "Other", "recognition" = "Recognition", "support" = "General support",
                            "time" = "Time", "training" = "Training", "unclear" = "Unclear",
                            "workloadManagement" = "Workload management"
)

# Meta-code % overall
all_metaPercent <- counts_percent_overall(cds_collapsed)

# rename values
all_metaPercent <- all_metaPercent %>% 
  mutate(code = meta_wordcloud_labels[as.character(code)])

# select first two columns
meta_wordcloud <- all_metaPercent[,1:2]

# save out for word cloud
write.csv(meta_wordcloud, "meta_for_wordcloud.csv")

################################################################################
# Meta-code % by groups
metaPercent_by_phase <- counts_percent_by_group(df_collapsed, "phase", start_idx = 7)
metaPercent_by_role <- counts_percent_by_group(df_collapsed, "role", start_idx = 7)

# Meta-code % by collapsed groups
metaPercent_by_collapsed_role <- counts_percent_by_group(df_collapsed, "collapsed_role", start_idx = 7)
metaPercent_by_phase_collapsed_role <- counts_percent_by_group(df_collapsed, "phase_role", start_idx = 7)

################################################################################
# Overall bar plots with meta-codes
metaCodes_overall_bar <- overall_barplot(all_metaPercent, meta_wordcloud_labels,
                                         "Primary Theme")
ggsave("metaCodes_overall_bar.png",
       plot = metaCodes_overall_bar, width = 20, height = 7,
       units = "cm", dpi = 600)

# Grouped bar plots with meta-codes
metaCodes_phase_role <- grouped_barplot(metaPercent_by_phase_collapsed_role,
                "phase_role", meta_wordcloud_labels, "Primary Theme")

ggsave("metaCodes_phase_role.png",
       plot = metaCodes_phase_role , width = 30, height = 20,
       units = "cm", dpi = 600)

# Phase
metaPercent_by_phase$Variable <- factor(metaPercent_by_phase$Variable,
                                                 levels = c("time", "moneySalary",
                                                            "training", "recognition",
                                                            "collab",
                                                            "frustration",
                                                            "workloadManagement",
                                                            "money",
                                                            "delivery",
                                                            "support",
                                                            "communication",
                                                            "flexibility",
                                                            "QA"))
metaCodes_phase <- grouped_barplot(metaPercent_by_phase,
                                        "phase", meta_wordcloud_labels, "Primary Theme")

ggsave("metaCodes_phase.png",
       plot = metaCodes_phase , width = 20, height = 7,
       units = "cm", dpi = 600)


# Collapsed Role
# set levels
metaPercent_by_collapsed_role$Variable <- factor(metaPercent_by_collapsed_role$Variable,
                                                          levels = c("time", "moneySalary",
                                                                     "training", "recognition",
                                                                    "collab",
                                                                     "frustration",
                                                                     "workloadManagement",
                                                                    "money",
                                                                     "delivery",
                                                                     "support",
                                                                     "communication",
                                                                     "flexibility",
                                                                     "QA"))

metaCodes_collapsed_role <-grouped_barplot(metaPercent_by_collapsed_role,
                "collapsed_role", meta_wordcloud_labels, "Primary Theme")

ggsave("metaCodes_collapsed_role.png",
       plot = metaCodes_collapsed_role, width = 20, height = 7,
       units = "cm", dpi = 600)

# Put overall and collapsed role bar chart together for report
metaCodes_overall_bar <- metaCodes_overall_bar + 
  theme(plot.title = element_blank(),
        axis.title.x = element_blank())

metaCodes_collapsed_role <- metaCodes_collapsed_role+ 
  theme(plot.title = element_blank())

reportFigure <- metaCodes_overall_bar / metaCodes_collapsed_role + 
  plot_annotation(title = 'What would the ideal school-based support for ITE mentors look like? (open text)',
                  theme = theme(plot.title = element_text(size = 12, hjust = 0.5,
                                                          face = "bold")),
                  tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12))
reportFigure

ggsave(
  filename = sprintf('reportFigure_niotColours.png'),
  plot = reportFigure,
  width = 20,
  height = 15,
  units = 'cm',
  dpi = 1000,
  device = ragg::agg_png,
  scaling = 1
)

################################################################################

# Overall bar plot with all codes
sorted_all_counts_percent_overall <- sort_by(all_counts_percent_overall,
                                             all_counts_percent_overall$Percentage,
                                             decreasing = TRUE)

top15 <- sorted_all_counts_percent_overall[1:15,]

codes_overall_top15 <- overall_barplot(top15, wordCloud_labels, "Sub-Code (Top 15)")

ggsave("codes_overall_top15.png",
       plot = codes_overall_top15, width = 20, height = 7,
       units = "cm", dpi = 600)

################################################################################

# Vector of column names which we want to calculate percentages for
time_for_columns <- c("time_meetings", "time_checkIns", "time_observations", "time_feedback", 
                        "time_collab", "time_admin", "time_training", "time_prep", "time_timeBack")

WHAT_time_is_wanted_for <- flexible_percentages(df, time_for_columns)
sort_by(WHAT_time_is_wanted_for, WHAT_time_is_wanted_for$Percentage)


## all training
training_all_columns <-  grep("^training_", names(df), value = TRUE)

all_training <-flexible_percentages(df, training_all_columns)
sort_by(all_training, all_training$Percentage)
write.csv(all_training, "allTraining_percentages.csv")