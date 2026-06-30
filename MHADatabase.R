#"r.plot.useHttpgd": true
#Need to download Rtools from https://cran.r-project.org/bin/windows/Rtools/
#install.packages("devtools")
#devtools::install_github("nx10/httpgd")

#install.packages("tidyverse")
#install.packages("openxlsx")
#install.packages("gt")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("car")
#install.packages("emmeans")
#install.packages("ggeffects")
#install.packages("patchwork")

library(tidyverse) #For data manipulation and visualization
library(openxlsx) #For reading Excel files
library(gt) #For creating summary statistic tables
library(lme4) #For linear mixed effects models
library(lmerTest) #For p-values in LMM summary
library(car) #Draws confidence intervals for QQ plots
library(emmeans) #For post-hoc pairwise comparisons of LMMs
library(ggeffects) #For visualizing LMM results
library(patchwork) #For combining ggplots

#File path
mha <- read.xlsx("Data\\MHADatabase_DATA_2026-01-20OD.xlsx", detectDates = TRUE)
mha <- rename(mha, instrument_name=redcap_repeat_instrument, repeat_instance=redcap_repeat_instance)

#Cleaning MHA Database---------------------------------------------------------------------------------
#Filtering out unnecessary columns
mha_clean <- mha %>%
select(-redcap_survey_identifier, 
      -client_profile_timestamp, 
      -date_survey_completed_pre,
      -date_survey_completed_th, 
      -date_survey_completed_siq, 
      -date_survey_completed_pmldc,
      -date_survey_completed_who5,
      -date_survey_completed_whodas,
      -date_survey_completed_rhs15,
      -date_survey_completed_eq5d3,  
      -contains("missing_data")) %>%
#Removing columns with all missing values
select(where(~!all(is.na(.)))) %>%
#Filtering out unnecessary rows (pcl5, whodas_shortform, record id = 20, and those with incomplete/unverified instances)
#Record ID 20 is a record with only one completed instrument
filter(!instrument_name %in% c("pcl5", "whodas_shortform"),!record_id %in% 20) %>%
filter(case_when(instrument_name == "client_profile" ~ client_profile_complete == 2,
      instrument_name == "psychlops_pretherapy" ~ psychlops_pretherapy_complete == 2,
      instrument_name == "trauma_history" ~ trauma_history_complete == 2,
      instrument_name == "hopkins_symptom_checklist" ~ hopkins_symptom_checklist_complete == 2,
      instrument_name == "siq" ~ siq_complete == 2,
      instrument_name == "pmldc" ~ pmldc_complete == 2,
      instrument_name == "who5" ~ who5_complete == 2,
      instrument_name == "psychlops_duringtherapy" ~ psychlops_duringtherapy_complete == 2,
      instrument_name == "psychlops_posttherapy" ~ psychlops_posttherapy_complete == 2,
      instrument_name == "rhs15" ~ rhs15_complete == 2,
      instrument_name == "eq5d3l" ~ eq5d3l_complete == 2,
      TRUE ~ FALSE)) %>%
#Trimming and fixing capitalization for character variables
mutate(across(where(is.character) & !matches("instrument_name"), ~ str_to_title(str_trim(.)))) %>%
mutate(legal_status = case_match(legal_status, "A03, Asylee" ~ "Asylee", "Siv" ~ "SIV",
      .default = legal_status), 
nationality = case_match(nationality, "Venezuelan" ~ "Venezuela", "Drc" ~ "DRC",
      .default = nationality)) %>%
#Converting dob to date format
mutate(dob = as.Date(dob))

#Count how many instruments each participant has completed
mha_clean %>% group_by(record_id) %>% 
summarize(n_instruments = n_distinct(instrument_name))

#Count how many times each instrument appears 
mha_clean %>% group_by(instrument_name) %>% summarize(n = n())

#Creating data frames for baseline assessments---------------------------------------------------------
#Extracting baseline data by grouping by record id and instrument name, then filtering for earliest instances
baseline <- mha_clean %>% group_by(record_id, instrument_name) %>%
arrange(repeat_instance) %>% slice(1) %>% ungroup()

#Pivoting wider baseline data
baseline_wide <- baseline %>%
pivot_wider(id_cols = record_id, names_from = instrument_name, 
values_from = -c(record_id, instrument_name, repeat_instance), 
names_glue = "{instrument_name}_{.value}") %>%
#Removing columns with all missing values
select(where(~!all(is.na(.)))) 
#Removing uncessary columns from baseline_wide
baseline_wide <- baseline_wide %>% select(
      -contains("complete"), 
      -contains("date_survey_completed"), 
      -contains("missing_data"), 
      -starts_with("psychlops_duringtherapy"), 
      -starts_with("psychlops_posttherapy"))

#Cleaning MHA Database---------------------------------------------------------------------------------
#Filtering out unnecessary columns
mha_clean <- mha %>%
select(-redcap_survey_identifier, 
      -client_profile_timestamp, 
      -date_survey_completed_pre,
      -date_survey_completed_th, 
      -date_survey_completed_siq, 
      -date_survey_completed_pmldc,
      -date_survey_completed_who5,
      -date_survey_completed_whodas,
      -date_survey_completed_rhs15,
      -date_survey_completed_eq5d3,  
      -contains("missing_data")) %>%
#Removing columns with all missing values
select(where(~!all(is.na(.)))) %>%
#Filtering out unnecessary rows (pcl5, whodas_shortform, record id = 20, and those with incomplete/unverified instances)
#Record ID 20 is a record with only one completed instrument
filter(!instrument_name %in% c("pcl5", "whodas_shortform"),!record_id %in% 20) %>%
filter(case_when(instrument_name == "client_profile" ~ client_profile_complete == 2,
      instrument_name == "psychlops_pretherapy" ~ psychlops_pretherapy_complete == 2,
      instrument_name == "trauma_history" ~ trauma_history_complete == 2,
      instrument_name == "hopkins_symptom_checklist" ~ hopkins_symptom_checklist_complete == 2,
      instrument_name == "siq" ~ siq_complete == 2,
      instrument_name == "pmldc" ~ pmldc_complete == 2,
      instrument_name == "who5" ~ who5_complete == 2,
      instrument_name == "psychlops_duringtherapy" ~ psychlops_duringtherapy_complete == 2,
      instrument_name == "psychlops_posttherapy" ~ psychlops_posttherapy_complete == 2,
      instrument_name == "rhs15" ~ rhs15_complete == 2,
      instrument_name == "eq5d3l" ~ eq5d3l_complete == 2,
      TRUE ~ FALSE)) %>%
#Trimming and fixing capitalization for character variables
mutate(across(where(is.character) & !matches("instrument_name"), ~ str_to_title(str_trim(.)))) %>%
mutate(legal_status = case_match(legal_status, "A03, Asylee" ~ "Asylee", "Siv" ~ "SIV",
      .default = legal_status), 
nationality = case_match(nationality, "Venezuelan" ~ "Venezuela", "Drc" ~ "DRC",
      .default = nationality)) %>%
#Converting dob to date format
mutate(dob = as.Date(dob))

#Count how many instruments each participant has completed
mha_clean %>% group_by(record_id) %>% 
summarize(n_instruments = n_distinct(instrument_name))

#Count how many times each instrument appears 
mha_clean %>% group_by(instrument_name) %>% summarize(n = n())

#Creating data frames for baseline assessments---------------------------------------------------------
#Extracting baseline data by grouping by record id and instrument name, then filtering for earliest instances
baseline <- mha_clean %>% group_by(record_id, instrument_name) %>%
arrange(repeat_instance) %>% slice(1) %>% ungroup()

#Pivoting wider baseline data
baseline_wide <- baseline %>%
pivot_wider(id_cols = record_id, names_from = instrument_name, 
values_from = -c(record_id, instrument_name, repeat_instance), 
names_glue = "{instrument_name}_{.value}") %>%
#Removing columns with all missing values
select(where(~!all(is.na(.)))) 
#Removing uncessary columns from baseline_wide
baseline_wide <- baseline_wide %>% select(
      -contains("complete"), 
      -contains("date_survey_completed"), 
      -contains("missing_data"), 
      -starts_with("psychlops_duringtherapy"), 
      -starts_with("psychlops_posttherapy"))

#Demographics Summary Statistics-----------------------------------------------------------------------
#Legal status distribution
legal_status_vis <- baseline_wide %>% count(client_profile_legal_status) %>% 
mutate(percent = n / sum(n) * 100) %>% arrange(desc(n))
baseline_wide %>% count(client_profile_legal_status) %>%
ggplot(aes(fct_reorder(client_profile_legal_status, n, .desc = TRUE), n)) + geom_col()+
geom_text(aes(label = n), vjust = -0.5, size = 4) +
labs(title = "Legal Status Distribution", x = "Legal Status", y = "Count") + theme_minimal()
legal_status_vis
#Gender distribution
gender_vis <- baseline_wide %>% count(client_profile_gender) %>% 
mutate(percent = n / sum(n) * 100) %>% arrange(desc(n))
baseline_wide %>% count(client_profile_gender) %>%
ggplot(aes(fct_reorder(client_profile_gender, n, .desc = TRUE), n)) + geom_col()+
geom_text(aes(label = n), vjust = -0.5, size = 4) +
labs(title = "Gender Distribution", x = "Gender", y = "Count") + theme_minimal()
gender_vis
#Marital status distribution
marital_status_vis <- baseline_wide %>% count(client_profile_marital_status) %>% 
mutate(percent = n / sum(n) * 100) %>% arrange(desc(n))
baseline_wide %>% count(client_profile_marital_status) %>%
ggplot(aes(fct_reorder(client_profile_marital_status, n, .desc = TRUE), n)) + geom_col()+
geom_text(aes(label = n), vjust = -0.5, size = 4) +
labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count") + theme_minimal()
marital_status_vis
#Nationality distribution
nationality_vis <- baseline_wide %>% count(client_profile_nationality) %>% 
mutate(percent = n / sum(n) * 100) %>% arrange(desc(n))
baseline_wide %>% count(client_profile_nationality) %>%
ggplot(aes(fct_reorder(client_profile_nationality, n, .desc = TRUE), n)) + geom_col()+
geom_text(aes(label = n), vjust = -0.5, size = 4) +
labs(title = "Nationality Distribution", x = "Nationality", y = "Count") + theme_minimal()
nationality_vis
#Primary language distribution
primary_language_vis <- baseline_wide %>% count(client_profile_primary_language) %>% 
mutate(percent = n / sum(n) *100) %>% arrange(desc(n))
baseline_wide %>% count(client_profile_primary_language) %>%
ggplot(aes(fct_reorder(client_profile_primary_language, n, .desc = TRUE), n)) + geom_col()+
geom_text(aes(label = n), vjust = -0.5, size = 4) +
labs(title = "Primary Language Distribution", x = "Primary Language", y = "Count") + theme_minimal()
primary_language_vis
#English level distribution
english_level_vis <- baseline_wide %>% count(client_profile_english_level) %>% 
mutate(percent = n / sum(n) * 100) %>% arrange(desc(n))
baseline_wide %>% mutate(english_level = factor(client_profile_english_level,
levels = c(0, 1, 2, 3), labels = c("None", "Some", "Good", "Excellent"))) %>% 
count(english_level) %>% ggplot(aes(english_level, n)) + geom_col() +
geom_text(aes(label = n), vjust = -0.5, size = 4) + 
labs(title = "English Level Distribution", x = "English Proficiency", y = "Count") + theme_minimal()
english_level_vis

#Calculating age at intake and time since arrival in months and years
baseline_wide <- baseline_wide %>% mutate(
      intake_age = floor(interval(client_profile_dob, client_profile_date_of_intake) / years(1)),
      years_since_arrival = interval(client_profile_date_of_arrival, client_profile_date_of_intake) / years(1),
      months_since_arrival = interval(client_profile_date_of_arrival, client_profile_date_of_intake) / months(1))
#Calculating summary statistics for age at intake and time since arrival
age_summary_table <- baseline_wide %>% summarize(min_age = floor(min(intake_age, na.rm = TRUE)), 
      max_age = floor(max(intake_age, na.rm = TRUE)), 
      mean_age = floor(mean(intake_age, na.rm = TRUE)), 
      med_age = floor(median(intake_age, na.rm = TRUE)), 
      min_months = round(min(months_since_arrival, na.rm = TRUE), 2), 
      max_months = round(max(months_since_arrival, na.rm = TRUE), 2), 
      mean_months = round(mean(months_since_arrival, na.rm = TRUE), 2), 
      med_months = round(median(months_since_arrival, na.rm = TRUE), 2)) %>%
pivot_longer(everything()) %>% separate(name, into = c("Stat", "Measure"), sep = "_") %>%
mutate(Stat = case_match(Stat, "max" ~ "Max", "min" ~ "Min", "mean" ~ "Mean", "med" ~ "Median"),
Measure = case_match(Measure, "age" ~ "Age at Intake", "months" ~ "Months Since Arrival")) %>%
pivot_wider(names_from = Measure, values_from = value) %>% rename(Statistic = Stat) %>% gt() %>%
tab_header(title = "Age and Arrival Summary") %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2))
age_summary_table

#Creating age group variable; One group 18-34 (n = 9) and other 35+ (n = 10); Based on median
#Calculate age groups based on median (34)
baseline_wide <- baseline_wide %>% mutate(age_group = case_when(
      intake_age <= 34 ~ "Younger (18-34)", intake_age > 34 ~ "Older (35+)"))
#Verify the counts
table(baseline_wide$age_group)

#Creating df with demographics data
demographics_summary <- bind_rows(
      baseline_wide %>% count(Category = age_group) %>% mutate(Variable = "Age Group"),
      baseline_wide %>% count(Category = client_profile_legal_status) %>% mutate(Variable = "Legal Status"),
      baseline_wide %>% count(Category = client_profile_gender) %>% mutate(Variable = "Gender"),
      baseline_wide %>% count(Category = client_profile_marital_status) %>% mutate(Variable = "Marital Status"),
      baseline_wide %>% count(Category = client_profile_nationality) %>% mutate(Variable = "Nationality"),
      baseline_wide %>% count(Category = client_profile_primary_language) %>% mutate(Variable = "Primary Language"),
      baseline_wide %>% mutate(Category = factor(client_profile_english_level, levels = c(0, 1, 2, 3), 
            labels = c("None", "Some", "Good", "Excellent"))) %>% 
      count(Category) %>% mutate(Variable = "English Level", Category = as.character(Category))) %>% 
      mutate(Category = ifelse(Category %in% c("Unknown", "NA") | is.na(Category), "Missing", Category)) %>%
      group_by(Variable) %>% mutate(percent = n / sum(n) * 100) %>% ungroup() %>% 
      select(Variable, Category, n, percent)
#Creating demographics summary table
demographics_table <- demographics_summary %>% 
mutate(Variable = factor(Variable)) %>%
arrange(Variable, desc(n)) %>% group_by(Variable) %>% gt() %>%
tab_header(title = "Participant Demographics Summary",
      subtitle = "Total Participants (N = 20)") %>%
fmt_number(columns = percent, decimals = 1) %>%
cols_label(n = "Count (n)",percent = "Percentage (%)") %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2)) %>%
tab_style(style = cell_fill(color = "#ffeeee"), locations = cells_row_groups())
demographics_table

#Baseline Psychlops------------------------------------------------------------------------------------
#How much has it affected you over the last week?: 0-5 = Not at all-Severely Affected
#How hard has it been to do something about it over the last week?: 0-5 = Not at all-Very Hard
#How have you felt in yourself over the last week?: 0-5 = Very good-Very bad
#Psychlops Pre-Therapy Total Score (0-20 scale); Higher = Worse
baseline_wide <- baseline_wide %>% 
mutate(pre_psych_total = rowSums(select(., 
      psychlops_pretherapy_how_much_has_it_affected_y, 
      psychlops_pretherapy_how_much_has_it_affected_2, 
      psychlops_pretherapy_how_hard_has_it_been_to_do, 
      psychlops_pretherapy_how_have_you_felt_in_yours)))

summary(baseline_wide$pre_psych_total)
sd(baseline_wide$pre_psych_total, na.rm = TRUE)
pre_psychlops_vis <- baseline_wide %>% ggplot(aes(pre_psych_total)) +
geom_histogram(bins=10) + theme_minimal() +
labs(title = "PSYCHLOPS Pre-Therapy Score Distribution", 
x = "Total PSYCHLOPS Pre-Therapy Score", y = "Count")

#Baseline Hopkins Symptom Checklist--------------------------------------------------------------------
#1 = Not at all, 2 = A little, 3 = Quite a bit, 4 = Extremely
hopkins_items <- baseline_wide %>% 
select(starts_with("hopkins_symptom_checklist_"))
#Anxiety symptoms
anxiety_items <- hopkins_items %>% select(1:10)
#Depression symptoms
depression_items <- hopkins_items %>% select(11:25)

#Adding Hopkins score averages to baseline_wide
baseline_wide <- baseline_wide %>% 
mutate(hopkins_anxiety_avg = rowMeans(anxiety_items),
      hopkins_depression_avg = rowMeans(depression_items),
      hopkins_total_avg = rowMeans(hopkins_items))

summary(baseline_wide$hopkins_anxiety_avg)
summary(baseline_wide$hopkins_depression_avg)
summary(baseline_wide$hopkins_total_avg)

#Adding Hopkins score totals to baseline_wide (25-100 scale); Higher Score = Worse
baseline_wide <- baseline_wide %>% 
mutate(hopkins_anxiety_total = rowSums(anxiety_items),
      hopkins_depression_total = rowSums(depression_items),
      hopkins_total_sum = rowSums(hopkins_items))

summary(baseline_wide$hopkins_anxiety_total)
summary(baseline_wide$hopkins_depression_total)
summary(baseline_wide$hopkins_total_sum)
sd(baseline_wide$hopkins_total_sum, na.rm = TRUE)

#Trauma History----------------------------------------------------------------------------------------
#0 = Neither, 1 = Self, 2 = Others, 3 = Both
#Count of different types of trauma exposures (Up to 16); Higher = More types of trauma exposures
baseline_wide <- baseline_wide %>% mutate(
trauma_exposure_count = select(., starts_with("trauma_history_")) %>% 
mutate(across(everything(), ~ ifelse(. > 0, 1, 0))) %>% rowSums())

summary(baseline_wide$trauma_exposure_count)

#SIQ---------------------------------------------------------------------------------------------------
#0 = No, 1 = Yes
#SIQ Total Score (0-3 scale); Higher = Worse
baseline_wide <- baseline_wide %>% mutate(siq_total = rowSums(
select(., starts_with("siq_") & !ends_with("total"))))

summary(baseline_wide$siq_total)

#PMLDC-------------------------------------------------------------------------------------------------
#0 = Not a problem/Did not happen, 1 = Small Problem, 2 = Moderately Serious Problem, 3 = Very Serious Problem
#PMLDC (0-48 scale); Higher = Worse
baseline_wide <- baseline_wide %>%
mutate(pmldc_total = rowSums(select(., starts_with("pmldc_") & !ends_with("total"))))

summary(baseline_wide$pmldc_total)

#Standardized WHO-5------------------------------------------------------------------------------------
#0 = At no time, 1 = Sometimes, 2 = Less than half, 3 = More than half, 4 = Most of the time, 5 = All of the time
#WHO5 Total Score (0-100 scale); Lower = Worse
baseline_wide <- baseline_wide %>%
mutate(who5_total = rowSums(select(., 
      c("who5_i_have_felt_cheerful_in_go", 
      "who5_i_have_felt_calm_and_relax", 
      "who5_i_have_felt_active_and_vig", 
      "who5_i_woke_up_feeling_fresh_an", 
      "who5_my_daily_life_has_been_fil"))) * 4)

summary(baseline_wide$who5_total)

#RHS15-------------------------------------------------------------------------------------------------
#0 = Not at all, 1 = A little, 2 = Moderately, 3 = Quite a bit, 4 = Extremely
#Coping Question: 0 = Can handle anything, 1 = Handle most, 2 = Handle some, 3 = Unable to cope with most, 4 = Unable to cope
#Distress Thermometer: 0-10 = Great-Worse
#RHS15 Total Score (0-66 scale); Higher = Worse
baseline_wide <- baseline_wide %>% mutate(rhs15_total = rowSums(
select(., starts_with("rhs15_") & !ends_with("total"))))

summary(baseline_wide$rhs15_total)
summary(baseline_wide$rhs15_distress_thermometer_how_h)

#EQ5D3L------------------------------------------------------------------------------------------------
#1 = No problem, 2 = Some problem, 3 = Unable/Extremely bad
#EQ5D3L EQ VAS (0-100 Scale); Higher = Worse
summary(baseline_wide$eq5d3l_provide_a_score_from_0_100)

#Creating Summary Table for Baseline Assessments ------------------------------------------------------
baseline_assessment_summary <- baseline_wide %>% summarize( #summarize() can be replaced with reframe()
`PSYCHLOPS Total (0-20)` = c(mean(pre_psych_total, na.rm = TRUE), median(pre_psych_total, na.rm = TRUE), 
      min(pre_psych_total, na.rm = TRUE), max(pre_psych_total, na.rm = TRUE), 
      sd(pre_psych_total, na.rm = TRUE)),  
`HSCL-25 Average (1-4)` = c(mean(hopkins_total_avg, na.rm = TRUE), median(hopkins_total_avg, na.rm = TRUE), 
      min(hopkins_total_avg, na.rm = TRUE), max(hopkins_total_avg, na.rm = TRUE), 
      sd(hopkins_total_avg, na.rm = TRUE)),
#`HSCL-25 Total (25-100)` = c(mean(hopkins_total_sum, na.rm = TRUE), median(hopkins_total_sum, na.rm = TRUE), 
#      min(hopkins_total_sum, na.rm = TRUE), max(hopkins_total_sum, na.rm = TRUE),0 
#      sd(hopkins_total_sum, na.rm = TRUE)),
#`HSCL-25 Anxiety (0-40)` = c(mean(hopkins_anxiety_total, na.rm = TRUE), median(hopkins_anxiety_total, na.rm = TRUE), 
#      min(hopkins_anxiety_total, na.rm = TRUE), max(hopkins_anxiety_total, na.rm = TRUE), 
#      sd(hopkins_anxiety_total, na.rm = TRUE)),
#`HSCL-25 Depression (0-60)` = c(mean(hopkins_depression_total, na.rm = TRUE), median(hopkins_depression_total, na.rm = TRUE), 
#      min(hopkins_depression_total, na.rm = TRUE), max(hopkins_depression_total, na.rm = TRUE), 
#      sd(hopkins_depression_total, na.rm = TRUE)),
`Trauma Exposure Count (0-16)` = c(mean(trauma_exposure_count, na.rm = TRUE), median(trauma_exposure_count, na.rm = TRUE), 
      min(trauma_exposure_count, na.rm = TRUE), max(trauma_exposure_count, na.rm = TRUE), 
      sd(trauma_exposure_count, na.rm = TRUE)),  
`PMLDC Total (0-48)` = c(mean(pmldc_total, na.rm = TRUE), median(pmldc_total, na.rm = TRUE),
      min(pmldc_total, na.rm = TRUE), max(pmldc_total, na.rm = TRUE), 
      sd(pmldc_total, na.rm = TRUE)),
`SIQ Total (0-3)` = c(mean(siq_total, na.rm = TRUE), median(siq_total, na.rm = TRUE), 
       min(siq_total, na.rm = TRUE), max(siq_total, na.rm = TRUE), 
      sd(siq_total, na.rm = TRUE)),
`WHO-5 Total (0-100)*` = c(mean(who5_total, na.rm = TRUE), median(who5_total, na.rm = TRUE),
      min(who5_total, na.rm = TRUE), max(who5_total, na.rm = TRUE), 
      sd(who5_total, na.rm = TRUE)),
`EQ VAS (0-100)*` = c(mean(eq5d3l_provide_a_score_from_0_100, na.rm = TRUE), 
      median(eq5d3l_provide_a_score_from_0_100, na.rm = TRUE), 
      min(eq5d3l_provide_a_score_from_0_100, na.rm = TRUE), 
      max(eq5d3l_provide_a_score_from_0_100, na.rm = TRUE), 
      sd(eq5d3l_provide_a_score_from_0_100, na.rm = TRUE)),
`RHS-15 Total (0-66)` = c(mean(rhs15_total, na.rm = TRUE), median(rhs15_total, na.rm = TRUE),
      min(rhs15_total, na.rm = TRUE), max(rhs15_total, na.rm = TRUE), 
      sd(rhs15_total, na.rm = TRUE)),
`RHS-15 Distress Thermometer (0-10)` = c(mean(rhs15_distress_thermometer_how_h, na.rm = TRUE), 
      median(rhs15_distress_thermometer_how_h, na.rm = TRUE), 
      min(rhs15_distress_thermometer_how_h, na.rm = TRUE), 
      max(rhs15_distress_thermometer_how_h, na.rm = TRUE), 
      sd(rhs15_distress_thermometer_how_h, na.rm = TRUE))) %>%
mutate(Statistic = c("Mean", "Median", "Min", "Max", "SD")) %>%
pivot_longer(-Statistic, names_to = "Assessment", values_to = "Value") %>%
pivot_wider(names_from = Statistic, values_from = Value)
baseline_assessment_summary

baseline_scores_table <- baseline_assessment_summary %>% gt() %>% tab_header(
title = "Baseline Clinical Assessment Summary") %>%
fmt_number(columns = c(Mean, Median, Min, Max, SD), decimals = 2) %>%
cols_label(Assessment = "Assessment Tool (Scale Range)", Mean = "Mean", Median = "Median", Min = "Min", Max = "Max", SD = "SD") %>%
#tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
tab_source_note(
source_note = "*Higher scores indicate greater distress, except for WHO-5 and EQ VAS where lower scores indicate worse well-being.") %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2))
baseline_scores_table     

#Creating data frames for follow-up and post-therapy assessments---------------------------------------
during_psych <- mha_clean %>% filter(instrument_name == "psychlops_duringtherapy")
hopkins <- mha_clean %>% filter(instrument_name == "hopkins_symptom_checklist", repeat_instance > 1)
psych_post <- mha_clean %>% filter(instrument_name == "psychlops_posttherapy") 

during_psych_wide <- during_psych %>%
      pivot_wider(id_cols = c(record_id, repeat_instance), names_from = instrument_name, 
      values_from = -c(record_id, instrument_name, repeat_instance),
      names_glue = "{instrument_name}_{.value}")
hopkins_wide <- hopkins %>%
      pivot_wider(id_cols = c(record_id, repeat_instance), names_from = instrument_name, 
      values_from = -c(record_id, instrument_name, repeat_instance),
      names_glue = "{instrument_name}_{.value}")
psych_post_wide <- psych_post %>%
      pivot_wider(id_cols = c(record_id, repeat_instance), names_from = instrument_name, 
      values_from = -c(record_id, instrument_name, repeat_instance),
      names_glue = "{instrument_name}_{.value}")

followup_wide <- during_psych_wide %>% 
      left_join(hopkins_wide, by = c("record_id", "repeat_instance")) %>%
      left_join(psych_post_wide, by = c("record_id", "repeat_instance")) %>%
      select(where(~!all(is.na(.)))) 

followup_wide <- followup_wide %>% select(
      -contains("complete"), 
      -contains("date_survey_completed"), 
      -contains("missing_data"))

#Follow-up Psychlops-----------------------------------------------------------------------------------
followup_wide <- followup_wide %>%
mutate(during_psych_total = rowSums(select(., 
      psychlops_duringtherapy_how_much_has_it_affected_3,
      psychlops_duringtherapy_how_much_has_it_affected_4,
      psychlops_duringtherapy_how_hard_has_it_been_to_d2,
      psychlops_duringtherapy_how_have_you_felt_in_your2)))

#Follow-up Hopkins-------------------------------------------------------------------------------------
hopkins_items2 <- followup_wide %>% 
select(starts_with("hopkins_symptom_checklist_"))
anxiety_items <- hopkins_items2 %>% select(1:10)
depression_items <- hopkins_items2 %>% select(11:25)

followup_wide <- followup_wide %>% 
mutate(hopkins_anxiety_avg = rowMeans(anxiety_items),
      hopkins_depression_avg = rowMeans(depression_items),
      hopkins_total_avg = rowMeans(hopkins_items2))

summary(followup_wide$hopkins_anxiety_avg)
summary(followup_wide$hopkins_depression_avg)
summary(followup_wide$hopkins_total_avg)

followup_wide <- followup_wide %>% 
mutate(hopkins_anxiety_total = rowSums(anxiety_items),
      hopkins_depression_total = rowSums(depression_items),
      hopkins_total_sum = rowSums(hopkins_items2))

summary(followup_wide$hopkins_anxiety_total)
summary(followup_wide$hopkins_depression_total)
summary(followup_wide$hopkins_total_sum)

#Post Psychlops----------------------------------------------------------------------------------------
followup_wide <- followup_wide %>% 
mutate(post_psych_total = rowSums(select(., 
      psychlops_posttherapy_how_much_has_it_affected_5, 
      psychlops_posttherapy_how_much_has_it_affected_6, 
      psychlops_posttherapy_how_hard_has_it_been_to_d3, 
      psychlops_posttherapy_how_have_you_felt_in_your3)))

#Creating a combined dataset for both PSYCHLOPS and Hopkins--------------------------------------------
analysis_long <- bind_rows(
baseline_wide %>% 
      transmute(record_id, PSYCHLOPS = pre_psych_total, `HSCL-25` = hopkins_total_avg, timepoint = "Baseline"), 
followup_wide %>% 
      transmute(record_id, PSYCHLOPS = during_psych_total, `HSCL-25` = hopkins_total_avg, timepoint = "Follow-up"), 
followup_wide %>% 
      transmute(record_id, PSYCHLOPS = post_psych_total, timepoint = "Post-Therapy")) %>% 
pivot_longer(cols = c(PSYCHLOPS, `HSCL-25`), names_to = "instrument", values_to = "score") %>% 
filter(!is.na(score))

#Converting timepoint and instrument to factor
analysis_long <- analysis_long %>%
mutate(timepoint = factor(timepoint, levels = c("Baseline", "Follow-up", "Post-Therapy")), instrument = factor(instrument))

#Visualize Change Over Time
psych_hopkins_change <- ggplot(analysis_long, aes(timepoint, score, group = record_id)) +
geom_point(alpha = 0.3) + geom_line(alpha = 0.3, color = "#008cff") + 
stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2, color = "red") +
facet_wrap(~instrument, scales = "free") + theme_minimal() + labs(title = "Treatment Progress by Instrument",
subtitle = "Red lines represents the mean score.\nHSCH-25 was not administered at Post-Therapy.", 
x = NULL, y = "Score")
psych_hopkins_change

#Linear Mixed Models (LMM)-----------------------------------------------------------------------------
      #t-test was considered but not used since only four participants have both baseline and post-therapy data for PSYCHLOPS and since it does not include all available data
      #Linear mixed model is used to account for repeated measures and varying time points across participants and to include all available data without excluding those with missing time points

#LMM for PSYCHLOPS (Random intercept only)
psychlops_model <- lmer(score ~ timepoint + (1 | record_id), 
data = analysis_long %>% filter(instrument == "PSYCHLOPS"))

#LMM for Hopkins (Random intercept only)
hopkins_model <- lmer(score ~ timepoint + (1 | record_id), 
data = analysis_long %>% filter(instrument == "HSCL-25"))

summary(psychlops_model)
summary(hopkins_model)
anova(psychlops_model)
anova(hopkins_model)

#LMM for PSYCHLOPS (Random intercept and slope) - does not work due to convergence issues, likely due to small sample size and limited time points
#psychlops_model2 <- lmer(score ~ timepoint + (timepoint | record_id),
#data = analysis_long %>% filter(instrument == "PSYCHLOPS"))
#summary(psychlops_model2)
#anova(psychlops_model2)

#LMM for Hopkins (Random intercept and slope) - does not work due to convergence issues, likely due to small sample size and limited time points
#hopkins_model2 <- lmer(score ~ timepoint + (timepoint | record_id), 
#data = analysis_long %>% filter(instrument == "HSCL-25"))
#summary(hopkins_model2)
#anova(hopkins_model2)

#Checking assumptions of LMMs
#Check homoscedasticity (residuals have constant variance across fitted values); Appears to be relatively homoscedastic for both models
psychlops_residual <- plot(psychlops_model, main = "PSYCHLOPS Model: Residuals vs Fitted") 
psychlops_residual
hopkins_residual <- plot(hopkins_model, main = "Hopkins Model: Residuals vs Fitted")
hopkins_residual
#Check normality; Appears to be relatively normal for both models
psychlops_qq <- qqPlot(residuals(psychlops_model), main = "PSYCHLOPS Model: Q-Q Plot of Residuals")
psychlops_qq
hopkins_qq <- qqPlot(residuals(hopkins_model), main = "Hopkins Model: Q-Q Plot of Residuals")
hopkins_qq

#LMM for PSYCHLOPS for dose-response relationship
#psychlops_sessions <- analysis_long %>% group_by(record_id) %>% 
#filter(instrument == "PSYCHLOPS") %>% mutate(total_sessions = n()) %>% ungroup()
#psychlops_dose_response_model <- lmer(score ~ timepoint * total_sessions + 
#(1 | record_id), data = psychlops_sessions)
#summary(psychlops_dose_response_model)

#Comparing PSYCHLOPS improvement rates by subgroup controlling for gender
      #Some variables (legal status, nationality, and primary language) experienced rank deficiencies and "missing cells" during the LMM
      #After collapsing categories, legal status and primary language still has rank deficiencies and missing cells; however, the rank deficiency is less severe than before
      #Legal Status and Language because no participants in the "Other" sub-categories reached the Post-Therapy assessment (missing cells). 
      #Findings for these two variables primarily reflect progress between Baseline and Follow-up.
      #Significant Results:
#1. Gender (Interaction, p = 0.0025137): The most significant interaction, showing highly distinct trajectories
#2. Primary Language (Interaction, p = 0.01258)
#3. Legal Status (Interaction, p = 0.033448)
#4. Region (Interaction, p = 0.02171) 
#5. English Level (Interaction, p = 0.07277): Close to significance
#6. Age Group (Main Effect, p = 0.005413)
#7. Legal Status (Main Effect, p = 0.031459)
#8. Language (Main Effect, p = 0.03524)

#Adding demographics data to analysis df
analysis_long_demo <- analysis_long %>% left_join(baseline_wide, by = "record_id") %>%
select(record_id, timepoint, instrument, score, 
      age_group, 
      client_profile_gender, 
      client_profile_legal_status, 
      client_profile_marital_status, 
      client_profile_nationality, 
      client_profile_primary_language, 
      client_profile_english_level)

#Collapsing variables with Missing Cells/Rank Deficiencies
analysis_long_demo <- analysis_long_demo %>%
mutate(
legal_status_simple = if_else(client_profile_legal_status == "Refugee", "Refugee", "Other (Asylee/SIV/Parolee)"),
region = case_when(client_profile_nationality %in% c("Venezuela", "Colombia", "Guatemala", "Nicaragua") ~ "Latin America",
      client_profile_nationality %in% c("Afghanistan", "Iran", "Syria") ~ "Middle East/Central Asia",
      client_profile_nationality %in% c("DRC", "Eritrea", "Burma") ~ "Africa/Asia", TRUE ~ "Other Regions"),
language_simple = case_when(client_profile_primary_language == "Spanish" ~ "Spanish",
      client_profile_primary_language %in% c("Dari", "Pashto", "Farsi", "Arabic") ~ "Middle Eastern/Central Asian",
      TRUE ~ "Other (African/Asian)"))

#Age Group (Younger/Older)
age_improvement_model <- lmer(score ~ timepoint * age_group + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#Gender
gender_improvement_model <- lmer(score ~ timepoint * client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#Legal Status (Consider excluding due to rank deficiency and "missing cells" issues)
#legal_improvement_model <- lmer(score ~ timepoint * client_profile_legal_status + (1 | record_id), 
#      data = analysis_long_demo %>% filter(instrument == "Psychlops"))
legal_improvement_model <- lmer(score ~ timepoint * legal_status_simple + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#Marital Status
marital_improvement_model <- lmer(score ~ timepoint * client_profile_marital_status + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#Region (Consider excluding due to rank deficiency and "missing cells" issues)
#nationality_improvement_model <- lmer(score ~ timepoint * client_profile_nationality + (1 | record_id), 
#      data = analysis_long_demo %>% filter(instrument == "Psychlops"))
region_improvement_model <- lmer(score ~ timepoint * region + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#Primary Language (Consider excluding due to rank deficiency and "missing cells" issues)
#language_improvement_model <- lmer(score ~ timepoint * client_profile_primary_language + (1 | record_id), 
#      data = analysis_long_demo %>% filter(instrument == "Psychlops"))
language_improvement_model <- lmer(score ~ timepoint * language_simple + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))
#English Level; Converted to numeric since it is an ordinal variable
english_improvement_model <- lmer(score ~ timepoint * as.numeric(client_profile_english_level) + client_profile_gender + (1 | record_id), 
      data = analysis_long_demo %>% filter(instrument == "PSYCHLOPS"))

anova(age_improvement_model)
anova(gender_improvement_model)
anova(legal_improvement_model)
anova(marital_improvement_model)
#anova(nationality_improvement_model)
anova(region_improvement_model)
anova(language_improvement_model)
anova(english_improvement_model)

#Making table with Significant Interactions and Main Effects
subgroup_list <- list(Age = anova(age_improvement_model),
      Gender = anova(gender_improvement_model), 
      Legal = anova(legal_improvement_model),
      Marital = anova(marital_improvement_model),
      Region = anova(region_improvement_model),
      Language = anova(language_improvement_model),
      English = anova(english_improvement_model))

subgroup_table <- map_df(names(subgroup_list), ~ subgroup_list[[.x]] %>% as.data.frame() %>% 
rownames_to_column("Effect") %>% mutate(Variable = .x, Effect_Type = case_when(
      grepl("timepoint:", Effect) ~ "Interaction Effect",
      grepl(":", Effect) ~ "Interaction Effect",
      TRUE ~ "Main Effect")), .id = NULL)
model_term <- function(x) {x %>%
      str_replace("timepoint:", "Timepoint × ") %>%
      str_replace("client_profile_gender", "Gender") %>%
      str_replace("language_simple", "Primary Language") %>%
      str_replace("legal_status_simple", "Legal Status") %>%
      str_replace("region", "Region") %>%
      str_replace("age_group", "Age Group")}
subgroup_table

subgroup_table1 <- subgroup_table %>% filter(`Pr(>F)` < 0.05, Effect != "timepoint", 
Effect != "client_profile_gender") %>% 
mutate(Variable = case_when(Variable == "Language" ~ "Primary Language", 
      Variable == "Legal" ~ "Legal Status", TRUE ~ Variable)) %>%
mutate(Effect = model_term(Effect)) %>%
rename(`F-Statistic` = `F value`, `p-value` = `Pr(>F)`) %>% arrange(`p-value`) %>% 
select(Variable, Effect, NumDF, DenDF, `F-Statistic`, `p-value`, Effect_Type) %>% 
group_by(Effect_Type) %>% gt() %>%
tab_header(title = "Significant Subgroup Effects on PSYCHLOPS Scores") %>%
cols_merge(columns = c(NumDF, DenDF), pattern = "{1}, {2}") %>%
cols_label(Variable = "Variable", Effect = "Model Term", NumDF = "df (num, den)", `p-value` = "p-value") %>%
fmt_number(columns = `F-Statistic`, decimals = 2) %>%
fmt_number(columns = `p-value`, decimals = 3) %>%
fmt_number(columns = DenDF, decimals = 1) %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2)) %>%
tab_style(style = cell_fill(color = "#ffeeee"), locations = cells_row_groups()) %>%
tab_footnote(footnote = "Only effects with p<.05 are shown")
subgroup_table1

#mutate(sig = case_when( 
#`p-value` < .001 ~ "***",
#`p-value` < .01 ~ "**",
#`p-value` < .05 ~ "*", TRUE ~ ""))

#Visualizing PSYCHLOPS improvement rates by Gender
psychlops_subgroup_gender_vis <- analysis_long_demo %>% filter(instrument == "PSYCHLOPS", client_profile_gender != "Unknown") %>%
ggplot(aes(x = timepoint, y = score, group = record_id, color = client_profile_gender)) +
geom_line(alpha = 0.4) + stat_summary(aes(group = client_profile_gender), fun = mean, geom = "line", linewidth = 1.5) +
facet_wrap(~client_profile_gender) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Gender",
subtitle = "Significant interaction found (p = .003)", x = "Timepoint", y = "PSYCHLOPS Score") + theme(legend.position = "none")
psychlops_subgroup_gender_vis
#Visualizing PSYCHLOPS improvement rates by Legal Status
psychlops_subgroup_legal_vis <- analysis_long_demo %>% filter(instrument == "PSYCHLOPS") %>%
ggplot(aes(x = timepoint, y = score, group = record_id, color = legal_status_simple)) +
geom_line(alpha = 0.4) +  stat_summary(aes(group = legal_status_simple), fun = mean, geom = "line", linewidth = 1.5) +
facet_wrap(~legal_status_simple) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Legal Status",
subtitle = "Significant interaction (p = .033); 'Other' group limited to Follow-up", 
x = "Timepoint", y = "PSYCHLOPS Score") + theme(legend.position = "none")
psychlops_subgroup_legal_vis
#Visualizing PSYCHLOPS improvement rates by Region
psychlops_subgroup_region_vis <- analysis_long_demo %>% filter(instrument == "PSYCHLOPS") %>%
ggplot(aes(x = timepoint, y = score, group = record_id, color = region)) + 
geom_line(alpha = 0.4) + stat_summary(aes(group = region), fun = mean, geom = "line", linewidth = 1.5) +
facet_wrap(~region) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Region",
subtitle = "Significant interaction (p = .022)", x = "Timepoint", y = "PSYCHLOPS Score") + theme(legend.position = "none")
psychlops_subgroup_region_vis
#Visualizing PSYCHLOPS improvement rates by Primary Language
psychlops_subgroup_language_vis <- analysis_long_demo %>% filter(instrument == "PSYCHLOPS") %>%
ggplot(aes(x = timepoint, y = score, group = record_id, color = language_simple)) + geom_line(alpha = 0.4) + 
stat_summary(aes(group = language_simple), fun = mean, geom = "line", linewidth = 1.5) +
facet_wrap(~language_simple) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Primary Language",
subtitle = "Significant interaction (p = .013); 'Other' group limited to Follow-up", 
x = "Timepoint", y = "PSYCHLOPS Score") + theme(legend.position = "none")
psychlops_subgroup_language_vis
combined_psychlops_vis <- psychlops_subgroup_gender_vis + psychlops_subgroup_region_vis + psychlops_subgroup_legal_vis + psychlops_subgroup_language_vis +
plot_layout(ncol = 2)
combined_psychlops_vis

#Visualizing PSYCHLOPS improvement rates by Gender
#gender_pred <- ggpredict(gender_improvement_model, terms = c("timepoint", "client_profile_gender"))
#psychlops_subgroup_gender_vis <- gender_pred %>% filter(!group %in% "Unknown") %>%
#ggplot(aes(x, predicted, color = group, group = group)) + geom_line(linewidth = 1.8) +
#geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
#facet_wrap(~group) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Gender",
#      subtitle = "Model-estimated trajectories with 95% CI", x = "Timepoint", y = "Predicted PSYCHLOPS Score") +
#      theme(legend.position = "none")
#Visualizing PSYCHLOPS improvement rates by Legal Status
#legal_pred <- ggpredict(legal_improvement_model, terms = c("timepoint", "legal_status_simple"))
#psychlops_subgroup_legal_vis <- legal_pred %>%
#ggplot(aes(x, predicted, color = group, group = group)) + geom_line(linewidth = 1.8) +
#geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
#facet_wrap(~group) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Legal Status",
#      subtitle = "Model-estimated trajectories with 95% CI", x = "Timepoint", y = "Predicted PSYCHLOPS Score") +
#      theme(legend.position = "none")
#Visualizing PSYCHLOPS improvement rates by Region
#region_pred <- ggpredict(region_improvement_model, terms = c("timepoint", "region"))
#psychlops_subgroup_region_vis <- region_pred %>% 
#ggplot(aes(x, predicted, color = group, group = group)) + geom_line(linewidth = 1.8) +
#geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
#facet_wrap(~group) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Region",
#      subtitle = "Model-estimated trajectories with 95% CI", x = "Timepoint", y = "Predicted PSYCHLOPS Score") +
#      theme(legend.position = "none")
#Visualizing PSYCHLOPS improvement rates by Primary Language
#language_pred <- ggpredict(language_improvement_model, terms = c("timepoint", "language_simple"))
#psychlops_subgroup_language_vis <- language_pred %>%
#ggplot(aes(x, predicted, color = group, group = group)) + geom_line(linewidth = 1.8) +
#geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
#facet_wrap(~group) + theme_minimal() + labs(title = "PSYCHLOPS Improvement by Primary Language",
#      subtitle = "Model-estimated trajectories with 95% CI", x = "Timepoint", y = "Predicted PSYCHLOPS Score") +
#      theme(legend.position = "none")
#combined_psychlops_vis <- psychlops_subgroup_gender_vis + psychlops_subgroup_region_vis + psychlops_subgroup_legal_vis + psychlops_subgroup_language_vis +
#plot_layout(ncol = 2)

#Estimated Marginal Means (EMMeans) and Pairwise Comparisons-------------------------------------------
      #Post-hoc pairwise comparisons for PSYCHLOPS model to examine significance between time points
      ###Not conducted for Hopkins model since it was not administered at Post-Therapy and does not include all three time points
      #There is a statistically significant improvement from Baseline to Follow-up, and that improvement is sustained through Post-Therapy

psychlops_comparisons <- emmeans(psychlops_model, pairwise ~ timepoint)
psychlops_emmeans <- as.data.frame(psychlops_comparisons$emmeans)
psychlops_contrasts <- as.data.frame(psychlops_comparisons$contrasts)

#Creating EMMeans table and plot for PSYCHLOPS model
psychlops_contrasts_renamed <- psychlops_contrasts %>%
mutate(contrast = case_match(contrast,
      "Baseline - (Follow-up)" ~ "Baseline vs. Follow-up",
      "Baseline - (Post-Therapy)" ~ "Baseline vs. Post-Therapy",
      "(Follow-up) - (Post-Therapy)" ~ "Follow-up vs. Post-Therapy"))
      
psychlops_emmeans_table <- psychlops_contrasts_renamed %>% 
as.data.frame() %>% gt() %>% 
tab_header(title = "Estimated Pairwise Differences in PSYCHLOPS Scores") %>%
fmt_number(columns = c(estimate, SE, df, t.ratio), decimals = 2) %>%
fmt_number(columns = p.value, decimals = 3) %>%
text_transform(locations = cells_body(columns = p.value, rows = p.value < 0.001),
      fn = function(x) "<.001") %>%
cols_label(contrast = "Comparison", estimate = "Difference",
      t.ratio = "t-value", p.value = "p-value") %>%
tab_style(style = cell_text(weight = "bold"),
      locations = cells_body(columns = p.value, rows = p.value < 0.05)) %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2)) #%>%
#tab_footnote(footnote = "Sample sizes: Baseline n=20; Follow-up n=27; Post-Therapy n=4; Hopkins Follow-up n=8.")
psychlops_emmeans_table

#Visualize estimated marginal means for PSYCHLOPS model
psychlops_emmeans_vis <- psychlops_emmeans %>% ggplot(aes(timepoint, emmean, group = 1)) +
geom_point(size = 3) + geom_line(color = "#008cff", linewidth = 1) +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
theme_minimal() + labs(title = "Estimated PSYCHLOPS Score Trend", 
subtitle = "Error bars represent 95% Confidence Intervals", 
x = "Assessment Timepoint", y = "Predicted PSYCHLOPS Score")
psychlops_emmeans_vis

#EMM and pairwise comparisons for Hopkins model
hopkins_comparisons <- emmeans(hopkins_model, pairwise ~ timepoint)
hopkins_emmeans <- as.data.frame(hopkins_comparisons$emmeans)
hopkins_contrasts <- as.data.frame(hopkins_comparisons$contrasts)

#Effect Size Calculation (Cohen's d)-------------------------------------------------------------------
#Calculating Cohen's d for the PSYCHLOPS model to measure effect size and magnitude of change between time points; 
      #Interpreting Cohen's d: 0.2 standard deviation (SD) = Small, 0.5 SD = Medium, 0.8 SD = Large
psychlops_effect_size <- eff_size(psychlops_comparisons, 
sigma = sigma(psychlops_model), edf = df.residual(psychlops_model)) %>% as.data.frame()

#Creating effect size table for PSYCHLOPS model
psychlops_effect_size_renamed <- psychlops_effect_size %>%
mutate(contrast = case_match(contrast,
      "(Baseline - (Follow-up))" ~ "Baseline vs. Follow-up",
      "(Baseline - (Post-Therapy))" ~ "Baseline vs. Post-Therapy",
      "((Follow-up) - (Post-Therapy))" ~ "Follow-up vs. Post-Therapy")) 

psychlops_effect_size_table <- psychlops_effect_size_renamed %>% 
select(contrast, SE, df, lower.CL, upper.CL, effect.size) %>% gt() %>% 
tab_header(title = "Effect Sizes for PSYCHLOPS Pairwise Comparisons") %>%
fmt_number(columns = c(effect.size, SE, df, lower.CL, upper.CL), decimals = 2) %>%
cols_label(contrast = "Comparison", effect.size = "Cohen's d", 
lower.CL = "Lower CI", upper.CL = "Upper CI") %>%
tab_style(style = cell_text(weight = "bold"), 
locations = cells_body(columns = effect.size, rows = contrast %in% 
      c("Baseline vs. Follow-up", "Baseline vs. Post-Therapy"))) %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2)) #%>%
#tab_footnote(footnote = "Sample sizes: Baseline n=20; Follow-up n=27; Post-Therapy n=4; Hopkins Follow-up n=8.")
psychlops_effect_size_table

#Visualize effect sizes for PSYCHLOPS Model
psychlops_effect_size_vis <- psychlops_effect_size_renamed %>%
ggplot(aes(fct_reorder(contrast, effect.size), effect.size)) +
geom_point(size = 3, color = "#008cff") +
geom_hline(yintercept = 0, linetype = "dotted", color = "#000000") +
geom_hline(yintercept = 0.5, linetype = "dotted", color = "#008cff") +
geom_hline(yintercept = 0.8, linetype = "dotted", color = "red") +
geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1, color = "#008cff") +
coord_flip() + labs(title = "PSYCHLOPS Treatment Effect Sizes",
subtitle = "Cohen's d with 95% Confidence Intervals\nBlack dashed line = no effect (0); blue dotted line = medium effect (0.5); red dotted line = large effect (0.8)",
x = NULL, y = "Effect Size (Cohen's d)") + theme_minimal()

#Attrition Analysis (comparing those who stayed in the program vs. those who didn't)-------------------
#Follow-Up = Those with at least one follow-up assessment; Baseline Only = Those with only baseline data and no follow-up assessments
#IDs of participants with at least one follow-up 
with_followup <- analysis_long %>% filter(timepoint %in% c("Follow-up", "Post-Therapy")) %>%
pull(record_id) %>% unique() 
#IDs of participants with only baseline data
all_baseline <- analysis_long %>% filter(timepoint == "Baseline") %>% pull(record_id) %>%
unique() 
baseline_only <- setdiff(all_baseline, with_followup)
#Adding attrition status to baseline_wide
baseline_wide <- baseline_wide %>%
mutate(status = if_else(record_id %in% with_followup, "Follow-up", "Baseline Only"))

#Running Welch's two sample t-test between Follow-Up and Baseline Only for baseline PSYCHLOPS
      #p-value = 0.61; 95% CI = (-4.26,  2.59); df = 15.5; t = -0.52
      #Alternative hypothesis: true difference in means between group Baseline Only and group Follow-up is not equal to 0
      #Results indicate no statisical significant difference between two groups at baseline
      #This suggests people who dropped out do not have significantly different base PSYCHLOPS scores than those who stayed, ruling out non-random attrition
attrition_t_test <- t.test(pre_psych_total ~ status, data = baseline_wide)

#Comparing mean baseline PSYCHLOPS between Follow-Up and Baseline Only; appears both groups have similar mean scores at baseline 
      #Follow-up = 16.69 mean, 4.01 SD; Baseline Only = 15.86 mean, 3.08 SD
attrition_stats <- baseline_wide %>% group_by(status) %>% summarize(n = n(),
mean_psych = mean(pre_psych_total), sd_psych = sd(pre_psych_total), 
se_score = sd(pre_psych_total) / sqrt(n())) %>% gt() %>%
tab_header(title = "Baseline PSYCHLOPS Scores by Attrition Status") %>%
fmt_number(columns = c(mean_psych, sd_psych, se_score), decimals = 2) %>%
cols_label(status = "Group", mean_psych = "Mean", sd_psych = "SD", se_score = "SE") %>%
tab_footnote(footnote = paste0("Welch's Two Sample t-test: t(", 
round(attrition_t_test$parameter, 1), ") = ", round(attrition_t_test$statistic, 2), 
", p = ", round(attrition_t_test$p.value, 2)),
locations = cells_column_labels(columns = mean_psych)) %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2))
attrition_stats

#Comparing baseline distress by subgroup---------------------------------------------------------------
#Select demographic predictors
demographics <- c("age_group", "client_profile_legal_status", "client_profile_gender", 
"client_profile_marital_status", "client_profile_nationality", 
"client_profile_english_level", "client_profile_primary_language")

#Function to run ANOVA for a specific outcome across all demographics
#fun_subgroup_analysis <- function(outcome) {results <- list()
#for (demo in demographics) {
#      formula <- as.formula(paste(outcome, "~", demo))
#      model <- lm(formula, data = baseline_wide)
#      results[[demo]] <- broom::tidy(anova(model))}
#return(bind_rows(results, .id = "Variable"))}

fun_subgroup_analysis <- function(outcome) {results <- list()
for (demo in demographics) {
formula <- as.formula(paste(outcome, "~", demo))
model <- lm(formula, data = baseline_wide)
a <- anova(model)
tidy_a <- broom::tidy(a)
tidy_a$NumDF <- tidy_a$df
tidy_a$DenDF <- a["Residuals", "Df"]
results[[demo]] <- tidy_a}
bind_rows(results, .id = "Variable")}

#Run function on baseline outcomes: PSYCHLOPS and Hopkins
psych_subgroups <- fun_subgroup_analysis("pre_psych_total")
hopkins_subgroups <- fun_subgroup_analysis("hopkins_total_avg")

#Creating table comparing baseline distress by subgroup for PSYCHLOPS and Hopkins; only Gender was statistically significant for Pre-PSYCHLOPS; none for Hopkins
psychlops_subgroup_differences <- bind_rows(
      psych_subgroups %>% mutate(Instrument = "PSYCHLOPS"),
      hopkins_subgroups %>% mutate(Instrument = "HSCL-25")) %>% 
filter(term != "Residuals") %>% 
mutate(Variable = case_match(Variable,
      "client_profile_legal_status" ~ "Legal Status",
      "client_profile_gender" ~ "Gender",
      "client_profile_marital_status" ~ "Marital Status",
      "client_profile_nationality" ~ "Nationality",
      "client_profile_english_level" ~ "English Level",
      "client_profile_primary_language" ~ "Primary Language",
      "age_group" ~ "Age Group")) %>%
select(Instrument, Variable, NumDF, DenDF, statistic, p.value) %>% 
group_by(Instrument) %>% gt() %>%
tab_header(title = "Subgroup Analysis of Baseline Distress") %>%
cols_merge(columns = c(NumDF, DenDF), pattern = "{1}, {2}") %>%
fmt_number(columns = c(statistic, p.value), decimals = 3) %>%
cols_label(statistic = "F-Statistic", p.value = "p-value", NumDF = "df (num, den)") %>%
tab_options(row_group.font.weight = "bold", 
      table.font.size = px(12),
      data_row.padding = px(2), 
      row_group.padding = px(2),
      column_labels.padding = px(2),
      heading.padding = px(2)) %>%
tab_style(style = cell_fill(color = "#ffeeee"), locations = cells_row_groups()) %>%
tab_style(style = list(cell_text(weight = "bold")),
locations = cells_body(columns = p.value, rows = p.value < 0.05))
psychlops_subgroup_differences

#Baseline PSYCHLOPS total scores by gender boxplot
psychlops_gender_anova_vis <- baseline_wide %>% filter(client_profile_gender != "Unknown") %>%
ggplot(aes(x = client_profile_gender, y = pre_psych_total, fill = client_profile_gender)) +
geom_boxplot(alpha = 0.6) + geom_jitter(width = 0.1, size = 2) +
scale_fill_brewer(palette = "Set1") + labs(title = "Baseline PSYCHLOPS Scores by Gender", 
subtitle = "Significant difference found (p = .001)", 
x = NULL, y = "PSYCHLOPS Score") + theme_minimal() + theme(legend.position = "none")
psychlops_gender_anova_vis

#Mean, min, and max for Baseline PSYCHLOPS by gender
baseline_wide %>% filter(client_profile_gender != "Unknown") %>%
group_by(client_profile_gender) %>%
summarise(mean_score = mean(pre_psych_total, na.rm = TRUE),
min_score = min(pre_psych_total, na.rm = TRUE),
max_score = max(pre_psych_total, na.rm = TRUE),
sample_size = n())

invisible(View(mha_clean))
invisible(View(baseline_wide))
invisible(View(followup_wide))
invisible(View(analysis_long))


#Thematic Analysis-------------------------------------------------------------------------------------
#Do in excel, individual by individual, using PSYCHLOPS and data from intake form
#Using Braun and Clarke Framework for Thematic analysis
#Familiarization, Initial coding (assigning short labels/codes to relevant data; includes descriptive/summarizing content, in vivo (quoting), and interpretive (my analysis)), searching for themes and grouping similar codes, reviewing themes, defining/naming themes, writing up


#Do we need standard age groups? No since the data set is very small. Splitting in to more categories can result in groups too small to analyze. Keeping two groups helps maintain statistical power. 
