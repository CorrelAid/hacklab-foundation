
# Data Cleaning of the questions 34 to 44

library(tidyverse)

# load data and select the questions of interest:
raw_data <- rio::import("../data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx") %>%
  select(ID, `What is your age (in years)? If you prefer not to answer, you may leave this question blank.` :`Any final thoughts?` )

# rename the questions:
cleaning_data <- raw_data %>%
  select(ID = ID,
         age_optional_34 = `What is your age (in years)? If you prefer not to answer, you may leave this question blank.`,
         gender_35 = `Which of the following do you currently identify as? Please select all that apply. If you prefer not to answer, you may leave this question blank.`,
         transgender_36 = `Do you identify as transgender?`,
         sexual_orientation_37 = `Which of the following do you currently identify as? Please select all that apply. If you prefer not to answer, you may leave this question blank.2`,
         ethnicity_38 = `Which of the following do you identify as? Please check all that apply. If you prefer not to answer, you may leave this question blank.`,
         disabilitys_39 = `Which of the following describe you, if any? This information will be kept private. If you prefer not to answer, you may leave this question blank.` ,
         mental_illness_40 = `Which of the following describe you, if any? This information will be kept private. If you prefer not to answer, you may leave this question blank.2`,
         dependents_41 = `Do you have any dependents (e.g., children, elders, or others) that you care for?`,
         survey_length_42 = `How do you feel about the length of this survey?` ,
         survey_difficulty_43 = `How easy or difficult was this survey to complete?`,
         final_thoughts_44 = `Any final thoughts?`
  )

# and now we check one by one each column and try to fix problems.
# cleaning_data <- cleaning_data %>%
cleaned_data <- cleaning_data %>%
  mutate(
    age_optional_34 = case_when(
      age_optional_34 == "20 years" ~ "20",
      age_optional_34 == "21 years" ~ "21",
      age_optional_34 == "28yrs" ~ "28",
      TRUE ~ age_optional_34
    ), # first handle special cases
    age_optional_34 = as.integer(age_optional_34), # then convert to numeric
    gender_35 = fct_relevel(gender_35, "Woman", "Man"), # convert to factor and order levels
    transgender_36 = fct_relevel(transgender_36, "Yes", "No"),  # convert to factor and order levels
    sexual_orientation_37 = case_when(
      sexual_orientation_37 == "Natural" ~ NA_character_, # we cannot know what this means...
      TRUE ~ sexual_orientation_37
    ),
    sexual_orientation_37 = as_factor(sexual_orientation_37), # convert to factor.
    ethnicity_38 = case_when(
      ethnicity_38 == "African" ~ "Black or of African descent", # suppose that it's the same...
      TRUE ~ ethnicity_38
    ),
    ethnicity_38 = as_factor(ethnicity_38), # convert to factor.
    disabilitys_39 = case_when(
      disabilitys_39 == "I prefer not to answer;None of the above;" ~ "I prefer not to answer", # supposition.
      TRUE ~ disabilitys_39
    ),
    disabilitys_39 = as_factor(disabilitys_39), # convert to factor.
    mental_illness_40 = case_when(
      mental_illness_40 == "I have a mood or emotional disorder (e.g. depression, bipolar disorder);None of the above;" ~ "I have a mood or emotional disorder (e.g. depression, bipolar disorder);", 
      mental_illness_40 == "N/A;" ~ NA_character_,
      mental_illness_40 == "None of the above;I sleep a lot;" ~ "None of the above;",
      TRUE ~ mental_illness_40
    ), # qw will have to rearrange, split, in function of the viz we decide to do later
    dependents_41 = fct_relevel(dependents_41, "Yes", "No", "I prefer not to say"),  # convert to factor and order levels
    survey_length_42 = fct_relevel(survey_length_42, "Appropriate in length", "Too long"),  # convert to factor and order levels
    survey_difficulty_43 = fct_relevel(survey_difficulty_43, "Easy", "Neither easy nor difficult", "Difficult"),  # convert to factor and order levels
    final_thoughts_44 = final_thoughts_44 # we see how we handle this later...
  )

# export the cleaned data;
rio::export(cleaned_data, "../data/clean/clean_Q34-Q44.csv")
rio::export(cleaned_data, "../data/clean/clean_Q34-Q44.rds")



