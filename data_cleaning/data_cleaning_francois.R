

# Data Cleaning of the questions between 
#   On average, how many hours per week do you work? Please enter a whole number in the box.
# to 
#   Developer communities you are a member of
# (included)

library(tidyverse)


# load data and select the questions of interest:
raw_data <- rio::import("../data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx") %>%
  select(ID, `On average, how many hours per week do you work? Please enter a whole number in the box.`:`Developer communities you are a member of` )

# rename the questions:
cleaning_data <- raw_data %>%
  select(ID = ID,
         weekly_work_hours_24 = `On average, how many hours per week do you work? Please enter a whole number in the box.`,
         overtime_work_25 = `How often do you work overtime or beyond the formal time expectation of your job?`,
         company_onboarding_process_26 = `Do you think your company has a good on-boarding process? (By on-boarding, we mean the structured process of getting you settled in to your new role at a company)`,
         improve_onboarding_27 = `How could on-boarding at your company be improved?`,
         montthly_salary_28 = `How much is your monthly salary in Ghana Cedis?`,
         job_status_29 = `Which of the following best describes your current job-seeking status?`,
         drivers_for_new_jobs_30 = `In general, what drives you to look for a new job? Select all that apply.`,
         how_learn_about_company_31 = `When job searching, how do you learn more about a company? Select all that apply.`,
         dev_community_member_32 = `Are you a member of any other online/offline developer communities?` ,
         which_dev_community_33 = `Developer communities you are a member of`
  )

# and now we check one by one each column and try to fix problems.









