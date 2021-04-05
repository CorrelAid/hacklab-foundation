

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
         monthly_salary_28 = `How much is your monthly salary in Ghana Cedis?`,
         job_status_29 = `Which of the following best describes your current job-seeking status?`,
         drivers_for_new_jobs_30 = `In general, what drives you to look for a new job? Select all that apply.`,
         how_learn_about_company_31 = `When job searching, how do you learn more about a company? Select all that apply.`,
         dev_community_member_32 = `Are you a member of any other online/offline developer communities?` ,
         which_dev_community_33 = `Developer communities you are a member of`
  )

# and now we check one by one each column and try to fix problems.
cleaning_data <- cleaning_data %>%
  mutate(
    weekly_work_hours_24 = as.integer(weekly_work_hours_24), # note: one 37.5 is becoming a 37
    overtime_work_25 = fct_relevel(overtime_work_25, 
                                   "Never", 
                                   "Rarely: 1-2 days per year or less",
                                   "Occasionally: 1-2 days per quarter but less than monthly",
                                   "Sometimes: 1-2 days per month but less than weekly",
                                   "Often: 1-2 days per week",
                                   "Frequently: 3 or more days per week"
    ), # convert to factor and order levels
    company_onboarding_process_26 = fct_relevel(company_onboarding_process_26, "Yes", "No"), # convert to factor
    improve_onboarding_27 = improve_onboarding_27, # manual work will be needed once we decide if using this or not
    monthly_salary_28 = fct_relevel(monthly_salary_28, 
                                    "Less than GHS 1,500", 
                                    "GHS 1,500 - GHS 2,000",
                                    "GHS 2,000 - GHS 2,500",
                                    "GHS 2,500 - GHS 3,000",
                                    "GHS 3,000 - GHS 3,500",
                                    "GHS 3500 - GHS 4,000",
                                    "GHS 4,000 - GHS 5,000",
                                    "GHS 5,000 - GHS 6,000",
                                    "GHS 6,000 - GHS 8,000",
                                    "GHS 8,000 - GHS 10,000",
                                    "GHS 10,000 - GHS 15,000",
                                    "GHS 15,000 - GHS 20,000",
                                    "GHS 20,000 - GHS 25,000",
                                    "Greater than GHS 25,000"
    ), # convert to factor and order levels
    monthly_salary_28 = fct_recode(monthly_salary_28, "GHS 3,500 - GHS 4,000" = "GHS 3500 - GHS 4,000"), # correct issue in the levels of the factor
    job_status_29 = job_status_29, # clean and no NAs
    drivers_for_new_jobs_30 = drivers_for_new_jobs_30, # We will have to split and count when plotting.
    how_learn_about_company_31 = how_learn_about_company_31, # We will have to split and count when plotting.
    dev_community_member_32 = fct_relevel(dev_community_member_32, "Yes", "No"), # convert to factor
    
    # a lot to clean on Q 33 if we want to be able to group and count them:
    which_dev_community_33 = str_replace(which_dev_community_33, "\r\n", ";"),
    which_dev_community_33 = str_replace(which_dev_community_33, " ,", ";"),
    which_dev_community_33 = str_replace(which_dev_community_33, ", ", ";"),
    which_dev_community_33 = str_replace(which_dev_community_33, ",", ";"),
    
    which_dev_community_33 = case_when(
      which_dev_community_33 == "N/A" ~ NA_character_, # else, throw an error. all the values need to be of the same type: char (and not logical)
      TRUE ~ which_dev_community_33
    )
    
  ) 








