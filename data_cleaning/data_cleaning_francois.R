

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
    which_dev_community_33 = str_replace_all(which_dev_community_33, "\r\n", ";"), # (str_replace) is only for the first pattern in a string
    which_dev_community_33 = str_replace_all(which_dev_community_33, " ,", ";"),
    which_dev_community_33 = str_replace_all(which_dev_community_33, ", ", ";"),
    which_dev_community_33 = str_replace_all(which_dev_community_33, ",", ";"),
    which_dev_community_33 = str_replace_all(which_dev_community_33, "[:digit:]\\)", ";"), # need \\ before )
    which_dev_community_33 = str_replace_all(which_dev_community_33, "[:digit:].", ";"),
    which_dev_community_33 = str_replace_all(which_dev_community_33, " and ", ";"),
    

    which_dev_community_33 = case_when(
      which_dev_community_33 == "N/A" ~ NA_character_, # else, throw an error. all the values need to be of the same type: char (and not logical)
      TRUE ~ which_dev_community_33
    )
    
    # we will follow another strategy to clean the question 33: 
    #     -> 0. convert a maximum of delimiters to ; (done above)
    #     -> 1. select ID and which_dev_community_33, split by ";", pivot to long format
    #     -> 2. clean
    #     -> 3. unite and left_join back to main tibble
  )

cleaning_q33 <- cleaning_data %>% 
  select(ID, which_dev_community_33) %>%
  separate_rows(which_dev_community_33, sep = ";") %>%
  mutate(which_dev_community_33 = str_trim(which_dev_community_33, side = "both")) %>% # trim whitespaces generated
  drop_na() %>% # remove NAs (some generated by our splits, some real NA -> doesn't matter because left_join later)
  filter(which_dev_community_33 != "") # remove blanks generated with our replacements


# OK, ready to group the organizations!
cleaning_q33 %>% count(which_dev_community_33) %>% arrange(-n) %>% View()

# Notes:
#   GDG = Google Developer Groups GDG Accra
#   AIA = https://aiagh.net/ Artificial Intelligence Association of Ghana
#   ALC = The Andela Learning Community (ALC) is a network of technologists and tech enthusiasts across Africa dedicated to learning how to use technology to solve humanity’s problems. 
#   KNUST = Kwame Nkrumah University of Science and Technology (KNUST) is a university in Kumasi, Ashanti, Ghana

cleaned_q33 <- cleaning_q33 %>%
  mutate(
    which_dev_community_33 = case_when(
      which_dev_community_33 == "Devcongress" ~ "DevCongress",
      which_dev_community_33 == "De Congress" ~ "DevCongress",
      which_dev_community_33 == "Dev Congress" ~ "DevCongress",
      which_dev_community_33 == "Dev" ~ "DevCongress", # assumption.
      which_dev_community_33 == "dev Congress" ~ "DevCongress",
      which_dev_community_33 == "DevC" ~ "DevCongress",
      which_dev_community_33 == "devcongress" ~ "DevCongress",
      which_dev_community_33 == "devCongress" ~ "DevCongress",
      which_dev_community_33 == "DevCongress - Slack" ~ "DevCongress",
      which_dev_community_33 == "DevCongress on Slack" ~ "DevCongress",
      which_dev_community_33 == "devcongress-community.slack.com" ~ "DevCongress",
      which_dev_community_33 == "Devcongresss" ~ "DevCongress",
      
      which_dev_community_33 == "Github" ~ "GitHub",
      which_dev_community_33 == "github" ~ "GitHub",

      which_dev_community_33 == "Pyladies" ~ "PyLadies Ghana",
      which_dev_community_33 == "PyLadies" ~ "PyLadies Ghana",
      which_dev_community_33 == "Pyladies Ghana" ~ "PyLadies Ghana",
      which_dev_community_33 == "PyladiyGhana" ~ "PyLadies Ghana",
      
      which_dev_community_33 == "Stackoverflow" ~ "Stack Overflow",
      which_dev_community_33 == "StackOverflow" ~ "Stack Overflow",
      which_dev_community_33 == "Stack overflow" ~ "Stack Overflow",
      
      which_dev_community_33 == "Facebook Developer Circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook developer community" ~ "Facebook Developer Circle", # appears to be the same
      which_dev_community_33 == "Facebook Developer Community" ~ "Facebook Developer Circle", # appears to be the same
      which_dev_community_33 == "Facebook Developer circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Developer circle" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Developer Circle  from Facebook" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Face Developer Circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebok Devs Accra" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Accra" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Developer Circle accra" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook developer circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Developers" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook developers circle" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Developers Circle" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Facebook Developers Circle Accra" ~ "Facebook Developer Circle",
      which_dev_community_33 == "FB Community Circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "FB developer circles" ~ "Facebook Developer Circle",
      which_dev_community_33 == "Developer Circles from Facebook" ~ "Facebook Developer Circle",
      
      which_dev_community_33 == "GDG Accra" ~ "Google Developer Group", # i think it is safe to group them all, the one in Accra seems to be the only one and most of the respondent live near Accra anyway.
      which_dev_community_33 == "GDG" ~ "Google Developer Group",
      which_dev_community_33 == "Google Developers Group" ~ "Google Developer Group", 
      
      which_dev_community_33 == "Developers in vogue" ~ "Developers in Vogue",
      which_dev_community_33 == "DIV" ~ "Developers in Vogue",
      which_dev_community_33 == "Developer in Vogue" ~ "Developers in Vogue",
      which_dev_community_33 == "Developers on Vogue" ~ "Developers in Vogue",
      
      which_dev_community_33 == ".NET User Group Accra" ~ "Accra .NET User Group",
      which_dev_community_33 == "Accra .net" ~ "Accra .NET User Group",
      
      which_dev_community_33 == "ALC" ~ "Andela Learning Community",
      which_dev_community_33 == "Andela Learning community" ~ "Andela Learning Community",
      which_dev_community_33 == "Andela" ~ "Andela Learning Community",
      
      which_dev_community_33 == "Developer Student Club - KNUST Chapter" ~ "Developer Student Club - KNUST",
      which_dev_community_33 == "Developer Student Clubs - KNUST" ~ "Developer Student Club - KNUST",
      which_dev_community_33 == "DSC Knust" ~ "Developer Student Club - KNUST",
      which_dev_community_33 == "DSC KNUST" ~ "Developer Student Club - KNUST",
      
      which_dev_community_33 == "DSC University of Ghana" ~ "Developer Student Club - University of Ghana",
      
      which_dev_community_33 == "Django girls" ~ "Django Girls",
      which_dev_community_33 == "Django girls." ~ "Django Girls",
      which_dev_community_33 == "Django" ~ "Django Girls", # assumption, could not find other django meetups
      
      which_dev_community_33 == "Flutter" ~ "Flutter Ghana",
      which_dev_community_33 == "Flutter discord" ~ "Flutter Ghana",
      which_dev_community_33 == "FLUTTER GHANA" ~ "Flutter Ghana",
      
      which_dev_community_33 == "Freecodecamp" ~ "freeCodeCamp",
      which_dev_community_33 == "FreeCodeCamp" ~ "freeCodeCamp",
      
      which_dev_community_33 == "None" ~ NA_character_,
      which_dev_community_33 == "etc" ~ NA_character_,
      which_dev_community_33 == "etc." ~ NA_character_,
      which_dev_community_33 == "4" ~ NA_character_,
      TRUE ~ which_dev_community_33
    )
  )

cleaned_q33 %>% count(which_dev_community_33) %>% arrange(-n) %>% View()

# ==> cleaning is not finished yet ! 




