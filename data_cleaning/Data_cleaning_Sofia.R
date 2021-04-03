# Data Cleaning Sofia

raw_data <- rio::import("../data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx")

library(tidyverse)
library(dplyr)
library(plyr)


####### Renaming variables #############
raw_data <-raw_data %>% rename(company_size = 'Approximately how many people are employed by the company or organization you currently work for?',
                                prim_opsyst = 'What is the primary operating system in which you work?',
                                rather_opsyst = 'In which operating system would you rather work?',
                                purch_influence = 'What level of influence do you, personally, have over new technology purchases at your organization?',
                                solution_research = 'When buying a new tool or software, how do you discover and research available solutions? Select all that apply.',
                                highest_edu = 'Which of the following best describes the highest level of formal education that you have completed?',
                                prim_study = 'What was your primary field of study?',
                                edu_importance = 'How important is a formal education, such as a university degree in computer science, to your career?',
                                change_edu = 'From Q18, if you could go back and change your educational path (but end up in the same career), what would you change?',
                                job_satisfaction = 'How satisfied are you with your current job? (If you work multiple jobs, answer for one you spend the most hours on.)')
                   


#### Converting variables into factors #######
raw_data$prim_study<-as.factor(raw_data$prim_study)
raw_data$highest_edu<-as.factor(raw_data$highest_edu)
raw_data$company_size<-as.factor(raw_data$company_size)
raw_data$change_edu<-as.factor(raw_data$change_edu)


############# Recoding factor levels of variables ############


### Primary Study

raw_data$prim_study <- recode(raw_data$prim_study, 
                                         "Agricultural science" = "Agricultural_Science", 
                                        "Agricultural Science" ="Agricultural_Science" ,
                                        "Agricultural Engineering" ="Agricultural_Science" ,
                                        "General Science" = "General_Science" ,
                                        "General science" = "General_Science",
                                        "Electrical Engineering" = "Electrical_Engineering",
                                        "Electrical and Electronics Engineering" = "Electrical_Engineering",
                                        "Electrical/Electronic Engineering" = "Electrical_Engineering",
                                        "physics" = "Physics",
                                        "Physical Sciences (Chemistry)" = "Physics",
                                        "Computer science. computer engineering or software engineering" = "Computer_Science_Engineering",
                                        "Engineering and technology anything that has to do with technology not only in the computer science sector" = "Computer_Science_Engineering",
                                        "Biomedical Engineering" = "Other_Engineering",
                                        "Anything that has to do with engineering and basically technology not only in the field of computer science." = "Other_Engineering",
                                        "Engineering" = "Other_Engineering",
                                        "Food Process Engineering" = "Other_Engineering",
                                        "Mechanical Engineering" = "Other_Engineering",
                                        "Petroleum engineering" = "Other_Engineering",
                                        "Geometic Engineering" = "Other_Engineering",
                                        "Geodetic Eng" = "Other_Engineering",
                                        "Building Technology" = "Other_Engineering",  
                                        "General science in senior high school but I'm currently studying mechanical engineering in the University." = "Other_Engineering",
                                        "Information Technology" = "Information_Technology",
                                        "ICT" =  "Information_Technology",
                                    "Information studies, information technology or system administration" = "Information_Technology",
                                    "Health science (such as nursing, pharmacy, etc.)" = "Health_Science",
                                    "Business discipline (such as accounting, finance, marketing, etc.)" = "Business",
                                    "Currently studying Social science (Economics Education) at University of Education, Winneba" = "Social_Science",
                                    "Social science (such as psychology, political science, etc.)" = "Social_Science",
                                    "General Arts( French, Geography, Economics, Elective maths)" = "General_Arts",
                                    "Chemistry, Biology, Physics and Elective Maths" = "Chemistry",
                                    "Fine Arts" = "Visual_Arts",
                                    "Visual Arts" = "Visual_Arts",
                              "Graphic Design" = "Graphic_Design",
                              "Web development or web design" = "Web_Development_Web_Design",
                              "Mathematics or statistics" = "Mathematics_Statistics"
                                      )
table(raw_data$prim_study)



### Highest Education

raw_data$highest_edu <- recode(raw_data$highest_edu, 
                                      "Bachelor's degree" = "Bachelor", 
                                      "Master's degree" = "Master",
                                      "Higher National Diploma" = "Higher_National_Diploma",
                                      "Advanced National Diploma" = "Higher_National_Diploma",
                                      "Higher National Diploma (HND)" = "Higher_National_Diploma",
                                      "HND" = "Higher_National_Diploma",
                                      "Diploma" = "Higher_National_Diploma",
                                      "Will finish bachelor's degree in 2021" = "Secondary_High_School",
                                      "Still in the University" = "Secondary_High_School",
                                      "Secondary school" = "Secondary_High_School",
                                      "Diploma in Basic Education" = "Teacher_Diploma",
                                      "Still in shs" = "Basic_Education",
                                      "Professional degree" = "Professional_Diploma"
                                      )

table(raw_data$highest_edu)

### Company Size

raw_data$company_size <- recode(raw_data$company_size, 
                                "Can't really tell" = "NA", 
                                "Just me - I am a freelancer, sole proprietor, etc." = "1",
                                "Not employed" = "NA",
                                "Not employed mainly working on side projects" = "NA",
                                "Am not employed" = "NA",
                                "Student" = "NA",
                                "More Educational" = "NA",
                                "20,000 +" = "Over_20000",
                                "1000 to .." = "Over_1000",
                                "1000+" = "Over_1000",
                                "Over 1000" = "Over_1000",
                                "Over 99" = "Over_100",
                                "100+" = "Over_100",
                                "more than 100" = "Over_100",
                                "More than 100" = "Over_100",
                                "150" = "Over_100",
                                "185" = "Over_100",
                                "100+ Large Corporation" = "Over_100",
                                "300-500" = "Over_100",
                                "400" = "Over_100",
                                "400+" = "Over_100",
                                "250" = "Over_100",
                                "500 to 1000" = "Over_500",
                                "Above 500" = "Over_500",
                                "Over 500" = "Over_500",
                                "500" = "Over_500",
                                "About 600" = "Over_500",
                                "700" = "Over_500",
                                "20 to 99 employees" = "Below_100",
                                "The entire staff is made up of individual contractors and I'd place a number between 9-25" = "Below 20",
                                "10 to 19 employees" = "Below_20",
                                "Below 20" = "Below_20",
                                "2 to 9 employees" = "Below_10",
)

##### QUESTION: Does this categorization make sense?
#   Below_20   Over_100  Over_1000   Below_10  Below_100 Over_20000   Over_500    NA          1 
#   28         11          4         52         60          1          6          8         49
                               
table(raw_data$company_size)                               
######
##### QUESTION: Where should I put "Diploma" and "Still in University", does Bachelor come after SHS?, does SHS come after Basic Education? Is Secondary schoo = SHS?
######



#### Changing Education in hindsight

raw_data$change_edu <- recode(raw_data$change_edu, 
                              "I won't change" = "Nothing",
                              "I won't change a thing" = "Nothing",
                              "I'm good" = "Nothing", 
                              "I would not change anything" = "Nothing", 
                              "no" = "Nothing", 
                              "No" = "Nothing", 
                              "NO" = "Nothing", 
                              "None" = "Nothing", 
                              "nothing" = "Nothing", 
                              "Nothing at all" = "Nothing",
                              "Nothing much" = "Nothing",
                              "Nothing to change" = "Nothing",
                              "Nothing." = "Nothing",
                              "Nothing, will do same." = "Nothing",
                              "N/A" = "Nothing",
                              "I will still prefer the same educational career" = "Nothing",
                              "Still will pursue computer science" = "Nothing",
                              "I'll Study computer science" = "I would study Computer Science",
                              "Probably study computer science/ Computer Eng" = "I would study Computer Science",
                              "Study Computer Science" = "I would study Computer Science",
                              "Would have studied CS/IT" = "I would study Computer Science",
                              "I would rather opt for a computer science discipline" = "I would study Computer Science",
                              "I would probably do computer engineering or science" = "I would study Computer Science",
                              "I will just add computer science to my program of study." = "I would study Computer Science",
                              "Change my field of study to computer science" = "I would study Computer Science",
                              "computer science" = "I would study Computer Science",
                              "CS" = "I would study Computer Science",
                              "Computer Science" = "I would study Computer Science",
                              "I would do more Math" = "More_Focus_on_Mathematics",
                              "Avoid the norm that Medicine and Pharmacy are the best , rather re-engineered my mind to channel a keen attention on Coding and Computers. The Physics and the E-Maths combined with IT." = "More_Focus_on_Mathematics",
                              "Do more mathematics" = "More_Focus_on_Mathematics",
                              "would take mathematics at university" = "More_Focus_on_Mathematics",
                              "Really really take Data Structures And Algorithms seriously." = "More_Focus_on_Mathematics",
                              "Offer BSc Mathematics instead of Computer Science" = "More_Focus_on_Mathematics",
                              "Offer Mathematics" = "More_Focus_on_Mathematics",
                              "Learn Coding earlier" = "learn_programming_earlier",
                              "Learn programming earlier" = "learn_programming_earlier",
                              "Learning how to code earlier, understanding data structures and algorithms and doing lots of programming projects" = "learn_programming_earlier",
                              "Coding" = "learn_programming_earlier",
                              "Learn programming earlier" = "learn_programming_earlier",
                              "I would have learnt programming alongside the programme I did" = "learn_programming_earlier",
                              "Start coding at early stage" = "learn_programming_earlier",
                              "Attend bootcamps" = "learn_more_independently",
                              "Commitment" = "learn_more_independently",
                              "Go to a Boot Camp" = "learn_more_independently",
                              "Invested in e-learning" = "learn_more_independently",
                              "Learn online rather than pursuing a degree" = "learn_more_independently",
                              "Nothing.. I would just not go to school and just learn stuff on my as am already doing" = "learn_more_independently",
                              "The school I attended" = "change_my_school",
                              "School" = "change_my_school",
                              "yes" = "Yes",
                              "Yes Please" = "Yes",
                              "Maybe yes" = "Yes",
                              "Yes, any day" = "Yes",
                              "Working on more projects" = "more_practical_work",
                              "There will be more practical work compared to theories. And there will be more practical quizzes and assignments." = "more_practical_work",
                              "Working on more projects" = "more_practical_work",
                              "More practical learning" = "more_practical_work",
                              "Begin professional certifications earlier" = "more_practical_work",
                              "Change Master's to Professional Certificates" = "more_practical_work",
                              "Computer Science degrees should be mode practical with modern employable languages" = "more_practical_work",
                              "Nothing. I will take more professional courses alongside my degree" = "more_practical_work",
                              "Master's degree" = "Master_degree",
                              "Master Degree" = "Master_degree",
                              "Masters" = "Master_degree",
                              "Master's degree" = "Master_degree",
                              "I'd probably do Science for the elective math but I might stick with Visual Arts" = "I_woud_have_added_something_else_to_my_studies",
                              "I will just add computer science to my program of study" = "I_woud_have_added_something_else_to_my_studies",
                              "I would include some social programs, maybe a little health. Something along the lines of human Psychology" = "I_woud_have_added_something_else_to_my_studies",
                              "Include entrepreneurship program" = "I_woud_have_added_something_else_to_my_studies",
                              "Nothing but would have added up Ui and Ux design" = "I_woud_have_added_something_else_to_my_studies",
                              "Take a professional certificate course in Programming before taking Computer Science." = "I_woud_have_added_something_else_to_my_studies",
                              "Would Involve technological studies" = "I_woud_have_added_something_else_to_my_studies",
                              "Change Master's to Professional Certificates" = "I_woud_not_go_to_university",
                              "Going to university. I would rather do certifications straight forward" = "I_woud_not_go_to_university",
                              "I won't go to university." = "I_woud_not_go_to_university",
                              "I wouldn't do university, but training in web development" = "I_woud_not_go_to_university"
                              )


############## Splitting Strings of Solution_Research ################## 
s <- strsplit(raw_data$solution_research, split = ";")
clean_long_data<-data.frame(ID = rep(c(raw_data$ID, sapply(s, length)), solution_research = unlist(s))
View(clean_long_data)

clean_data <- raw_data %>% select(prim_study, highest_edu, company_size, change_edu, ID)











############### Failed attempts to merge both data sets (clean_data and clean_long_data) ######################

final2<-rbind.fill(long_dat, clean_data)

final<-merge(long_dat, clean_data, by="ID", all.x=TRUE, all.y=FALSE)
View(final)                              

FINAL<-complete(clean_data, ID, highest_edu) %>%
  inner_join(., long_dat)

library(gtools)
FINAL1<-smartbind(long_dat, clean_data)
View(FINAL1)
