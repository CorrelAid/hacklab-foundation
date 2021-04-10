# Data Cleaning Sofia

raw_data <- rio::import("../data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx")

library(tidyverse)
library(dplyr)
library(plyr)


####### Renaming variables #############
raw_data <-raw_data %>% rename(company_size_23 = 'Approximately how many people are employed by the company or organization you currently work for?',
                               prim_opsyst_14 = 'What is the primary operating system in which you work?',
                               rather_opsyst_15 = 'In which operating system would you rather work?',
                               purch_influence_16 = 'What level of influence do you, personally, have over new technology purchases at your organization?',
                               solution_research_17 = 'When buying a new tool or software, how do you discover and research available solutions? Select all that apply.',
                               highest_edu_18 = 'Which of the following best describes the highest level of formal education that you have completed?',
                               prim_study_19 = 'What was your primary field of study?',
                               edu_importance_20 = 'How important is a formal education, such as a university degree in computer science, to your career?',
                               change_edu_21 = 'From Q18, if you could go back and change your educational path (but end up in the same career), what would you change?',
                               job_satisfaction_22 = 'How satisfied are you with your current job? (If you work multiple jobs, answer for one you spend the most hours on.)')



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
                                "20,000 +" = "Over 500",
                                "1000 to .." = "Over 500",
                                "1000+" = "Over 500",
                                "Over 1000" = "Over 500",
                                "Over 99" = "Over 100",
                                "100+" = "Over 100",
                                "more than 100" = "Over 100",
                                "More than 100" = "Over 100",
                                "150" = "Over 100",
                                "185" = "Over 100",
                                "100+ Large Corporation" = "Over 100",
                                "300-500" = "Over 100",
                                "400" = "Over 100",
                                "400+" = "Over 100",
                                "250" = "Over 100",
                                "500 to 1000" = "Over 500",
                                "Above 500" = "Over 500",
                                "Over 500" = "Over 500",
                                "500" = "Over 500",
                                "About 600" = "Over 500",
                                "700" = "Over 500",
                                "20 to 99 employees" = "Below 100",
                                "The entire staff is made up of individual contractors and I’d place a number between 9-25" = "Below 20",
                                "10 to 19 employees" = "Below 20",
                                "Below 20" = "Below 20",
                                "2 to 9 employees" = "Below 10",
)

##### QUESTION: Does this categorization make sense?
#   Below 20   Over 100  Over 1000   Below 10  Below 100 Over 20000   Over 500    NA          1 
#   28         11          4         52         60          1          6          8         49

table(raw_data$company_size)                               
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
                              "I’ll Study computer science" = "I would study Computer Science",
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
                              "I would do more Math" = "More Focus on Mathematics",
                              "Avoid the norm that Medicine and Pharmacy are the best , rather re-engineered my mind to channel a keen attention on Coding and Computers. The Physics and the E-Maths combined with IT." = "More Focus on Mathematics",
                              "Do more mathematics" = "More Focus on Mathematics",
                              "would take mathematics at university" = "More Focus on Mathematics",
                              "Really really take Data Structures And Algorithms seriously." = "More Focus on Mathematics",
                              "Offer BSc Mathematics instead of Computer Science" = "More Focus on Mathematics",
                              "Offer Mathematics" = "More Focus on Mathematics",
                              "Learn Coding earlier" = "learn programming earlier",
                              "Learn programming earlier" = "learn programming earlier",
                              "Learning how to code earlier, understanding data structures and algorithms and doing lots of programming projects" = "learn programming earlier",
                              "Coding" = "learn programming earlier",
                              "Learn programming earlier" = "learn programming earlier",
                              "I would have learnt programming alongside the programme I did" = "learn programming earlier",
                              "Start coding at early stage" = "learn programming earlier",
                              "Attend bootcamps" = "learn more independently",
                              "Commitment" = "learn more independently",
                              "Go to a Boot Camp" = "learn more independently",
                              "Invested in e-learning" = "learn more independently",
                              "Learn online rather than pursuing a degree" = "learn more independently",
                              "Nothing.. I would just not go to school and just learn stuff on my as am already doing" = "learn more independently",
                              "The school I attended" = "change my school",
                              "School" = "change my school",
                              "yes" = "Yes",
                              "Yes Please" = "Yes",
                              "Maybe yes" = "Yes",
                              "Yes, any day" = "Yes",
                              "Working on more projects" = "more practical work",
                              "There will be more practical work compared to theories. And there will be more practical quizzes and assignments." = "more practical work",
                              "Working on more projects" = "more practical work",
                              "More practical learning" = "more practical work",
                              "Begin professional certifications earlier" = "more practical work",
                              "Change Master’s to Professional Certificates" = "more practical work",
                              "Computer Science degrees should be mode practical with modern employable languages" = "more practical work",
                              "Nothing. I will take more professional courses alongside my degree" = "more practical work",
                              "Master's degree" = "Master degree",
                              "Master Degree" = "Master degree",
                              "Masters" = "Master degree",
                              "Master’s degree" = "Master degree",
                              "I'd probably do Science for the elective math but I might stick with Visual Arts" = "I woud have added something else to my studies",
                              "I will just add computer science to my program of study" = "I woud have added something else to my studies",
                              "I would include some social programs, maybe a little health. Something along the lines of human Psychology" = "I woud have added something else to my studies",
                              "Include entrepreneurship program" = "I woud have added something else to my studies",
                              "Nothing but would have added up Ui and Ux design" = "I woud have added something else to my studies",
                              "Take a professional certificate course in Programming before taking Computer Science." = "I woud have added something else to my studies",
                              "Would Involve technological studies" = "I woud have added something else to my studies",
                              "Change Master's to Professional Certificates" = "I woud not go to university",
                              "Going to university. I would rather do certifications straight forward" = "I woud not go to university",
                              "I won't go to university." = "I woud not go to university",
                              "I wouldn't do university, but training in web development" = "I woud not go to university"
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
                            