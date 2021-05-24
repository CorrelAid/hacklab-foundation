
# Data:
library(ggplot2)
library(xaringanExtra) # to be able to have tabs. To install: install 'devtools' package, then run: devtools::install_github("gadenbuie/xaringanExtra")
xaringanExtra::use_panelset() # allow the creation of tabs in Distill (see https://github.com/rstudio/distill/issues/11)

library(tidyverse) # includes dplyr, tidyr, ggplot2, rio, stringr, ...

library(leaflet) # for the interactive map
library(tmaptools) # for geocoding

Qs <- rio::import("data/clean/clean_all_Qs.rds") %>%
  # reodering certain factors for the plots: (use barplot_ordered() for plotting)
  mutate(
    purch_influence_16 = fct_relevel(purch_influence_16,
                                     "I have little or no influence",
                                     "I have some influence",
                                     "I have a great deal of influence"),
    job_satisfaction_22 = fct_relevel(job_satisfaction_22,
                                      "Neither satisfied nor dissatisfied",
                                      "Slightly dissatisfied",
                                      "Very dissatisfied",
                                      "Slightly satisfied",
                                      "Very satisfied")
  )

skills <- rio::import("data/clean/skills_final.csv") %>% 
  select(-V1) %>%  # artifact due to a save as .csv to remove (at least on a Mac?)
  mutate_all(na_if,"") # the column 'level' contains blanks ("") instead of NAs -> transform them to NAs.
# we add characteristics of the respondents to the skills, 
# so that we can differentiate their popularity by occupation or gender:
skills_with_characteristics <- skills %>% left_join(select(Qs, ID, profession_1, gender_35), by = c("id" = "ID"))

number_of_respondents = dim(Qs)[1] # number of respondents (272) used to compute percentages.
number_of_prof_dev = dim(Qs %>% filter(profession_1 == "I am a developer by profession"))[1]
number_of_students = dim(Qs %>% filter(profession_1 == "I am a student who is learning to code"))[1]


```

```{r}

# Themes and Functions:

HL_colors = c("#610b70","#88b101","#eb1c96","#e98403","#454545")
HL_font = "sans" # !!! Should change and use Gotham !!!
main_fontsize = 7 

barplot_theme <- function(){
  # the definition of a custom ggplot theme for our barplots.
  theme_minimal() + 
    theme(
      plot.title = element_text(family = HL_font, size = main_fontsize + 2, color = "black"),
      # plot.subtitle = element_text(family = HL_font, size = main_fontsize),
      axis.title = element_text(family = HL_font, size = main_fontsize, color = "black"),
      axis.text = element_text(family = HL_font, size = main_fontsize, color = "black"),
      # plot.caption = element_text(hjust = 0, family = my_font, 
      #                             size = main_fontsize, face= "italic"), #Default is hjust=1 / 0
      plot.title.position = "plot",  # left-align title
      # plot.caption.position =  "plot", 
      panel.grid.minor.x = element_blank(),  # remove useless minor grid
      panel.grid.major.y = element_blank(),  # remove useless major grid in bar charts
      panel.grid.major.x = element_blank(),  # remove useless major grid in bar charts
      axis.title.x = element_blank(), # we do not want an y axis for the barplot, we have labels already
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
      # plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm")
    )
}

basic_barplot <- function(my_df, a_factor, its_values, labels, title){
  # a function to plot a standard barplot
  # uses the barplot_theme defined above
  # needs an (ordered) factor as input, and its values
  # returns a ggplot
  current_plot <- ggplot(data = my_df, 
                         aes(x = fct_reorder({{a_factor}}, {{its_values}}), 
                             y = {{its_values}})) + 
    geom_col(fill = HL_colors[1]) + 
    geom_text(
      aes(label = {{labels}}, y = {{its_values}} + (0.06*max({{its_values}}))),
      size = 2
    ) +
    scale_y_continuous(labels = scales::percent) +
    barplot_theme() + 
    coord_flip() +
    labs(title = stringr::str_wrap(title,100)) + # automated text wrap in title
    xlab("") +
    ylab("")
  return(current_plot)
}

#Function for barplot that does not override order
barplot_ordered <- function(my_df, a_factor, its_values, labels, title){
  # a function to plot a standard barplot
  # uses the barplot_theme defined above
  # needs an (ordered) factor as input, and its values
  # returns a ggplot
  current_plot <- ggplot(data = my_df, 
                         aes(x = {{a_factor}},
                             y = {{its_values}})) + 
    geom_col(fill = HL_colors[1]) + 
    geom_text(
      aes(label = {{labels}}, y = {{its_values}} + (0.06*max({{its_values}}))),
      size = 2
    ) +
    scale_y_continuous(labels = scales::percent) +
    barplot_theme() + 
    coord_flip() +
    labs(title = stringr::str_wrap(title,100)) + # automated text wrap in title
    xlab("") +
    ylab("")
  return(current_plot)
}

compute_perc <- function(my_df, a_factor){
  # a small function to count a factor and compute the percentages
  # also return the percentages as char for labelling.
  # USE THIS FUNCTION IF THE SUM OF THE ANSWERS IS 272! (including NAs) -> not working if several possible choices.
  my_df %>%
    count({{a_factor}}) %>%
    drop_na() %>% # always in percentage of the respondents to the question.
    mutate(
      perc = n/sum(n),
      perc_label = paste0(as.character(round(perc*100,1)), "%")
    ) %>% return()
  
}

compute_perc_popular_skills <- function(the_skills, skillset, number_of_resp){
  # USE THIS FUNCTION for the popular skills (last year + both)
  # NOTE: we use all respondents, not only those who responded. -> some respondents did not enter any skills. -> not optimal, but annoying to correct?
  the_skills %>%
    filter(tool %in% skillset) %>% # select only the languages from all the tools
    filter(level == "Worked with in PAST year" | level == "Both") %>%
    count(tool) %>%
    mutate(
      perc = n/number_of_resp, 
      perc_label = paste0(as.character(round(perc*100,1)), "%")
    ) %>% 
    arrange(-perc) %>%  
    return()
}

barplot_ordered(my_df = compute_perc(Qs, job_satisfaction_22 ), 
              a_factor = job_satisfaction_22 , its_values = perc, labels = perc_label, 
              title = "Q22. How satisfied are you with your current job?")

basic_barplot(my_df = compute_perc(Qs %>% filter(profession_1 == "I am a developer by profession"), highest_edu_18), 
              a_factor = highest_edu_18, its_values = perc, labels = perc_label, 
              title = "")

table(Qs %>% filter(profession_1 == "I am a developer by profession"))
table(Qs$profession_1)

table(Qs$gender_35)

names(Qs)
class(Qs$monthly_salary_28)
class(Qs$job_satisfaction_22)
library(ggplot2)


# Heat Map
runningcounts.df <- as.data.frame(table(Qs$job_satisfaction_22, Qs$overtime_work_25), na.rm=TRUE)
ggplot(runningcounts.df, aes(Var1, Var2)) +
  geom_tile(aes(fill = Freq), colour = "black") +
  scale_fill_gradient(low = "white", high = "blue")

table(Qs$job_satisfaction_22, Qs$monthly_salary_28)

# Barplot
ggplot(runningcounts.df, aes(factor(Var2))) +
  geom_bar(aes(y = Freq, alpha = rev(Var1)),
           stat = "identity", position = "dodge",
           color = "#209f1b", fill = "#35665C") +
  geom_text(aes(y = -5, label = Freq), color = "red") +
  facet_grid(Var2 ~ Var1, switch = "both") +
  labs(title = "frequency",
       y = "Satsifaction",
       x = "Overtime") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(size = 12),
        legend.position = "none")

data<-structure(list(improvement = c("none", "moderate", "substantial", 
                               "none", "moderate", "substantial", "none", "moderate", "substantial", 
                               "none", "moderate", "substantial", "none", "moderate", "substantial", 
                               "none", "moderate", "substantial"), treatment = c(0L, 0L, 0L, 
                                                                                 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L), 
               baseline = c("mild", "mild", "mild", "mild", "mild", "mild", 
                            "moderate", "moderate", "moderate", "moderate", "moderate", 
                            "moderate", "severe", "severe", "severe", "severe", "severe", 
                            "severe"), frequency = c(5L, 41L, 4L, 19L, 19L, 12L, 19L, 
                                                     24L, 7L, 20L, 14L, 16L, 7L, 21L, 22L, 12L, 15L, 23L)), .Names = c("improvement", 
                                                                                                                       "treatment", "baseline", "frequency"), row.names = c(NA, -18L
                                                                                                                       ), class = "data.frame")
