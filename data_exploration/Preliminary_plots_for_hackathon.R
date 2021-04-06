
# Preliminary plots for the hackathon
# ! made with "un-clean" data !

library(tidyverse)
library(extrafont) # to use the Gotham font family (https://freefontsfamily.com/gotham-font-family/)

# load data:
survey <- rio::import("../data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx")


HL_colors = c("#610b70","#88b101","#eb1c96","#e98403","#454545")
HL_font = "Open Sans" # I dunno why I can't make Gotham work??? "Gotham Book"
main_fontsize = 7 # must be large if used primarily on slides 

HL_theme <- function(){
  theme_minimal() + 
    theme(
      # legend.text = element_text(family = HL_font, size = 15),
      # legend.title = element_text(family = HL_font, size = 15, face = "bold"),
      plot.title = element_text(family = HL_font, size = main_fontsize + 2, color = "black"),
      # plot.subtitle = element_text(family = HL_font, size = main_fontsize),
      axis.title = element_text(family = HL_font, size = main_fontsize, color = "black"),
      axis.text = element_text(family = HL_font, size = main_fontsize, color = "black"),
      # plot.caption = element_text(hjust = 0, family = my_font, 
      #                             size = main_fontsize, face= "italic"), #Default is hjust=1 / 0
      plot.title.position = "plot",  # left-align title
      # plot.caption.position =  "plot", 
      panel.grid.minor.x = element_blank()  # remove useless minor grid
      # plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm")
    )
}


# HL received 272 answers!

#   Here a few statistics about the respondents.


# --- Q1
survey %>% 
  mutate(Q1 = `Which of the following options best describes you today? Here, by "developer" we mean someone who writes code.`) %>%
  mutate(
    Q1 = case_when(
      Q1 == "A Developer" ~ "I am a developer by profession",
      Q1 == "Currently in Transition to Dveloper" ~ "I am a student who is learning to code",
      Q1 == "I am a business admin student learning how to code both as a hobby and towards a career." ~ "I am a student who is learning to code",
      Q1 == "I am studying to occupy a data scientist role" ~ "I am a student who is learning to code",
      Q1 == "I want to keanr how to cods" ~ "I am a student who is learning to code",
      Q1 == "I work closely with Developers" ~ "None of these",
      Q1 == "I am a developer and a student" ~ "None of these",
      TRUE ~ Q1
    )
  ) %>%
  count(Q1) %>%
  ggplot(aes(x = fct_reorder(Q1,n), y = n)) + 
  geom_col(fill = HL_colors[1]) + 
  geom_text(
    aes(label = n, y = n + 5),
    size = 2
  ) +
  HL_theme() + 
  coord_flip() +
  labs(title = "What is your current stage in your developer's journey?") +
  xlab("") +
  ylab("")

ggsave(filename = "Preliminary_plots_for_hackathon/Q1.png", 
       width = 200,
       height = 80, 
       units = "mm",
       dpi = 400)  


# --- Q20 
survey %>% 
  mutate(Q20 = `How important is a formal education, such as a university degree in computer science, to your career?`) %>%
  count(Q20) %>%
  drop_na() %>%
  mutate(Q20 = fct_relevel(Q20, 'Not at all important/not necessary', "Somewhat important", 
                           "Fairly important",'Very important', "Critically important")) %>%
  ggplot(aes(x = Q20, y = n)) + 
  geom_col(fill = HL_colors[1]) + 
  geom_text(
    aes(label = n, y = n + 2),
    size = 2
  ) +
  HL_theme() + 
  coord_flip() +
  labs(title = "How important is a formal education to your career?") +
  xlab("") +
  ylab("")

ggsave(filename = "Preliminary_plots_for_hackathon/Q20.png", 
       width = 150,
       height = 80, 
       units = "mm",
       dpi = 400)  


# --- Q24

survey %>% 
  mutate(Q24 = as.numeric(`On average, how many hours per week do you work? Please enter a whole number in the box.`)) %>%
  select(Q24) %>%
  drop_na() %>%
  ggplot() + 
  geom_histogram(aes(Q24), fill = HL_colors[1], bins = 13) +
  HL_theme() + 
  labs(title = "On average, how many hours per week do you work?") +
  xlab("Hours per week") +
  ylab("Count") +
  scale_x_continuous(breaks=seq(0,120, 10))

ggsave(filename = "Preliminary_plots_for_hackathon/Q24.png", 
       width = 150,
       height = 80, 
       units = "mm",
       dpi = 400)  


# --- Q43

survey %>% 
  mutate(Q43 = `How easy or difficult was this survey to complete?`) %>%
  count(Q43) %>%
  drop_na() %>%
  mutate(Q43 = fct_relevel(Q43, 'Difficult', "Neither easy nor difficult", "Easy")) %>%
  ggplot(aes(x = Q43, y = n)) + 
  geom_col(fill = HL_colors[1]) + 
  geom_text(
    aes(label = n, y = n + 4),
    size = 2
  ) +
  HL_theme() + 
  coord_flip() +
  labs(title = "How easy or difficult was this survey to complete?") +
  xlab("") +
  ylab("")

ggsave(filename = "Preliminary_plots_for_hackathon/Q43.png", 
       width = 150,
       height = 60, 
       units = "mm",
       dpi = 400)   

# --- Q28

survey %>% 
  mutate(Q28 = `How much is your monthly salary in Ghana Cedis?`) %>%
  count(Q28) %>%
  drop_na() %>%
  mutate(
    Q28 = fct_relevel(Q28, 
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
    Q28 = fct_recode(Q28, "GHS 3,500 - GHS 4,000" = "GHS 3500 - GHS 4,000") # correct issue in the levels of the factor
  ) %>%
  ggplot(aes(x = Q28, y = n)) + 
  geom_col(fill = HL_colors[1]) + 
  geom_text(
    aes(label = n, y = n + 2),
    size = 2
  ) +
  HL_theme() + 
  coord_flip() +
  labs(title = "How much is your monthly salary in Ghana Cedis?") +
  xlab("") +
  ylab("")

ggsave(filename = "Preliminary_plots_for_hackathon/Q28.png", 
       width = 150,
       height = 100, 
       units = "mm",
       dpi = 400)   


















