# Repository of the Analytical Report of the Hacklab Ghana Developer Census 2020

This is the repository for the CorrelAid project with the Hacklab Foundation. Hacklab Ghana Developer Census 2020 is the first and most comprehensive survey of people who code in Ghana. This survey was given to 272 participants of the Hackathon 2020 hosted by the Hacklab Foundation and consisted of 41 questisons. CorrelAid created a report of this survey and here you have access to our code to replicate and extend the analyses and insights of the report.

## Summary of the [CorrelAid Project](https://correlaid.org/)
The role of  was to summarize the Census in an insightful, visual report about Ghana’s developer community to gain a better understanding of the developer’s skills and characteristics. The creation of the report included data exploration as well as data analysis and visualization.

The final report can be found [here](insert link to the final published report]

## About the [Hacklab Foundation](https://hacklabfoundation.org/)
The Hacklab Foundation is an international nonprofit organization headquartered in Ghana with a focus on preparing the youth for future digital jobs through technology education and skills development. The Hacklab Foundation achieves this through various events such as bootcamps, hackathons, mentorships, skills training and job placement.
The Foundation is known for its annual Hacklab Hackathon in Ghana that brings 1000+ developers, engineers, designers, entrepreneurs, policymakers, development partners and other stakeholders in technology to meet and discuss critical issues impacting the Global South.


## Project Organization

Run [Hacklab_Ghana_Developer_Census_2020.Rmd](https://github.com/CorrelAid/hacklab-foundation/blob/main/Hacklab_Ghana_Developer_Census_2020.Rmd) Rmarkdown to reproduce the HTML [report](https://github.com/CorrelAid/hacklab-foundation/blob/main/Hacklab_Ghana_Developer_Census_2020.html).  

To compile, the .Rmd needs:
* the [cleaned data](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/clean_all_Qs.rds), the answers to all the questions except questions 9-13 (the skills). 
* the [cleaned skills](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/skills_final.csv) (tools and technologies)
* a [network visualization](https://github.com/CorrelAid/hacklab-foundation/blob/main/Network_visualization/backbone-weighted.png) in PNG format. It was created with [Network.R](https://github.com/CorrelAid/hacklab-foundation/blob/main/Network_visualization/Network.R)
* a [theme.css](https://github.com/CorrelAid/hacklab-foundation/blob/main/css_theme/theme.css) file where the CSS of the report is defined
* a [footer.html](https://github.com/CorrelAid/hacklab-foundation/blob/main/css_theme/footer.html) where the footer of the report is defined
  * the footer needs the logos of the Hacklab Foundation and of Correlaid, which are in the same folder


### Data Cleaning
The [raw anonymized data](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx) was cleaned step by step with the scripts in [data_cleaning](https://github.com/CorrelAid/hacklab-foundation/tree/main/data_cleaning), joined together with [Merge_clean_data.R](https://github.com/CorrelAid/hacklab-foundation/blob/main/data_cleaning/Merge_clean_data.R) and the output: [clean_all_Qs.rds](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/clean_all_Qs.rds) is the dataset used for the analyses and report. The tools and technologies are saved separately in [skills_final.csv](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/skills_final.csv) ([Data_Cleaning_ST.R
](https://github.com/CorrelAid/hacklab-foundation/blob/main/data_cleaning/Data_Cleaning_ST.R) is the script used for this, note that we re-attributed all the answers to question 13 to questions 9-12).  




