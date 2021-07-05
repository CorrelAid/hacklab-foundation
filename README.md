# hacklab-foundation

Repository for the CorrelAid project with the Hacklab Foundation

## Summary the Project
* insert link to the final published report
* insert link to Correlaid

## About the Hacklab Foundation
* insert linnk to Hacklab Foundation's website


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




