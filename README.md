# Repository of the Analytical Report of the Hacklab Ghana Developer Census 2020

This repository contains all the code and data of the [Hacklab Ghana Developer Census 2020](https://correlaid.github.io/hacklab-foundation/Developer-Census-2020-Report.html). **Hacklab Ghana Developer Census 2020 is the first and most comprehensive survey of people who code in Ghana**. The report was developed by CorrelAid in a Data4Good project in collaboration with the Hacklab Foundation.  

272 participants responded to the 41 questions of the **survey conducted by Hacklab Research** between November and December 2020. CorrelAid volunteers created a report of the survey results and here you have access to the code to replicate and extend the analyses and insights of the report. The anonymized results of the survey are [available for download](https://github.com/Hacklab-Foundation/Developer-Census-2020) under the [Open Database License (ODbL)](https://opendatacommons.org/licenses/odbl/1-0/).

----- 

## Summary of the [CorrelAid Project](https://correlaid.org/)
The role of CorrelAid was to summarize the Census in an insightful and visual report about Ghana’s developer community to gain a better understanding of the developers’ skills and characteristics. The creation of the report included data exploration as well as data analysis and visualization.

The **final report can be found [here](https://hacklabfoundation.org/Developer-Census-2020-Report.html)**.

## About [CorrelAid](https://correlaid.org/about/)
CorrelAid is a non-profit network of data science enthusiasts who want to engage themselves and use their knowledge for common good and to help social organizations on a fully voluntary basis. CorrelAid's work focuses on the social sector and aims to make the world a better place. Furthermore, the volunteers at CorrelAid educate social organizations on data-related topics and emphasizes the dialogue about the importance of data and data analysis for common good.

## About the [Hacklab Foundation](https://hacklabfoundation.org/)
The Hacklab Foundation is an international nonprofit organization headquartered in Ghana with a focus on preparing the youth for future digital jobs through technology education and skills development. The Hacklab Foundation achieves this through various events such as bootcamps, hackathons, mentorships, skills training and job placement.
The Foundation is known for its annual Hacklab Hackathon in Ghana that brings 1000+ developers, engineers, designers, entrepreneurs, policymakers, development partners and other stakeholders in technology to meet and discuss critical issues impacting the Global South.


-----

## Project Organization and Methods

The report was built with the R language ([r-project.org](https://www.r-project.org/)) and some bits of HTML and CSS for the design. The result is a standalone HTML report which can be hosted anywhere and offers more interactivity than a PDF.   

Run [Hacklab_Ghana_Developer_Census_2020.Rmd](https://github.com/CorrelAid/hacklab-foundation/blob/main/Developer%20Census%202020%20Report.Rmd) Rmarkdown to reproduce the HTML [report](https://github.com/CorrelAid/hacklab-foundation/blob/main/Developer-Census-2020-Report.html).  

To compile, the .Rmd needs:
1. the [cleaned data](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/clean_all_Qs.rds), that contains the answers to all the questions except questions 9-13 (the skills). 
2. the [cleaned skills](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/skills_final.csv) which contain the answers to the skills (tools and technologies) question 9-13.
3. a [theme.css](https://github.com/CorrelAid/hacklab-foundation/blob/main/css_theme/theme.css) file where the CSS of the report is defined
4. a [footer.html](https://github.com/CorrelAid/hacklab-foundation/blob/main/css_theme/footer.html) where the footer of the report is defined
5. the footer needs the logos of the Hacklab Foundation and of Correlaid, which are located in the same folder


### Data Cleaning
Data Cleaning and wrangling were done with the [raw anonymized data](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/raw/census-base-anonymized-2020_without_parsing_errors.xlsx). The data set went through the following data cleaning and wrangling steps: 
1. Major data cleaning was done with the scripts in [data_cleaning](https://github.com/CorrelAid/hacklab-foundation/tree/main/data_cleaning)
2. We joined these cleaned data sets together with [Merge_clean_data.R](https://github.com/CorrelAid/hacklab-foundation/blob/main/data_cleaning/Merge_clean_data.R). 
3. The cleaned output [clean_all_Qs.rds](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/clean_all_Qs.rds) is the dataset used for the analyses and the report. 
4. The survey questions about the tools and technologies are saved separately in [skills_final.csv](https://github.com/CorrelAid/hacklab-foundation/blob/main/data/clean/skills_final.csv) due to their different data formats. ([Data_Cleaning_ST.R](https://github.com/CorrelAid/hacklab-foundation/blob/main/data_cleaning/Data_Cleaning_ST.R) is the script used for this, note that we re-attributed all the answers of the free form question 13 to questions 9-12).  




