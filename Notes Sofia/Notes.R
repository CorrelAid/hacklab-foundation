devtools::install_github("gadenbuie/xaringanExtra")
library(xaringanExtra)
library(tidyverse)
library(tibble)
library(purrr)
library(showtext)
library(extrafont)
font_files()

x = mvtnorm::rmvnorm(1000, c(0,0), diag(2))
plot(x, 
     las = 1, ylab = "Dependent variable", 
     xlab = "Independent variable", 
     pch = 19, col = rgb(0,0,1,.1) )
