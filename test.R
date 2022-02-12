# https://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html

devtools::install_github("abresler/gdeltr2")
library(gdeltr2)

# -----------------------------------

# https://cran.r-project.org/web/packages/GDELTtools/GDELTtools.pdf

install.packages("GDELTtools")

library(GDELTtools)
df1 <- GetGDELT(start_date="2021-01-01", end_date="2021-12-31")
df1
