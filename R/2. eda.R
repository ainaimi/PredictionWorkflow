#' List necessary packages here
packages <- c("splines","data.table","parallel","SuperLearner",
              "doParallel","doRNG","tidyverse","VIM","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN',dependencies=TRUE)
  }
}

for (package in packages) {
  library(package, character.only=T)
}

#' Format graphs in ggplot
thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

#' Read in data and explore

a <- read_csv(here("data","MOMI_4.01.2019.csv"))

glimpse(a)

#' Missing data

aggr(a)

#' Explore distributions and relations for all relevant variables
#' -- See e.g.: https://r4ds.had.co.nz/exploratory-data-analysis.html
