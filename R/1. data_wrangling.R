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

#' Keep only relevant variables
#' using na.omit() to simplify. need to handle missing data appropriately

analytic_data <- a %>% select(ptb32sp,delyear,dmomrace,dmomage,dmomedu2,dmomhgt,mhxpara,dmommari,zipcode) %>% na.omit()

#' Format data
#'
#' --set all continuous variables to as.numeric
#' --set all categorical variables with more than two levels to as.factor
#' --set all binary variables to 0/1 indicators

analytic_data <- analytic_data %>% mutate(dmomrace = as.factor(dmomrace),
                                          dmomedu2 = as.factor(dmomedu2),
                                          mhxpara = as.factor(mhxpara),
                                          dmommari = as.factor(dmommari))


#' Save cleaned and transformed data for analysis

write_csv(analytic_data,here("data","analytic_data.csv"))




