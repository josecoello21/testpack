library(testthat)
library(tidyverse)
source(file.path(getwd(), 'R', 'fars_functions.R'))

setwd(file.path(getwd(), 'test'))

expect_that(fars_summarize_years(2013) %>% dim(), equals(c(12,2))) %>%
  suppressMessages()
