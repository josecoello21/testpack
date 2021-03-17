library(testthat)
library(tidyverse)

expect_that(fars_summarize_years(2013) %>% dim(), equals(c(12,2))) %>%
  suppressMessages()
