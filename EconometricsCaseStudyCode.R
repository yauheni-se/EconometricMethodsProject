# Setup:
library(dplyr)
library(magrittr)
library(zoo)
library(AER)
library(systemfit)

# Read in and manipulate data:
data <- read.csv2("EconometricsCaseStudyData.csv")

data <- data %>%
  rename(date = X, terminals_p1000 = POS_terminals_per_1000ppl, cards_pc = Cards_per_capita) %>% 
  mutate(date = as.yearmon(date, "%b.%y"),  # convert to zoo object
         terminals_p1000 = zoo(log10(terminals_p1000), order.by = date),  # we want the impact of 1%incr. so use log10
         cards_pc = zoo(cards_pc, order.by = date))
str(data)
summary(data)

# Adding control variables:
data_iv <- data %>%
  mutate(terms_sqrt = sqrt(abs(terminals_p1000)),
         terms_lag = na.locf(lag(terminals_p1000)),
         terms_cube = terminals_p1000^(1/3))

write.csv(data_iv, "data_extended_Yauheni_Semianiuk_82591.csv")

# Simultaneity bias:
cards <- cards_pc ~ terminals_p1000
terminals <- cards_pc ~ terminals_p1000 + terms_lag+terms_cube + terms_sqrt
sys <- list(cards,terminals)
instr <- ~ terms_lag + terms_cube + terms_sqrt

model_simult <- systemfit(sys, inst = instr, method = "2SLS", data = data_iv)
summary(model_simult)$eq[[1]]

# OLS:
model_ols <- lm(cards_pc ~ terminals_p1000, data = data_iv)

# 2SLS:
model_2sls <- ivreg(cards_pc ~ terminals_p1000|terms_lag + terms_cube + terms_sqrt, data = data_iv)

# Weak instruments, Wu-Hausman, Sargan  tests:
summary(model_2sls, diagnostics = TRUE)$diagnostics

# t and F statistic of significance:
model_terms_ols <- lm(terminals_p1000 ~ terms_sqrt + terms_lag + terms_cube, data = data_iv)
summary(model_terms_ols)$coefficients[, 3:4]