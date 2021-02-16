library(tidyverse)
library(lubridate)

statement_month <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "1 month")
statement_amounts_20 <- c(196.30, 251.96, 266.47, 42.80, 183.38, 364.45, 188.13, 257.46, 943.24, 194.16, 300.38, 306.07)
statement_amounts_19_d <- c(9.79, 341.49, 153.67, 39.91, 83.22, 71.25, 104.83, 0, 17.39, 0, 148.74, 0)
statement_amounts_19_u <- c(0, 0, 0, 0, 0, 0, 0, 73.04, 384.60, 77.19, 687.90, 305.46)
statement_amounts_21 <- c(186.76)

total_statements <- data.frame(statement_month, statement_amounts_20, statement_amounts_19_d, statement_amounts_19_u)
total_statements <- total_statements %>% mutate(statement_amounts_19_total = statement_amounts_19_d + statement_amounts_19_u)

base <- ggplot(total_statements, aes(x = statement_month))+
          geom_line(aes(y = statement_amounts_20, color = "2020"))+
          geom_line(aes(y = statement_amounts_19_total, color = "2019"))

base+scale_x_date(breaks = "1 month", date_labels = "%b")+
      scale_y_continuous(breaks = seq(0, 1000, by = 200))+
    labs(title = "Credit Card Statement Amounts",
         y = "Amount")