library(tidyverse)
library(lubridate)
library(readxl)
library(modelr)

# Reading
# To create a table for the year
#   week_beginning <-  seq(from = as.Date("2021-01-03"), to = as.Date("2021-12-31"), by = "1 week")
# hours <- c(1:52)
# cbind (week_beginning, hours)

payroll <- read_xlsx("Book1.xlsx")

# Formatting

payroll$week_ending <- as.Date(payroll$week_ending) # Convert to date format
payroll <- rename(payroll, OT_hours = `OT hours`) # Rename column
payroll$OT <- ifelse(payroll$Hours > 40, "1", "0") # Indicates if OT or not
payroll$OT_hours <- ifelse(payroll$OT_hours < 0, "0", payroll$OT_hours) # Remove negative OT hours

# Base Plot
plot <- payroll %>% 
        ggplot(aes(x = week_ending, y = Hours))+
        geom_ref_line(h = 40, size = 1)+
        geom_line()+
        geom_point(color = OT)

# Graphics
    plot + scale_x_date(breaks = payroll$week_ending, date_labels = "%m/%d")+
            scale_y_continuous(breaks = seq(from = 10, to = 64, by = 10))+
            scale_color_manual(values = c("black", "red"))+
            labs(title = "Hours worked 2020")+
            theme(axis.text.x = element_text(angle = -90, vjust=0.5),
                  panel.grid.minor = element_blank(),
                  legend.position = "none")

# Misc Notes
# Look to change the color of geom_line depending on y value (done with geom_points here but not with the geom_line)
