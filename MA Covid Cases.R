library(tidyverse)
library(knitr)
library(lubridate)
library(modelr)

covid_index_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_index_us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
today <- tail(covid_index_states,1)$date # Setting "today" to the latest date

# Data
ma <- covid_index_states %>% 
            filter(state == "Massachusetts") %>%
            mutate(delta_cases = coalesce(cases - lag(cases, 1))) %>% 
            select(date, cases, deaths, delta_cases)

# First graph
ggplot(data = ma, aes(x = date, y = cases))+
  geom_line()

# graph of deltas
ggplot(data = ma, aes(x = date, y = delta_cases))+
  geom_point()

#Remove deltas, group by month
ma2 <- ma %>% 
        filter(delta_cases >= 0 & date != "2020-11-26") %>% 
          mutate(month = month(date, label = TRUE),
                 wday = wday(date, label = TRUE))


#avg new cases / month
ma2 %>%
  group_by(month) %>% 
  summarise(avg_new_cases_month = mean(delta_cases)) %>% 
  ggplot(aes(x = month, y = avg_new_cases_month))+
  geom_col(aes(color = month, fill = month))+
  labs(title = "Average num of new cases per month",
       y = "Cases")

#avg new cases / wday
ma2 %>%
  group_by(wday) %>% 
  summarise(new_cases_wday = mean(delta_cases)) %>% 
  ggplot(aes(x = wday, y = new_cases_wday))+
  geom_col(aes(color = wday, fill = wday))+
  labs(title = "Average num of new cases per weekday",
       y = "Cases")
  

#Total cases for each month
ma2 %>% 
  group_by(month) %>% 
  slice(n()) %>% 
  ggplot(aes(x = month, y = cases))+
  geom_col(aes(color = month, fill = month))+
  labs(title = "Total cases split by month",
       y = "Cases")+
  scale_y_continuous(expand = expansion(mult = c(0, .02)))

# Plot deltas for each day
ma2 %>% ggplot(aes(date, delta_cases))+
        labs(title = "New MA cases per month",
          y = "New cases",
          x = "Month")+
        geom_line()+
        scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")+
        geom_smooth(se = FALSE)+
        scale_y_continuous(expand = expansion(mult = c(0, .02)))

# County data
ma_county <- covid_index_counties %>% 
              filter(state == "Massachusetts") %>% 
               select(date, county, state, cases, deaths) %>% 
                mutate(delta_cases = coalesce(cases - lag(cases, 1)),
                delta_deaths = coalesce(deaths - lag(deaths, 1)))


# Adding Season
szn <- function(date){
  cut(date,
      breaks = ymd(20200301, 20200601, 20200831, 20201231),
      labels = c("spring", "summer", "fall")
  )
}
ma2 %>%
  mutate(season = szn(date)) %>% 
  ggplot(aes(x = date, y = delta_cases, color = season))+
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")


# Using MASS::rlm
model3 <- MASS::rlm(delta_cases ~ date, data = ma2)
ma2 %>% 
  add_residuals(model3, "resid") %>% 
  ggplot(aes(date, resid))+
  geom_hline(yintercept = 0, color = "white", size = 2)+
  geom_line()

# US Data
us <- covid_index_us %>% 
  mutate(delta_cases = coalesce(cases - lag(cases, 1))) %>% 
  select(date, cases, deaths, delta_cases)

# Plot US Data
us %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  ggplot(aes(date, delta_cases))+
  labs(title = "New US cases per month",
       y = "New cases",
       x = "Month")+
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")+
  geom_smooth(se = FALSE)
