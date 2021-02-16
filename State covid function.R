library(tidyverse)
library(knitr)
library(lubridate)

# Enter "state" to present graph of cases

state_covid <- function(state1){
  
  # Getting raw data
  
  covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  covid_index_us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

  # Get data for the state 
  
    state_index <- covid_index_states %>% 
      filter(state == state1) %>% 
      mutate(delta_cases = coalesce(cases - lag(cases, 1)),
             delta_deaths = coalesce(deaths - lag(deaths, 1))) %>% 
      filter(delta_cases > 0 & delta_deaths > 0) # Removes negative deltas due to state reporting incorrectly
  
  # Get the last row for the state
  
    state_today <- tail(state_index, 1)
  
  # Get last row for the country
    
    us_today <- tail(covid_index_us, 1)
  
  # Combining tables
  total_index <- covid_index_us %>%
    left_join(state_index, by = "date") %>%
    mutate(prop_cases = coalesce(cases.y / cases.x * 100, 0),
           prop_deaths = coalesce(deaths.y / deaths.x * 100, 0))
  
  # Basic Plot of cases
  total_plot <- ggplot(data = state_index, aes(x = date))+
                geom_line(aes(y = cases))+
                scale_x_date(breaks = "1 month", date_labels = "%b")+
                scale_y_continuous(n.breaks = 15)+
                labs(title = state1,
                     y = "Total Covid-19 Cases",
                     x = "")
  
  delta_plot <- ggplot(data = state_index, aes(x = date))+
    geom_line(aes(y = delta_cases))+
    scale_x_date(breaks = "1 month", date_labels = "%b")+
    scale_y_continuous(n.breaks = 15)+
    labs(title = state1,
         y = "New Covid-19 Cases",
         x = "")
  
  
  
  print(total_plot)
  print(delta_plot)
}
