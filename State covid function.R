library(tidyverse)
library(knitr)

# Enter "state" to present graph of cases and percentage of total reported Covid cases in the US

state_covid <- function(state1){
  
  # Getting raw data
  
  covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  covid_index_us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

  # Get data for the state 
  
    state_index <- covid_index_states %>% 
      filter(state == state1) %>% 
      mutate(delta_cases = coalesce(cases - lag(cases, 0)),
             delta_deaths = coalesce(deaths - lag(deaths, 0))) %>% 
      filter(delta_cases >= 0 & delta_deaths >= 0)
  
  # Get the last row for the state
  
    current_state_indx <- tail(state_index, 1) 
  
  # Get last row for the country
    
    current_us_index <- tail(covid_index_us, 1) 
  
  # Calculating percentages    
  
  state_prop_cases <-  round(current_state_indx$cases / current_us_index$cases, digits = 4)
  state_prop_deaths <-  round(current_state_indx$deaths / current_us_index$deaths, digits = 4)
  
  # Combining tables
  total_index <- covid_index_us %>%
    left_join(state_index, by = "date") %>%
    mutate(prop_cases = coalesce(cases.y / cases.x * 100, 0),
           prop_deaths = coalesce(deaths.y / deaths.x * 100, 0))
  
  # Plotting percenatages
  # prop_plot <- ggplot(data = total_index, aes(x = date, y = prop_cases))+
  #         geom_line()+
  #           labs(y = "Percent of cases",
  #             x = "Month",
  #             title = str_c("Percentage of total cases for ", state1))
  # print(prop_plot)
  
  # Basic Plot of cases
  plot <- ggplot(data = state_index, aes(x = date))+
                geom_line(aes(y = cases, color = "cases"))+
                geom_line(aes(y = deaths, color = "deaths"))+
                geom_line(aes(y = delta_cases, color = "delta cases"))+
                scale_x_date(breaks = "1 month", date_labels = "%b")+
                scale_y_continuous(n.breaks = 6)+
                labs(title = state1)
  
  print(plot)
  
  
  # Print message
  current_date <- current_us_index$date
  str_c(state1, " accounts for ",
        state_prop_cases * 100, "% of total US covid cases and ",
        state_prop_deaths * 100, "% of total US covid realted deaths as of ",
        format(current_date, "%m-%d-%Y"))
}
