library(shiny)
library(tidyverse)
library(lubridate)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>%
  separate(state, into = c("dot", "state"), extra = "merge") %>%
  select(-dot) %>%
  mutate(state = str_to_lower(state))

covid19_2 <- covid19 %>%
  mutate(state_lower = tolower(state)) %>%
  group_by(state_lower) %>%
  mutate(recent = max(cases))

covid_with_2018_pop_est <- covid19_2 %>%
  left_join(census_pop_est_2018,
            by = c("state_lower" = "state")) %>%
  mutate(ymd= ymd(date)) %>%
  mutate(lag1 = lag(cases, 1, order_by = date, replace_na(0))) %>%
  mutate(daily_new_cases = (cases - lag1)) %>%
  mutate(daily_covid_per_10000 = (daily_new_cases/est_pop_2018)*10000)

ui <- fluidPage(titlePanel("Covid19 Cases per state"),
                selectInput(inputId = "state",
                            label = "Different States:",
                            choices = unique(covid_with_2018_pop_est$state),
                            multiple = TRUE),
                sliderInput(inputId = "ymd",
                            label = "Dates:",
                            min = as.Date("2020-01-21", "%Y-%m-%d"),
                            max = as.Date("2022-03-30", "%Y-%m-%d"),
                            value = c(as.Date("2020-01-21", "%Y-%m-%d"), as.Date("2022-03-30", "%Y-%m-%d")),
                            timeFormat = "%Y-%m-%d"),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "Covid19_plot"))

server <- function(input, output) {
  output$Covid19_plot <- renderPlot(
    covid_with_2018_pop_est %>%
      filter(state %in% input$state) %>%
      ggplot(aes(x= ymd, y= daily_covid_per_10000, color = state)) +
      geom_line() +
      scale_x_date(limits = input$ymd) +
      labs(x = "Date",
           y = "",
           color = "State")
  )}

shinyApp(ui = ui, server = server)