library(shiny)
library(tidyverse)
library(jsonlite)
library(curl)

url <- readLines(curl("https://api.covid19api.com/summary")) # free covid-19 api from covid19api.com
covid <- fromJSON(txt = url)

covid <- covid$Countries
drop <- "Premium"
covid <- covid[, !(names(covid) %in% drop)] # api offers premium data that we can drop

countries <- covid$Country # list of countries

ui <- fluidPage(
  titlePanel("COVID-19 Statistics by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", c("-", countries)),
    ),
    mainPanel(
      plotOutput("barPlot"),
      textOutput("country_title"),
      tableOutput("summary"),
      textOutput("date_title"),
      textOutput("date_updated"),
      textOutput("global_title"),
      tableOutput("confirm_table"),
      tableOutput("death_table"),
      tableOutput("recover_table")
    )
  )
)

server <- function(input, output, session) {
  covid_data <- reactive({
    covid %>%
      select(Country, NewConfirmed, TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered, Date) %>%
      filter(Country == input$country) %>%
      tibble() # coerce reactive expression to dataframe
  })

  output$barPlot <- renderPlot({
    req(input$country != "-")
    'title <- sprintf(
      "COVID-19 statistics from %s",
      input$country)'
    barplot(c(
      covid_data()$TotalConfirmed, covid_data()$NewConfirmed, covid_data()$TotalDeaths,
      covid_data()$NewDeaths, covid_data()$TotalRecovered, covid_data()$NewRecovered
    ),
    main = sprintf("COVID-19 statistics from %s", input$country),
    names.arg = c("Confirmed Cases", "New Confirmed Cases", "Deaths", "New Deaths", "Recovered Cases", "New Recovered Cases")
    )
  })

  output$country_title <- renderText({
    req(input$country != "-")
    "Country Statistics"
  })

  output$summary <- renderTable({
    req(input$country != "-")
    covid_data() %>%
      summarize(
        "Confirmed Cases" = TotalConfirmed, "New Confirmed Cases" = NewConfirmed, "Deaths" = TotalDeaths,
        "New Deaths" = NewDeaths, "Recovered Cases" = TotalRecovered, "New Recovered Cases" = NewRecovered
      )
  })
  output$date_title <- renderText({
    req(input$country != "-")
    "Last Updated:"
  })
  output$date_updated <- renderPrint({
    req(input$country != "-")
    covid_data()$Date
  })
  output$global_title <- renderText({
    "Global Statistics"
  })

  output$confirm_table <- renderTable(
    {
      covid %>%
        summarize(
          "Total Confirmed Cases" = sum(TotalConfirmed), "Median Confirmed Cases" = floor(median(TotalConfirmed)),
          "New Confirmed Cases" = sum(NewConfirmed), "Median New Confirmed Cases" = floor(median(NewConfirmed))
        )
    },
    width = 800
  )
  output$death_table <- renderTable(
    {
      covid %>%
        summarize(
          " Total Deaths" = sum(TotalDeaths), " Median Deaths" = floor(median(TotalDeaths)),
          " New Deaths" = sum(NewDeaths), " Median New Deaths" = floor(median(NewDeaths))
        )
    },
    width = 800
  )
  output$recover_table <- renderTable(
    {
      covid %>%
        summarize(
          "Total Recovered Cases" = sum(TotalRecovered), "Median Recovered Cases" = floor(median(TotalRecovered)),
          "New Recovered Cases" = sum(NewRecovered), "Median New Recovered Cases" = floor(median(NewRecovered))
        )
    },
    width = 800
  )
}

shinyApp(ui = ui, server = server)
