library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(usmap)
library(png)
library(Cairo)
library(stringr)
options(bitmapType='cairo')

#Import covid deaths spreadsheet
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
c19data = read.csv(url(urlfile))

countydata1 = read.csv('countydata1.csv', stringsAsFactors = FALSE, sep = ',', header = TRUE)
countydata1$FIPS = sprintf("%05d", as.numeric(countydata1$FIPS))
justfips = countydata1[,c('FIPS', 'TOT_POP')]
justfips = justfips %>% rename(fips = FIPS)
df = map_with_data(justfips, values = 'TOT_POP')
justfips = data.frame(justfips)

# Define UI ----
ui <- fluidPage(
  #Define a title panel
  titlePanel(h1('COVID County, USA', align = 'center')),
  
  hr(),
  
  helpText(h3('The COVID-19 pandemic has upended life as we know it. 
  It can be hard to fully understand the magnitude of the pandemic, particularly in the 
  United States, which has lost more people than any other country. 
  This app helps visualize the impact of the COVID-19 pandemic.'),
           h3('First, pick a date using the calendar widget below.
      The app will then create a map of the United States at the county level.'),
           h3('Counties shaded in red have a smaller population than the cumulative U.S. COVID-19 death total
                       as of the specified date.'),
           h3('Counties shaded in green have a larger population than the cumulative COVID-19 death total.'), align = 'center'),
  helpText('Copyright 2020, Alexander J. Adams', align = 'center'),
  tags$h6('Data:', align = 'left'),
  tags$a(href = 'https://github.com/nytimes/covid-19-data', 'The New York Times, '),
  tags$a(href = 'https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds', 'USCensus2010 Package'),
  
  fluidRow(
    column(4, offset = 4,
           h4('Select a date:'), 
           dateInput('date1', '', value = '2020-02-29', min = '2020-02-29', 
                     max = toString(max(c19data$date))), align = 'center')),
  fluidRow(
    column(4, offset = 4,
           textOutput("helpertext"), align = 'center')),
  fluidRow(
    column(12, plotOutput("selected_date"), align = 'center'))
)

# Define server logic ----
server <- function(input, output) {
  
  output$helpertext <- renderText({
    #First convert selected date to string
    chosendate = toString(input$date1)
    
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    
    (paste0('The cumulative COVID-19 death total in the United States on ', chosendate, ' was ', covidslice$deaths, 
            '.'))
  })
  
  #Specify output
  output$selected_date <- renderPlot({
    
    #First convert selected date to string
    chosendate = toString(input$date1)
    
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    
    justfips$covid = ifelse(justfips$TOT_POP > covidslice$deaths,'No','Yes')
    #df$covid = ifelse(df$TOT_POP > covidslice$deaths, 1, 0)
    
    bespokemap = plot_usmap(regions = 'counties', data = justfips, values = 'covid') + 
      ggplot2::aes(color = 'covid') + 
      scale_fill_manual(name = 'County Population > \nCOVID-19 Deaths?', 
                        values = c('#00DF34', '#E3170D'), labels = c('Yes', 'No')) + 
      theme(legend.position = 'left', legend.text = element_text(size = 20), legend.title = element_text(size = 24),
            panel.background = element_rect(fill = '#F0F8FF'))
    
    bespokemap
  }, height = 775, width = 1200)
}

# Run the app ----
shinyApp(ui = ui, server = server)