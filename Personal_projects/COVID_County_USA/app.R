library(shiny)
library(maps)
library(mapproj)
library(readr)
library(ggplot2)
library(dplyr)
library(usmap)

# Define UI ----
ui <- fluidPage(
  #Define a title panel
  titlePanel(h1('COVID County, USA', align = 'center')),
  
  #Define an input for the sidebar panel
  sidebarLayout(
    #First, some help text
    sidebarPanel(helpText('Select a date:'),
                 #Next, a calendar input. Make sure to specify the first two arguments, even if the
                 #label is an empty string!
                 dateInput('date1', '', value = '2020-01-21', min = '2020-01-21', max = toString(max(c19data$date)))
                 ),
    mainPanel(textOutput("selected_date"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  #Import covid deaths spreadsheet
  urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
  c19data = read.csv(url(urlfile))

  #Import county population data
  countydata = read.csv('countydata1.csv')

  countydata$FIPS = sprintf("%05d", as.numeric(countydata$FIPS))
  
  justfips = select(countydata, FIPS, TOT_POP)
    
  #Import county map data
  #counties = map_data('county')
  
  #Adjust capitalization
  #simpleCap <- function(x) {
  #  s <- strsplit(x, " ")[[1]]
  #  paste(toupper(substring(s, 1,1)), substring(s, 2),
  #        sep="", collapse=" ")
  #}
  #Region = state, subregion = county
  #counties$region = sapply(counties$region, simpleCap)
  #counties$subregion = sapply(counties$subregion, simpleCap)
  
  #allcounty = merge(counties, countydata, by = c('region', 'subregion'), all = TRUE)
  
  #Specify output
  output$selected_date <- renderPlot({
    
    #First convert selected date to string
    chosendate = toString(input$date1)
    
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    #countyslice = subset(countydata, TOT_POP >= covidslice$deaths)
    #countydata$more_covid = ifelse(countydata$TOT_POP >= covidslice$Deaths, 1, 0)
    
    #paste0("On ", input$date1, " the total number of U.S. deaths from COVID-19 was greater than the populations of ",
    #       countyslice$County,'.')
    
    usmap::plot_usmap(regions = 'counties', data = justfips, values = 'TOT_POP', color = TOT_POP > covidslice$deaths)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
