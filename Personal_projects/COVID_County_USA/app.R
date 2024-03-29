library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(usmap)
library(png)
library(Cairo)
library(stringr)
library(DT)
library(purrr)
options(bitmapType='cairo')
#setwd("C:/Users/alexi/Desktop/data_visualizations/Personal_projects/COVID_County_USA")
#Import covid deaths spreadsheet
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
c19data = read.csv(url(urlfile))

countydata1 = read.csv('countydata2020.csv', stringsAsFactors = FALSE, sep = ',', header = TRUE)
#countydata1$FIPS = sprintf("%05d", as.numeric(countydata1$FIPS))
#justfips = countydata1[,c('FIPS', 'TOT_POP')]
countydata1 = countydata1 %>% rename('fips' = FIPS, 'State' = region, 'County' = subregion, 
                                     'Total_Population' = TOT_POP)
#remove_if = c('city','Parish')
#countydata1_a = subset(countydata1, !grepl(paste(remove_if, collapse="|"), County))
#countydata1_a$County = paste(countydata1_a$County, 'County')
#countydata1_b = subset(countydata1, grepl(paste(remove_if, collapse="|"), County))
#countydata1 = rbind.data.frame(countydata1_a,countydata1_b)
                               
newlist = c()
for(ii in (countydata1$fips)){
  abc = countydata1 %>% filter(fips == ii)
  x = c19data$date[min(which(c19data$deaths > abc$Total_Population))]
  newlist = append(newlist,x)
}
countydata1 = cbind.data.frame(countydata1, newlist) %>% rename('Date' = newlist) %>% select(fips, County, 
                                                                                             State, Date, 
                                                                                             Total_Population)
df = map_with_data(countydata1, values = 'Total_Population')
countydata1 = data.frame(countydata1)
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
           h3('Counties shaded in green have a larger population than the cumulative COVID-19 death total.'),
           h4("In the table below, the column 'Date' shows the date when the cumulative COVID-19 death total surpassed 
              that county's population."),
           h4("The column 'covid' indicates if the death total has surpassed the county's population
              as of the date entered in the calendar widget."),align = 'center'),
  helpText('Copyright 2020, Alexander J. Adams', align = 'center'),
  tags$h6('Data:', align = 'left'),
  tags$a(href = 'https://github.com/nytimes/covid-19-data', 'The New York Times, '),
  tags$a(href = 'https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-counties-total.html', '2020 U.S. Census Data'),
  
  fluidRow(
    column(4, offset = 4,
           h4('Select a date:'), 
           dateInput('date1', '', value = '2020-02-29', min = '2020-02-29', 
                     max = toString(max(c19data$date))), align = 'center')),
  fluidRow(
    column(4, offset = 4,
           textOutput("helpertext"), align = 'center')),
 fluidRow(column(8, plotOutput("selected_date")), column(4,dataTableOutput("countytable")))
)

# Define server logic ----
server <- function(input, output) {
  
  output$helpertext <- renderText({
    #First convert selected date to string
    chosendate = toString(input$date1)
    
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    countydata1$covid = ifelse(countydata1$Total_Population > covidslice$deaths,'No','Yes')
    no = countydata1 %>% filter(covid == 'No') %>% arrange(Total_Population)
    yes = countydata1 %>% filter(covid == 'Yes') %>% arrange(desc(Total_Population))
    (paste0('The cumulative COVID-19 death total in the United States on ', chosendate, ' was ', covidslice$deaths, 
            '.','\n\n\nThe smallest county with a population larger than the cumulative 
            U.S. COVID-19 death total on this date was ', no$County[which.min(no$Total_Population)],', ',
            no$State[which.min(no$Total_Population)], ' (Population: ',min(no$Total_Population),').',
            '\n\n\nThe largest county with a population smaller than the cumulative 
            U.S. COVID-19 death total on this date was ', yes$County[which.max(yes$Total_Population)],', ',
            yes$State[which.max(yes$Total_Population)],' (Population: ',max(yes$Total_Population),').'))
  })
  
  #Specify output
  output$selected_date <- renderPlot({
    #First convert selected date to string
    chosendate = toString(input$date1)
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    countydata1$covid = ifelse(countydata1$Total_Population > covidslice$deaths,'No','Yes')
    #df$covid = ifelse(df$TOT_POP > covidslice$deaths, 1, 0)
    bespokemap = plot_usmap(regions = 'counties', data = countydata1, values = 'covid') + 
      ggplot2::aes(color = 'covid') + 
      scale_fill_manual(name = 'County Population > COVID-19 Deaths?', 
                        values = c('#00DF34', '#E3170D'), labels = c('Yes', 'No')) + 
      theme( legend.position = 'top', 
             legend.justification = 'left',
             legend.direction = "horizontal", legend.text = element_text(size = 20), legend.title = element_text(size = 24),
            panel.background = element_rect(fill = '#F0F8FF'))
    bespokemap
  }, height = 775, width = 1200)
  
  output$countytable <- renderDataTable({
    #First convert selected date to string
    chosendate = toString(input$date1)
    #Then create data frame slice of just selected date
    covidslice = subset(c19data, date == chosendate)
    countydata1$covid = ifelse(countydata1$Total_Population > covidslice$deaths,'No','Yes')
    countydata1 = countydata1 %>% select(!c(fips)) %>% arrange(by = desc(Date))
    countydata1})
}

# Run the app ----
shinyApp(ui = ui, server = server)