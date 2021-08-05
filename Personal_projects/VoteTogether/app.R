library(readr)
library(shiny)
library(tidyverse)

imgs = "https://raw.githubusercontent.com/voteview/member_photos/main/members.csv"
imgs = read.csv(url(imgs))
df_Hv = readRDS('Hall_votes.rds')
df_Hm = readRDS('Hall_members.rds')
df_Sv = readRDS('Sall_votes.rds')
df_Sm = readRDS('Sall_members.rds')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("How Often Do Members of Congress Vote Together?", align = 'center')),
    hr(),
    helpText(h3('This app lets you compare two representatives or senators and see what percentage of the time
       they voted together in a given congress.',align = 'center'),
             h3('The first congress available for analysis is the 79th Congress,
       which took office in 1945.',align = 'center'),
             h3("First, use the 'Congress' text box to choose a Congress 
                by entering a number from 79 to 117 (the current Congress).", align = 'center'),
             h3("Next, use the 'Chamber' drop-down menu to choose a chamber, either the House of Representatives or the Senate.",
                align = 'center'),
             h3("Finally, use the 'Member 1' and 'Member 2' drop-down search bars to find the members you want to compare.
                The app will do the rest!", align = 'center')),
    helpText('Copyright 2021, Alexander J. Adams', align = 'center'),
    tags$h6('Data:', align = 'left'),
    tags$a(href = 'https://voteview.com/', 'VoteView.com'),


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           numericInput('congress',
                        'Congress',
                        value = 117,
                        min = 79,
                        max = 117,
                        step = 1,
                        width = '300px'),
           selectInput('chamber', 'Chamber',
                       c('House of Representatives', 'Senate'),width = '300px'),
           uiOutput('selector1'),
           uiOutput('selector2')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput('helpertext')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    namedf <- reactive({
        if(input$chamber == 'House of Representatives'){
            df = df_Hm %>% filter(congress == input$congress & chamber == 'House') %>% select(bioname,icpsr)}
        if(input$chamber == 'Senate'){
            df = df_Sm %>% filter(congress == input$congress & chamber == 'Senate') %>% select(bioname,icpsr)}
        return(df)
    })
    
    votedf <- reactive({
        if(input$chamber == 'House of Representatives'){
            df = df_Hv %>% filter(congress == input$congress) %>% select(icpsr,cast_code,rollnumber) %>% pivot_wider(names_from = 'rollnumber',values_from = 'cast_code')}
        if(input$chamber == 'Senate'){
            df = df_Sv %>% filter(congress == input$congress) %>% select(icpsr,cast_code,rollnumber) %>% pivot_wider(names_from = 'rollnumber',values_from = 'cast_code')}
        return(df)
    })
    
    output$selector1 <- renderUI(
        selectInput('member1','Member 1',choices = namedf()$bioname)
        )
    
    output$selector2 <- renderUI(
        selectInput('member2','Member 2',choices = namedf()$bioname)
    )
    
    output$helpertext <- renderText({
        members1 = namedf() %>% filter(bioname %in% c(input$member1))
        members2 = namedf() %>% filter(bioname %in% c(input$member2))
        votes1 = votedf() %>% filter(votedf()$icpsr %in% members1$icpsr)
        votes2 = votedf() %>% filter(votedf()$icpsr %in% members2$icpsr)
        votes = rbind.data.frame(votes1,votes2)
        #rownames(votes) <- c('member1','member2')
        votes = data.frame(t(votes))
        #votes = votes %>% pivot_longer(cols = colnames(votes), names_to = c('member1','member2'), names_pattern = ('member'))
        colnames(votes) <- c('member1','member2')
        votes$simscore <- ifelse(votes$member1 == votes$member2,1,0)
        simscore = (sum(votes$simscore)/length(votes$simscore))*100
        (paste0(input$member1,' and ',input$member2, ' voted together ', simscore, '% of the time, or ',
              sum(votes$simscore), ' out of ', length(votes$simscore), ' votes.',sep = ''))
        
        
    })
}
# Run the application 
shinyApp(ui = ui, server = server)