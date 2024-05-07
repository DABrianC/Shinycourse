#Course: Building WEb Applications with Shiny

#Chapter 1----
library(shiny)

#exercise 1 - create an input text box
ui <- fluidPage(
  #makes an input box for name. The box is called User1
  textInput("name", "User1:"),
  #the output object is called greeting
  textOutput("greeting")
  
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    #This makes text appear under the input box that says "Hello," 
    #and the text that is typed into the box
    paste("Hello,", input$name) #this pulls the name from the input box
  })
}

shinyApp(ui=ui, server = server)

#exercise 2 - baby name finder
library(shiny)
library(tidyverse)
library(babynames)

ui <- fluidPage(
  titlePanel("Baby Name Explorer")
  ,sidebarLayout(sidebarPanel(
    textInput(inputId = 'name', label = 'Enter Name', 'David')
  )
  # CODE BELOW: Display the plot output named 'trend'
  , 
  mainPanel(plotOutput('trend'))
  
)
)

server <- function(input, output, session) {
  # CODE BELOW: Render an empty plot and assign to output named 'trend'
  output$trend <- renderPlot({
    ggplot(subset(babynames, name == input$name)) +
      geom_line(aes(x = year, y = prop, color = sex))
  })
 }

shinyApp(ui = ui, server = server)

#exercise 3 - babynames app continued
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  # CODE BELOW: Add select input named "sex" to choose between "M" and "F"
  selectInput("sex"
              , "Select Sex"
              , selected = "F"
              , choices = c("M", "F")),
  #Add slider input named 'year' to select years  (1900 - 2010)
  sliderInput("year"
              , "Select Year"
              , value = 1900
              , min = 1900
              , max = 2010),
  # Add plot output to display top 10 most popular names
  plotOutput('plot_top_10_names'),
  #Add table output named "table_top_10_names"
  tableOutput("table_top_10_names")
)

server <- function(input, output, session){
  # Render plot of top 10 most popular names
  output$plot_top_10_names <- renderPlot({
    # Get top 10 names by sex and year
    top_10_names <- babynames %>% 
      # MODIFY CODE BELOW: Filter for the selected sex
      #filter for the slider year
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
    # Plot top 10 names by sex and year
    ggplot(top_10_names, aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
}

shinyApp(ui = ui, server = server)

#exercise 3 - babynames app continued - table not plot
library(DT)
ui <- fluidPage(
  titlePanel("What's in a Name?"),
  # CODE BELOW: Add select input named "sex" to choose between "M" and "F"
  selectInput("sex"
              , "Select Sex"
              , selected = "F"
              , choices = c("M", "F")),
  #Add slider input named 'year' to select years  (1900 - 2010)
  sliderInput("year"
              , "Select Year"
              , value = 1900
              , min = 1900
              , max = 2010),
  #Add table output named "table_top_10_names" and make it interactive using DT
  DT::DTOutput("table_top_10_names")
)

server <- function(input, output, session){
    # Get top 10 names by sex and year
    top_10_names <- function() {
      babynames %>% 
      # MODIFY CODE BELOW: Filter for the selected sex
      #filter for the slider year
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
    }
    #Render a table output named "table_top_10_names"
    output$table_top_10_names <- DT::renderDT({
      top_10_names()
  })
}

shinyApp(ui = ui, server = server)

#exercise 4 - adding plotly to the babynames stuff
library(plotly)
ui <- fluidPage(
  selectInput('name', 'Select Name', top_10_names$name),
  # CODE BELOW: Add a plotly output named 'plot_trendy_names'
  plotly::plotlyOutput("plotly_trendy_names"),
  #Add table output named "table_top_10_names" and make it interactive using DT
  DT::DTOutput("table_top_10_names")
  
)

server <- function(input, output, session){
  # Get top 10 names by sex and year
  top_10_names <- function() {
    babynames %>% 
      # MODIFY CODE BELOW: Filter for the selected sex
      #filter for the slider year
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
  }
  
  #Render a table output named "table_top_10_names"
  output$table_top_10_names <- DT::renderDT({
    top_10_names()
  })
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  # CODE BELOW: Render a plotly output named 'plot_trendy_names'
  output$plotly_trendy_names <- plotly::renderPlotly({
    plot_trends
  })
  
  
}

shinyApp(ui = ui, server = server)