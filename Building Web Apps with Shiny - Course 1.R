#Course: Building WEb Applications with Shiny

#Chapter 1----
library(shiny)

#exercise 1
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
    paste("Hello,", input$name)
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
    textInput('name', 'Enter Name', 'David')
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


