library(shiny)

fluidPage(
  titlePanel(""),
  
  tabPanel("Graf",
           sidebarPanel(
             selectInput("Leto", label = "Izberi leto", 
                         choices = unique(gospodarskadejavnost$leto))),
           mainPanel(plotOutput("graf_dejavnosti"),
                     tableOutput("legenda")))
  
)
