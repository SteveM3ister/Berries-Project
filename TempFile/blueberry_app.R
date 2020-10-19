#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(shinycssloaders)
library(shinythemes)
library(tidyr)
library(dplyr)
library(DT)
#To make the app run smoothly, we will use ag_data.R to tidy the dataset.
#The ag_data.R will generate a .csv file names bberry.csv
#If you want to consume the script into the app, uncomment the following line.
#Make sure the origin data file, berries.csv is in your working directory.
#source("ag_data.R")
bberry<-read.csv2("bberry.csv",header=TRUE,sep=",")


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Berries Project"),


    #Navbar 
    navbarPage("Quick Stats for Blueberry",theme=shinytheme("lumen"),
               tabPanel("Killer Stats",fluid = TRUE,
               tags$style(button_color_css),
               sidebarLayout(
                   sidebarPanel(
                       titlePanel("Characteristics"),
                       
                       #select type
                   fluidRow(column(6,
                       selectInput(inputId="TypeFinder",
                                   label = "Select Type",
                                   choices=bberry$type,
                                   selected = "TAME",
                                   width="220px"
                                   )),
                       
                       #select State
                       column(6,
                                       checkboxGroupInput(inputId="StateFinder",
                                                          label="Select State(s):",
                                                          choices=unique(bberry$State),
                                                          selected="CALIFORNIA")
               )),
               hr(),
               sliderInput(inputId = "YearFinder",
                           label = "Select Time Range:",
                           min=2015,
                           max=2019,
                           value=c(2015,2015),
                           width="220px"),
               hr(),
               checkboxGroupInput(inputId="ChemFinder",
                                  label="Select Chemical:",
                                  choices=unique(bberry$Chem),
                                  selected="TOTAL")

               ),
               mainPanel(
                   # withSpinner(plotOutput(outputID="plot")
                   #             ),
                   # hr(),
                   # fluidRow(column(width=2,conditionalPanel(
                   #     condition="output.berrytableFinder",
                   #     actionButton(inputID="FinderClear",label="Clear Table")
                   # ))),
                   # br(),
                   fluidRow(column(10,
                       withSpinner(dataTableOutput(outputId = "berrytable"))
                   )),
                   br(),
                   fluidRow(
                       withSpinner(plotOutput("hist"))
                   )
   
                   )
               )
               )
               )
               
               
               
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    BerryFinder<-reactive({
        req(input$TypeFinder)
        req(input$StateFinder)
        req(input$YearFinder)
        req(input$ChemFinder)
        filter(bberry,type %in% input$TypeFinder)%>%
            filter(State %in% input$StateFinder)%>%
            filter(Year >= input$YearFinder[1],Year<=input$YearFinder[2])%>%
            filter(Chem %in% input$ChemFinder)
    })
    output$berrytable<-DT::renderDataTable({
        DT::datatable(BerryFinder())
    })
    output$hist<-renderPlot({
        ggplot(BerryFinder(),aes(x=killer,color=Chem,fill=Chem))+geom_histogram(position="identity",stat="count")
    }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
