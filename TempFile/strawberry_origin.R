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
#To make the app run smoothly, we will use ag_data_strawberry.Rmd to tidy the dataset.
#The RMarkdown file will generate a .csv file names bberry.csv
#If you want to consume the script into the app, uncomment the following line.
#Make sure the origin data file, berries.csv is in your working directory.
#source("ag_data_strawberry.Rmd")
sberry<-read.csv2("sberry.csv",header=TRUE,sep=",")


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
    navbarPage("Quick Stats for Strawberry",theme=shinytheme("lumen"),
               tabPanel("Chemical Treatment Stats",fluid = TRUE,
               tags$style(button_color_css),
               sidebarLayout(
                   sidebarPanel(
                       titlePanel("Characteristics"),
                       
                       #select type
                   fluidRow(column(3,
                       selectInput(inputId="TypeFinder",
                                   label = "Select Type",
                                   choices=c("BEARING - TREATED","BEARING - APPLICATIONS"),
                                   selected = "BEARING - TREATED",
                                   width="220px"
                                   )),
                       
                       #select State
                       column(3,
                                       checkboxGroupInput(inputId="StateFinder",
                                                          label="Select State(s):",
                                                          choices=unique(sberry$State),
                                                          selected="CALIFORNIA")
               )),
               hr(),
               sliderInput(inputId = "YearFinder",
                           label = "Select Time Range:",
                           min=2015,
                           max=2019,
                           value=c(2015,2016),
                           width="220px"),
               hr(),
               checkboxGroupInput(inputId="ChemFinder",
                                  label="Select Chemical:",
                                  choices=unique(sberry$Chem),
                                  selected="CHEMICAL")

               ),
               mainPanel(
                   fluidRow(column(12,
                                   withSpinner(dataTableOutput(outputId = "berrytable"))
                   )),
                   br(),
                   fluidRow(column(6,withSpinner(plotOutput("hist"))),
                       column(6,withSpinner(plotOutput("pie")))
                           )      
                        ) 
                      )
              ),
              tabPanel("Killer Compound Stats",fluid=TRUE)

               
               
               
               
               
               )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    BerryFinder<-reactive({
        req(input$TypeFinder)
        req(input$StateFinder)
        req(input$YearFinder)
        req(input$ChemFinder)
        filter(sberry,Status %in% input$TypeFinder)%>%
            filter(State %in% input$StateFinder)%>%
            filter(Year >= input$YearFinder[1],Year<=input$YearFinder[2])%>%
            filter(Chem %in% input$ChemFinder)
    })
    output$berrytable<-renderDataTable({
        datatable(BerryFinder())
    })
    output$hist<-renderPlot({
        ggplot(BerryFinder(),aes(x=killer,color=Chem,fill=Chem))+geom_histogram(position="identity",stat="count")
    }
    )
    output$pie<-renderPlot({
        n<-length(BerryFinder()[1])*100
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
            )
        ggplot(BerryFinder(),aes(x="killer",color=Chem,fill=killer))+geom_bar(stat="count",position="stack",width=1)+
            coord_polar(theta="y",start=0)+labs(x='',y='',title='')+geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
