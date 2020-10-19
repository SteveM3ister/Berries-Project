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
    titlePanel("Berries Project"),
    navbarPage("Quick Stats for Starberries",theme=shinytheme("lumen"),
    # Application title
               tabPanel("Chemical Comparison",fluid=TRUE,icon=icon("bong"),tags$style(button_color_css),
    #tags$h2("Comparison of Chemical Treatments on Strawberries"),
    hr(),
    fluidRow(
        column(6,tags$h3("Condition A")),
        column(6,tags$h3("Condition B"))
    ),
    fluidRow(
        column(6,
               wellPanel(
                   fluidRow(column(6,
                              selectInput(inputId="TypeFinder1",
                                          label = "Select Type",
                                          choices=c("BEARING - TREATED","BEARING - APPLICATIONS"),
                                          selected = "BEARING - TREATED",
                                          width="220px"
                              ),
                              checkboxGroupInput(inputId="StateFinder1",
                                                 label="Select State(s):",
                                                 choices=unique(sberry$State),
                                                 selected="CALIFORNIA")),
                            column(6,
                              sliderInput(inputId = "YearFinder1",
                                          label = "Select Time Range:",
                                          min=2015,
                                          max=2019,
                                          value=c(2015,2016),
                                          width="220px"),
                              checkboxGroupInput(inputId="ChemFinder1",
                                                 label="Select Chemical:",
                                                 choices=unique(sberry$Chem),
                                                 selected="CHEMICAL"))
                              
                   )
               )),
        column(6,
               wellPanel(
                   fluidRow(
                       column(6,
                              selectInput(inputId="TypeFinder2",
                                          label = "Select Type",
                                          choices=c("BEARING - TREATED","BEARING - APPLICATIONS"),
                                          selected = "BEARING - TREATED",
                                          width="220px"
                              ),
                              checkboxGroupInput(inputId="StateFinder2",
                                                 label="Select State(s):",
                                                 choices=unique(sberry$State),
                                                 selected="CALIFORNIA")),
                       column(6,
                              sliderInput(inputId = "YearFinder2",
                                          label = "Select Time Range:",
                                          min=2015,
                                          max=2019,
                                          value=c(2015,2016),
                                          width="220px"),
                              checkboxGroupInput(inputId="ChemFinder2",
                                                 label="Select Chemical:",
                                                 choices=unique(sberry$Chem),
                                                 selected="CHEMICAL"))
                              
                       )
                   )
               )
    ),
    fluidRow(
        column(6,tags$h4("Histogram for Condition A"),
               plotOutput("a_Hist")),
        column(6,tags$h4("Histogram for Condition B"),
               plotOutput("b_Hist"))
    ),
    fluidRow(
        column(6,tags$h4("Pie Chart for Condition A"),
               plotOutput("a_Pie")),
        column(6,tags$h4("Pie Chart for Condition B"),
               plotOutput("b_Pie"))
    ),
    tags$h4("Table of Condition A"),
    hr(),
    fluidRow(
               dataTableOutput("a_Table")),
    tags$h4("Table of Condition B"),
    hr(),
    fluidRow(
               dataTableOutput("b_Table")
    )
    ),
    tabPanel("More",icon=icon("info-circle"),
             fluidRow(
                        hr(),
                        h3("About this Project"),
                        h4("This project began as practice of ShinyApp for MA615(Data Science in R) of MSSP program in Boston University."),
                        h4("The project is intended to make some quick stats and comparisons for berries(blueberries,strawberries and raspberries) in United States."),
                        h4(p("These data were collected from the",a("USDA database selector",href="https://quickstats.nass.usda.gov"))),
                        h4(p("The dataset we use here is from",a("this webpage,",href="https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1"),"downloaded as CSV file")),
                        hr(),
                        h5("Built with",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                           "by",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                           "."))
             )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    BerryFinder1<-reactive({
        req(input$TypeFinder1)
        req(input$StateFinder1)
        req(input$YearFinder1)
        req(input$ChemFinder1)
        filter(sberry,Status %in% input$TypeFinder1)%>%
            filter(State %in% input$StateFinder1)%>%
            filter(Year >= input$YearFinder1[1],Year<=input$YearFinder1[2])%>%
            filter(Chem %in% input$ChemFinder1)
    })
    BerryFinder2<-reactive({
        req(input$TypeFinder2)
        req(input$StateFinder2)
        req(input$YearFinder2)
        req(input$ChemFinder2)
        filter(sberry,Status %in% input$TypeFinder2)%>%
            filter(State %in% input$StateFinder2)%>%
            filter(Year >= input$YearFinder2[1],Year<=input$YearFinder2[2])%>%
            filter(Chem %in% input$ChemFinder2)
    })
    
    output$a_Table<-renderDataTable({
        datatable(BerryFinder1())
    })
    output$b_Table<-renderDataTable({
        datatable(BerryFinder2())
    })
    output$a_Hist<-renderPlot({
        ggplot(BerryFinder1(),aes(x=killer,color=Chem,fill=Chem))+geom_histogram(position="identity",stat="count")
    }
    )
    output$b_Hist<-renderPlot({
        ggplot(BerryFinder2(),aes(x=killer,color=Chem,fill=Chem))+geom_histogram(position="identity",stat="count")
    }
    )
    output$a_Pie<-renderPlot({
        n<-length(BerryFinder1()[1])*100
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
        ggplot(BerryFinder1(),aes(x="killer",color=Chem,fill=killer))+geom_bar(stat="count",position="stack",width=1)+
            coord_polar(theta="y",start=0)+labs(x='',y='',title='')+geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")
    })
    output$b_Pie<-renderPlot({
        n<-length(BerryFinder2()[1])*100
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
        ggplot(BerryFinder2(),aes(x="killer",color=Chem,fill=killer))+geom_bar(stat="count",position="stack",width=1)+
            coord_polar(theta="y",start=0)+labs(x='',y='',title='')+geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
