library(shiny)
library(readr)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(plyr)
library(reshape)
library(gtrendsR)
library(reshape2)
library(shinythemes)
library(shinydashboard)
library(highcharter)
library(shinyWidgets)
library(gganimate)
library(DT)
library(xlsx)
library(plotly)
library(shinycssloaders)
library(grDevices)

# Reading .csv file for beer data set
beers <- read.csv("data/result.csv")

# Removing "X.__" from each column name
beers <- beers %>% rename_at(vars(starts_with("X.__")), funs(str_replace(., "X.__", "")))
# Renaming "X." to Additions
beers <- reshape::rename(beers, replace = c("X." = "addition"))
# Sorting dataset to be able to use it
beers <- melt(beers, id=c("addition"))
# Renaming "variable" to "beers" 
beers <- reshape::rename(beers, replace = c("variable" = "beer"))
# Replacing the "." in between all instance with " "
beers$beer <- gsub(".", " ", beers$beer, fixed = TRUE)
# Capitalizing the first letters for each addition
cap <- function(data) {
  paste(toupper(substring(data, 1, 1)), substring(data,2), sep = "")
}
beers$addition <- cap(beers$addition)
# Taking a look at the dataset
#glimpse(beers)
#view(beers)


# Sidebar menu for the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Home Menu
    menuItem("Home", tabName = "home", icon = icon("home")),
    # Beers Menu
    menuItem("Beers", icon = icon("beer"), tabName = "beerTab"),
    # Addition Menu
    menuItem("Flavors", icon = icon("plus"), tabName = "additionsTab"),
    # Survey Menu
    menuItem("Survey", icon = icon("poll"), tabName = "surveyTab")
  )
)

tags$head(
  tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
)

# the body of each sidebar menu
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            #creats custom CSS for the home page
            fluidRow(
              column(width = 9,class = 'Center',tags$head(tags$style(HTML('
       /* logo */
       .skin-blue .main-header .logo {
       background-color: #000000;
       }
      /* logo when hovered */
       .skin-blue .main-header .logo:hover {
       background-color: #B9B9B9;
      }
       /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #000000;
        }
    /* main sidebar */
    .skin-blue .main-sidebar {
    background-color: #CE7209;
    }
    /* other links in the sidebarmenu when hovered */
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    background-color: #000000;
    }
    /* body */
    .content-wrapper, .right-side {
    background-color: #000000;
    }'))),
                     #text for the homepage
                     h1("Welcome to Beersgalore",style = "font-family: 'Audry'; letter-spacing: 2px; color:white; text-align:center"),
                     br(),
                     tags$img(src='beerpic.jpg', height=350, width=500,style="display: block; margin-left: auto; margin-right: auto;opacity: 0.9;"),
                     br(),
                     
                     h2('"Fill your belly. Day and Night. Make Merry" - The Epic of Gilgamesh, Sumerian Poem (c.2100 B.C)', 
                        style = "font-family: 'Bebas Neue';
      font-weight: 100;letter-spacing: 2px; font-size:15px; line-height: 1.3;color:orange; 
      text-align:center;"), br(),
                     p('Beer is one of the oldest brewed beverages in the world. If you are asking yourself "How can we brew a better and more consistent beer?" 
                       then let us help you. 
                       With Beergalore you can analyze your beer data and brewing data in real-time. 
                       You can then visualize this data on the app which can help you cut out the waste and boost profits. 
                       With Beergalore you can make data-driven decisions on a new beer recipe.
                       We are making it incredibly easy for you to choose the best beer additions that pair well with many styles of beers.'), br(), 
                     p('"How"? You ask?!
      We took data from a survey from WhatTheBrew.com which is home to almost 100 different types of beer and over 40 different additions. Additionally, the survey results are from real people. '), 
                     style = "font-family: 'Bebas Neue';
      font-weight: 100;letter-spacing: 1.6px; font-size:13px; line-height: 1.3;color:white; 
      text-align:center;", br(),br(),br(),
                     box(title="Created by:",status = "warning",background = "black",solidHeader = TRUE,width=NULL,
                         "Alexia King",br(), "Elvis Dang", br(),"Christy Hoke",br(),"Lavanya Bommareddy"),
                     actionButton(inputId='ab1', label=h6("Link to gitHub"), 
                                  icon = icon("code"), 
                                  onclick ="window.open('https://github.com/a-king12/ShinyApps/tree/master/001-Final%20Project', '_blank')")
              )),
            
            
    ),
    # Plots and Input that will be in the Beer menu
    tabItem(
      tabName = "beerTab",
      # Beer drop down list and slider bar for the amount of addition that the user will want to see        
      box(title = "Beer Seletion", background = 'black',
          h3(),width = 12,
          selectInput("beerSlider", "Choose a beer:", beers$beer,multiple = FALSE, selected = 1),
          setSliderColor(c("orange"),c(1)),
          sliderInput(inputId = "topFlavors", "Show how many top flavors:", min = 1, max = 45, value = 15)
          ),
      # Plots
      fluidRow(
        # Plot to visual the comparesion of likeness of the addition with the beer of choice
        box(width = 6,background= 'black',plotlyOutput("bar", width = "auto", height = "600px") %>% withSpinner(type=5,color="#CE7209")),
        # Plot of trends the addition has
        box(width = 6,background= 'black',plotOutput("trends", width = "auto", height = "600px") %>% withSpinner(type=5,color="#CE7209"))
      ),
      # The table for the addition and the popularity of the addition with the beer of choice
      fluidRow(
        dataTableOutput("table", width = "100%", height = "auto") %>% withSpinner(type=5,color="#CE7209")
      ),
      
      
    ),
    tabItem(tabName = "additionsTab",
            #Section for user to select their flavor chocie
            box(width=9,background='black',
                h3(),
                selectInput("flavors", 
                            label= h6("Choose any flavor to see the google search trends and top 15 beers:",multiple = FALSE), 
                            choices= beers$addition,
                            selected = "apple")
            ),
            #Place holder for the bar chart of the beers after user selects flavor and for the data table  
            fluidRow(
              box(width = "10",background= 'black',plotOutput("trends_flavor", width = "auto", height = "600px") %>% withSpinner(type=5,color="#CE7209")),
              dataTableOutput("flavortable", width = "auto", height = "auto") %>% withSpinner(type=5,color="#CE7209"),
              #highchartOutput("flav", width = "auto", height = "600px")%>% withSpinner(type=5,color="#CE7209"),
              )
    ),
  
    # Plots and Input that will be in the Survey menu
    tabItem(width = 10,
            tabName = "surveyTab",
            textAreaInput("survey", "Please enter your oppinion on our beer below:", ""),
            actionButton("submit", "Submit your response!")
    )
  )
)
