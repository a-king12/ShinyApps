ibrary(shiny)
library(readr)
library(tidyverse)
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
library(shinycssloaders)

# Reading .csv file for beer data set
beers <- read.csv("data/result.csv")

# Removing "X.__" from each column name
beers <- beers %>% rename_at(vars(starts_with("X.__")), funs(str_replace(., "X.__", "")))
# Renaming "X." to Additions
beers <- rename(beers, replace = c("X." = "addition"))
# Sorting dataset to be able to use it
beers <- melt(beers, id=c("addition"))
# Renaming "variable" to "beers" 
beers <- rename(beers, replace = c("variable" = "beer"))
# Replacing the "." in between all instance with " "
beers$beer <- gsub(".", " ", beers$beer, fixed = TRUE)
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
    menuItem("Survey", icon = icon("poll"), tabName = "surveyTab"),
    menuItem("Link to code files", tabName="link", (href="https://www.github.com"), icon=icon("code"))
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
      h2("Its the simple things in life, life beer, that need an app.", 
      style = "font-family: 'Bebas Neue';
      font-weight: 100;letter-spacing: 2px; font-size:15px; line-height: 1.3;color:orange; 
      text-align:center;"), br(),
      p('Beersgalore is an new and improved app you never knew you needed. This app will help you make data driven decisions on a new beer recipie.
      We make it incredibly easy for you for to chose the best beer additions that pair well with many types beer.'), br(), 
      p('"How"? You ask?!
      We took data from a survey from WhatTheBrew.com that is home to almost 100 different types of beer and over 40 different additions and the survey results from real people. 
      '), 
      style = "font-family: 'Bebas Neue';
      font-weight: 100;letter-spacing: 1.6px; font-size:13px; line-height: 1.3;color:white; 
      text-align:center;", br(),br(),br(),
      box(title="Created by:",status = "warning",background = "black",solidHeader = TRUE,width=NULL,
       "Alexia King",br(), "Elvis Dang", br(),"Christy Hoke",br(),"Lavanya Bommareddy"
       )),
              
            )
    ),
    tabItem(tabName = "additionsTab",
    #Section for user to select their flavor chocie
    column(width=12, fluidRow(
      box(title = "Flavors on flavors",width=6,background='orange',
      h3(),
      selectInput("flavors", "Choose any flavor to see the top 10 beeers based on votes:", beers$addition)
         )
          ),
    #Place holder for the bar chart of the top 10 beers  
     fluidRow(
       box(background="black", plotOutput("flav", width = "600px", height = "600px"))
          )
          )),
    tabItem(tabName = "link",
    ),
    # Plots and Input that will be in the Beer menu
    tabItem(
      tabName = "beerTab",
      # Beer drop down list and slider bar for the amount of addition that the user will want to see        
      box(title = "Beer Seletion",
          h3(),width = 12,
          selectInput("beerSlider", "Choose a beer:", beers$beer),
          sliderInput(inputId = "topFlavors", "Show how many top flavors:", min = 1, max = 45, value = 15),
          
      ),
      # The table for the addition and the popularity of the addition with the beer of choice
      fluidRow(
        dataTableOutput("table", width = "100%", height = "auto") %>% withSpinner(color="#0dc5c1")
      ),
      # Plots
      fluidRow(
        # Plot to visual the comparesion of likeness of the addition with the beer of choice
        box(width = 6,plotOutput("bar", width = "auto", height = "600px") %>% withSpinner(color="#0dc5c1")),
        # Plot of trends the addition has
        box(width = 6,plotOutput("trends", width = "auto", height = "600px") %>% withSpinner(color="#0dc5c1"))
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

shinyApp(ui = ui, server = server)
