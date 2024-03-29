ui <- fluidPage(theme = shinytheme("readable"),
  titlePanel("Shakespeare's Plays Word Frequencies") ,# Application title
  
  sidebarLayout(
    
    sidebarPanel(selectInput("choice", "Choose a book",books),
    checkboxInput("stopwords",'Stop Words',value=TRUE),
    actionButton("actionbutton","Rerun"),
    hr(),
    h3("Word Cloud Settings"),
    sliderInput("maxwords","Max # of Words", min = 10,max=200,value=100,step=10),
    sliderInput("largestwords","Sizeo of largest Words",min=1,max=8,value=4),
    sliderInput("smallestwords","Size of smallest words", min = 0.1,max = 4,value=0.5),
    hr(),
    h3("Word Count Settings"),
    sliderInput("minwordcount","Min Words for counts chart", min = 10, max = 100, value = 25),
    sliderInput("wordsize","Word Size for counts chart",min = 8, max = 30, value = 14)
    ),
    
    mainPanel(
      tabsetPanel(tabPanel("Word Cloud",plotOutput("cloud",height = "700px",width='700px')),
                  tabPanel("Word Count",plotOutput("freq",height = "700px",width='700px')))
      
    )
  )
  
)

