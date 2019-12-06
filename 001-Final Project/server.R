# Server 
server <- function(input, output) {
  # Bar chart on the flavors tab to show the top choices of beer per flavor
  output$flav <- renderPlot({beer_flav() %>%
      ggplot(aes(x= reorder(beer, value), y=value)) + geom_bar(stat = "identity",show.legend = FALSE,color="black")+
      theme(axis.title.x=element_blank(),axis.title.y=element_blank())+ coord_flip()
  })
  # Reactive function to filter the choice of beer and amount of top addition the user wanted to see
  topBeers <- reactive({
    filterBeers <- beers %>% filter(beer == input$beerSlider)
    filterBeers %>% arrange(desc(value)) %>% top_n(n = input$topFlavors)
  })  
  
  #Reactive function to filter for top 10 beers based on flavor  
  beer_flav <- reactive({
      filterFlavors <- beers %>% filter(addition == input$flavors)
      filterFlavors %>% arrange(desc(value)) %>% top_n(n = 10)
 
  })
  # the bar plot for the beer menu
  output$bar <- renderPlot({topBeers() %>%
      ggplot(aes(x= reorder(addition,value), y = value, fill = addition)) + 
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      scale_y_continuous(breaks=c(0,median(topBeers()$value), max(topBeers()$value)),
                         labels=c("Least liked","In the middle", "Most Liked")) 
  })
  # The plot of trends of the additions
  output$trends <- renderPlot({
    trend <- gtrends(c("pumpkin"), geo="US", time = "2018-01-01 2019-01-01")
    plot(trend)
  })
  # the table of the addition and it's popularity
  output$table <- DT::renderDataTable({
    forTable <- topBeers() %>% select(-c(beer)) %>% rename(replace = c("value" = "Likeness By Percentage(0 - 1)"))
    datatable(forTable)
  })
  
}
