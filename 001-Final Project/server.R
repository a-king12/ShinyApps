# Server 
server <- function(input, output) {
  
  # # Interactive bar chart on the flavors tab to show the top choices of beer per flavor
  # output$flav <- renderHighchart({ highchart() %>% hc_add_series(beers %>% filter(addition== input$flavors),
  #                                                                type="bar",hcaes(x=beer,y=value*100),tooltip=list(pointFormat = "{point.beer}: {point.value}"),showInLegend=F) %>%
  #     hc_add_series(beers %>% filter(addition== input$flavors),type='bubble',hcaes(x=beer,y=value*100,size=value),
  #                   tooltip=list(pointFormat = "{point.beer}: {point.value}"),showInLegend=F,marker=list(fillOpacity=1),minSize=3,
  #                   maxSize=5) %>%
  #     hc_plotOptions(column = list(pointWidth = .5, pointPlacement = "on")) %>%
  #     hc_xAxis(title = list(text = "List of Beers")) %>%
  #     hc_yAxis(title = list(text = "Likliness by percentage", 
  #                           style = list(color = "white", 
  #                                        fontSize = 13)), 
  #              labels = list(style = list(color = "white", fontSize = 11)),
  #              tickColor = "white", 
  #              gridLineColor = "transparent") %>%
  #     hc_colors(color = "orange") %>%
  #     hc_xAxis(labels = list(enabled = F), gridLineColor = "transparent")%>%
  #     hc_chart(backgroundColor = 'transparent')
  #   
  # })
  
  # the table of the addition and it's popularity
  output$flavortable <- DT::renderDataTable({
    forFlavTable <- topflavor() %>% select(-c(addition)) %>% reshape::rename(replace = c("value" = "Likeness(Highest = 100%)") %>% reshape::rename(replace = c("addition" = "Flavors")))
    datatable(forFlavTable, caption = "    Top Flavors") %>% formatPercentage('Likeness(Highest = 100%)')  
  })
  
  
  # Reactive function to filter the choice of beer and amount of top addition the user wanted to see
  topBeers <- reactive({
    input$update
    filterBeers <- beers %>% filter(beer == input$beerSlider) %>% arrange(desc(value)) %>% top_n(n = input$topFlavors)
  })  
  topflavor <- reactive({
    filterFlavor <- beers %>% filter(addition== input$flavors)
    filterFlavor %>% arrange(desc(value)) %>% top_n(n = 15)
  }) 
  
  topflavor_1 <- reactive({
    filterFlavor <- beers %>% filter(addition== input$flavors)
    filterFlavor %>% arrange(desc(value)) %>% top_n(n = 1)
  }) 
  
  # the bar plot for the beer menu
  output$bar <- renderPlotly({
      heatColors <- heat.colors(input$topFlavors, 1, rev = TRUE)
      input$update
      popBar <- topBeers() %>%ggplot(aes(x= reorder(addition,value), y = value)) + 
      geom_bar(stat = "identity",show.legend = FALSE, fill = heatColors) + 
      coord_flip() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            axis.text.x = element_text(colour = "Orange", size = 16),
            axis.ticks.x = element_line(colour = "Orange"),
            axis.text.y = element_text(colour = "Orange", size = 12),
            axis.ticks.y = element_line(colour = "Orange"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "black",colour = NA),
            plot.background = element_rect(fill = "black",colour = NA)) +
      scale_y_continuous(breaks=c(0, max(topBeers()$value)),
                         labels=c("Least liked", "Most Liked"))
      popBar <- popBar + labs(x = "Flavor",y="Likeness")
      
      ggplotly(popBar)
  })
  # The plot of trends of the additions
  output$trends <- renderPlot({
    topAdditions <- topBeers() %>% select(-c("beer","value"))
    topAdditions <- data.frame(lapply(topAdditions, as.character), stringsAsFactors=FALSE)
    choice1 <- topAdditions[1,1]
    choice2 <- topAdditions[2,1]
    choice3 <- topAdditions[3,1]
    choice4 <- topAdditions[4,1]
    choice5 <- topAdditions[5,1]
    if(input$topFlavors >= 5){
      trend <- gtrends(c(choice1, choice2, choice3,choice4, choice5), geo="US", time = "2018-01-01 2019-01-01") 
    }
    else if(input$topFlavors == 4){
      trend <- gtrends(c(choice1, choice2, choice3, choice4), geo="US", time = "2018-01-01 2019-01-01") 
    }
    else if(input$topFlavors == 3){
      trend <- gtrends(c(choice1, choice2, choice3), geo="US", time = "2018-01-01 2019-01-01") 
    }
    else if(input$topFlavors == 2){
      trend <- gtrends(c(choice1, choice2), geo="US", time = "2018-01-01 2019-01-01") 
    } 
    else{
      trend <- gtrends(c(choice1), geo="US", time = "2018-01-01 2019-01-01") 
    }
    plot(trend) + theme(title = element_text("Internet Search Trends of Top Flavors", size = 14, color = 'orange'),plot.background = element_rect(fill = 'black'),axis.text.x = element_text(colour = "Orange", size = 16),
                                  axis.ticks.x = element_line(colour = "Orange"),
                                  axis.text.y = element_text(colour = "Orange", size = 12),
                                  axis.ticks.y = element_line(colour = "Orange"),
                                  panel.background = element_rect(fill = 'black'),
                                  panel.grid.major = element_blank(),
                                  legend.background = element_rect(fill = 'orange'),
                                  legend.key.height = unit(7,"line")) + 
                                  ggtitle("Internet Search Trends for Top Flavors")
  })
  # the table of the addition and it's popularity
  output$table <- DT::renderDataTable({
    forTable <- topBeers() %>% select(-c(beer)) %>% reshape::rename(replace = c("value" = "Likeness(Highest = 100%)") %>% reshape::rename(replace = c("addition" = "Flavors")))
    datatable(forTable, caption = "    Top Flavors") %>% formatPercentage('Likeness(Highest = 100%)') 
  })
  
  output$trends_flavor <- renderPlot({
    topFlavors_tab <- topflavor_1()  %>% select(-c("beer","value"))
    topFlavors_tab <- data.frame(lapply(topFlavors_tab, as.character), stringsAsFactors=FALSE)
    choice1 <- topFlavors_tab[1,1]
trend_flavor <- gtrends(c(choice1), geo="US", time = "2018-01-01 2019-01-01")
plot(trend_flavor) + theme(title = element_text("Internet Search Trends of flavor", size = 14, color = 'orange'),plot.background = element_rect(fill = 'black'),axis.text.x = element_text(colour = "Orange", size = 16),
                        axis.ticks.x = element_line(colour = "Orange"),
                        axis.text.y = element_text(colour = "Orange", size = 12),
                        axis.ticks.y = element_line(colour = "Orange"),
                        panel.background = element_rect(fill = 'black'),
                        panel.grid.major = element_blank(),
                        legend.background = element_rect(fill = 'orange'),
                        legend.key.height = unit(7,"line")) + 
      ggtitle("Internet Search Trends for Chosen Flavor")
  })
  
}
  
}
