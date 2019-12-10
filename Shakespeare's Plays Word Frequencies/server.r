server <- function(input, output) {

  freq <-eventReactive(input$choice, {withProgress({
    setProgress(message = "Processing corpus...")
    getFreq(input$choice,input$stopwords) 
  })
  })
  
 output$cloud <- renderPlot({v <- freq()
 pal <- brewer.pal(8,"Dark2")

 v %>% 
   with(
     wordcloud(
       word, 
       n, 
       scale = c(input$largestwords, input$smallestwords),
       random.order = FALSE, 
       max.words = input$maxwords, 
       colors=pal))  
 })
 
 output$freq <- renderPlot({v<-freq()
 
 barCount <- v %>% filter(n >= input$minwordcount)%>% ggplot(aes(x=reorder(word,n), y=n))+geom_bar(stat = "identity")+coord_flip()+ 
   theme(text=element_text(size=input$wordsize),axis.title.x=element_blank(),axis.title.y=element_blank())
 
 barCount
 
 })
 
}
