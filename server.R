library(shiny)
#source("hypercube_visualisation_functions_V2.R")
#don<-readRDS("venise_climat_state_day_hypercube.rda")




# Define server logic
shinyServer(function(input, output) {


  cube<- reactive({
   # hc<-dts[[input$topic]][[input$TimeSpan]]
    hc<-dts[[input$topic]]
    hc<-hc[substr(hc$who,4,6) %in% input$host,]
    if (input$TimeSpan != "week"){ hc$when<-as.Date(cut(hc$when, input$TimeSpan, start.on.monday = TRUE))
                                hc<-hc[, list(news=sum(news)),by =list(order,who, when, where1, where2, what)]}
 #   hc<-dts[[input$TimeSpan]]
    if(input$Media=="who_state"){hc$who <-substr(hc$who,4,6)} else {hc$who <-substr(hc$who,4,12)}
#    hc$tags<-hc$what!="_no_"
#    if (input$subtopic != "_no_"){hc$tags<-hc$what == input$subtopic}
#    hc$weight<-hc[[input$wgt]]
    hc<-hc[(hc$order <= max(input$textsize)),]
    hc<-hc[(hc$order >= min(input$textsize)),]
    hc<-hc[as.Date(when) >= min(as.Date(input$timeperiod)),]
    hc<-hc[as.Date(when) <= max(as.Date(input$timeperiod)),]
    hc<-hc[order(hc$when),]
 #   hc$where<-hc$where1
    return(hc)
  })

#  output$timePlot <- renderPlotly({

#  })

#  output$what <-renderPlotly({    what(hc= cube(),
                                    #   subtop = input$subtopic,
#                                       title = "Salience ?")}$plotly)

  output$who.what <- renderPlotly({who.what(hc = cube(),
                                            test = input$test,
                                            minsamp = input$minsamp,
                                            mintest = input$mintest,
                                            title = "Variation by media")}$plotly)



  output$when.what <- renderPlotly({when.what(hc = cube(),
                                              test = input$test,
                                              minsamp = input$minsamp,
                                              mintest = input$mintest,
                                              title = "Variation through time")}$plotly)

  output$when.who.what <- renderPlotly({when.who.what(hc = cube(),
                                                      test = input$test,
                                                      minsamp = input$minsamp,
                                                      mintest = input$mintest,
                                                      title = "Variation by media through time")}$plotly)

  output$where.what <- renderPlotly({where.what(hc = cube(),
                                                test = input$test,
                                                minsamp = input$minsamp,
                                                mintest = input$mintest,
                                                map =map,
                                                title = "Map of guest countries")}$plotly)

  output$when.where.what <- renderPlotly({when.where.what(hc = cube(),
                                                          test = input$test,
                                                          minsamp=input$minsamp,
                                                          mintest = input$mintest,
                                                          maxloc = input$maxloc,
                                                          title = "Variation of guest countries throug time")}$plotly)

  output$where.who.what <- renderPlotly({ where.who.what(hc = cube(),
                                                         test = input$test,
                                                         minsamp=input$minsamp,
                                                         mintest = input$mintest,
                                                         maxloc = input$maxloc,
                                                         title = "Variation of guest countries by media")}$plotly)

  output$where.where.what <- renderPlot({ where.where.what(hc = cube(),
                                                         test=input$test,
                                                         minsamp=input$minsamp,
                                                         mintest = input$mintest,
                                                         minedge = input$minedge,
                                                         minnode = input$minnode,
                                                         title = "Network of guest countries")}$plot)

})

