library(sf)
library(plotly)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(knitr)
library(tidygraph)
library(ggraph)
#library(reldist)
#library(quanteda)



### ---------------- what ----------------
#' @title  Compute the average salience of the topic
#' @name what
#' @description create a table and graphic of the topic
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param title Title of the graphic


what <- function (hc = hypercube,
                  subtop = NA,
                  title = "What ?")
{
 
  
tab<-hc
if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}

tab<-tab[,list(news = sum(news)),by = what]
tab$pct<-100*tab$news/sum(tab$news)

p <- plot_ly(tab,
             labels = ~what,
             values = ~pct,
             type = 'pie') %>%
  layout(title = title,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

output<-list("table" = tab, "plotly" =p)

return(output)

}


### ---------------- testchi2 ----------------
#' @title  Compute the average salience of the topic
#' @name what
#' @description create a table and graphic of the topic
#' @param tabtest a table with variable trial, success and null.value
#' @param cut_break : thresholds of p.value
#' @param cut_names : semantic associated to breaks
#' @param mintest : Threshold of estimated value requested for chi-square test


testchi2<-function(tabtest=tabtest,
                   cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                   cut_names = c("---","--","-","n.s.","+","++","+++"),
                   mintest = mintest
) {
  tab<-tabtest
  n<-dim(tab)[1]
  tab$estimate<-round(tab$success/tab$trial,5)
  tab$salience<-log(tab$estimate/tab$null.value)/log(2)
  tab$chi2<-NA
  tab$p.value<-NA
  for (i in 1:n) {
    if(tab$trial[i]*tab$null.value[i]>=mintest) {  
      test<-prop.test(x=tab$success[i],n=tab$trial[i], p=tab$null.value[i], alternative = "less")
      tab$chi2[i]<-round(test$statistic,2)
      tab$p.value[i]<-round(test$p.value,5)
     } 
  }
  tab$signif<-cut(tab$p.value, breaks=cut_breaks,include.lowest = T)
  levels(tab$signif)<-cut_names
  tab$signif<-as.character(tab$signif)
  tab$signif[is.na(tab$signif)]<-"n.s."
  tab$signif<-as.factor(tab$signif)
  return(tab)
}
#### ---------------- who.what ----------------
#' @title  visualize variation of the topic between media
#' @name who.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


who.what <- function (hc = hypercube,
                      test = FALSE,
                      subtop = NA,
                      mintest = 5,
                      title = "Who says What ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(who)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}
  tab<-tab[tab$trial*tab$null.value>mintest,]
  
  p <- plot_ly(tab,
               x = ~who,
               y = ~estimate*100,
               color= ~index,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Source: ',who,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),'%',  
                             '<br /> p.value : ', round(p.value,4)),
               type = "bar")  %>%
    layout(title = title,
           yaxis = list(title = "% news"),
           barmode = 'stack')
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}




#### ---------------- when.what ----------------
#' @title  visualize variation of the topic through time
#' @name when.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


when.what <- function (hc = hypercube,
                       test = FALSE,
                       subtop = NA,
                       mintest = 5,
                       title = "When said What ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  tab<-tab[is.na(when)==F,]
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(when)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  tab<-tab[tab$trial*tab$null.value > mintest,]
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}

  
  p <- plot_ly(tab,
               x = ~as.character(when),
               y = ~estimate*100,
               color = ~index,
               #  color=~signif,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Source: ',when,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),'%',  
                             '<br /> p.value : ', round(p.value,4)),
               type = "bar")  %>%
    layout(title = title,
           #        xaxis = list(xlim = c(min(tab$when),max(tab$when))),
           yaxis = list(title = "% news"),
           barmode = 'stack')
  
  
  output<-list("table" = tab, "plotly" =p)
  
  
  
}




#### ---------------- where.what ----------------
#' @title  visualize spatialization of the topic 
#' @name where.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param map a map with coordinates in lat-long
#' @param proj a projection accepted by plotly
#' @param title Title of the graphic


where.what <- function (hc = hypercube,
                        test = TRUE,
                        subtop = NA,
                        mintest = 5,
                        map = world_ctr,
                        proj = 'azimuthal equal area',
                        title = "Where said What ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  
  tab<-tab[,list(trial = round(sum(news),0),success=round(sum(news*what),0)),by = list(where1)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  tab<-tab[tab$trial*tab$null.value > mintest,]
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}
 
  
  tab<-tab[order(-chi2),]
  
  
  
  map<-merge(map,tab,all.x=F,all.y=F,by.x="ISO3",by.y="where1")
  
  
  #map2<-map[is.na(map$pct)==F,]
  #map2<-st_centroid(map2)
  #map2<-st_drop_geometry(map2)
  
  
  g <- list(showframe = TRUE,
            framecolor= toRGB("gray50"),
            coastlinecolor = toRGB("gray50"),
            showland = TRUE,
            landcolor = toRGB("gray90"),
            showcountries = TRUE,
            countrycolor = toRGB("white"),
            countrywidth = 0.2,
            projection = list(type = proj))
  
  
  
  p<- plot_geo(map)%>%
    add_markers(x = ~lon,
                y = ~lat,
                sizes = c(0, 250),
                size = ~success,
                #             color= ~signif,
                color = ~index,
                colors= mycol,
                hoverinfo = "text",
                text = ~paste('Location: ',NAME,
                              '<br /> Total news  : ', round(trial,0),
                              '<br /> Topic news : ', round(success,0),
                              '<br /> % observed  : ', round(estimate*100,2),'%',
                              '<br /> % estimated : ', round(null.value*100,2),'%',
                              '<br /> Salience : ', round(salience,2),'%',  
                              '<br /> p.value : ', round(p.value,4)) ) %>%
    
    layout(geo = g,
           title = title)
  
  p
  
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}



#### ---------------- when.who.what ----------------
#' @title  visualize variation of the topic by media through time
#' @name when.who.what
#' @description create a table of variation of the topic by media through time
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


when.who.what <- function (hc = hypercube,
                           test=TRUE,
                           subtop = NA,
                           mintest = 5,
                           title = "What by Whom and When ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  tab<-tab[is.na(when)==F,]
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(when,who)]
  ref<-tab[,list(null.value = round(sum(success)/sum(trial),4)), by = list(who)]
  tab<-merge(tab,ref,by="who")
  tab<-tab[tab$trial*tab$null.value>mintest,]
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}

  
  p <- plot_ly(tab,
               x = ~when,
               y = ~who,
               z= ~index,
               sizes = c(0, 250),
               size = ~success,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Date: ',when,
                             '<br>Media: ',who,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),'%',  
                             '<br /> p.value : ', round(p.value,4)),
               #  name = ~tags,
               type = "heatmap")  %>%
    layout(title = title,
           yaxis = list(title = "media"),
           xaxis = list(title = "time"))
  p
  
  
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}


#### ---------------- where.who.what ----------------
#' @title  visualize variation of the topic by location and media
#' @name where.who.what
#' @description create a table of variation of the topic by location and media
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param maxloc maximum number of location
#' @param mintest sample size of estimate for chi-square test
#' @param title Title of the graphic


where.who.what <- function (hc = hypercube,
                            test = TRUE,
                            subtop = NA,
                            maxloc = 15,
                            mintest = 5,
                            title = "What by Whom and Where ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1,who)]
  ref<-tab[,list(null.value = round(sum(success)/sum(trial),4)), by = list(who)]
  tab<-merge(tab,ref,by="who")

  
  # selection
  sel<-tab[,list(success = sum(success)), by = list(where1)]
  sel<-sel[order(-success),]
  sel<- sel[1:maxloc,]
  tab<-tab[where1 %in% sel$where1,]
  tab$trial<-round(tab$trial,0)
  tab$success<-round(tab$success,0)
  tab<-tab[tab$trial*tab$null.value>mintest,]
  
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}

  
  
  
  
  p <- plot_ly(tab,
               x = ~where1,
               y = ~who,
               z= ~index,
               sizes = c(0, 250),
               size = ~success,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Location: ',where1,
                             '<br>Media: ',who,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),'%',  
                             '<br /> p.value : ', round(p.value,4)),
               #  name = ~tags,
               type = "heatmap")  %>%
    layout(title = title,
           yaxis = list(title = "Host media"),
           xaxis = list(title = "Guest countries"))
  
  
  
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}

#### ---------------- when.where.what ----------------
#' @title  visualize variation of the topic by location through time
#' @name when.where.what
#' @description create a table of variation of the topic by location through time
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param maxloc maximum number of location
#' @param mintest minimum expected size of news for test
#' @param title Title of the graphic


when.where.what <- function (hc = hypercube,
                             test = TRUE,
                             subtop = NA,
                             maxloc = 15,
                             mintest = 5,
                             title = "What, Where and When ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1,when)]
  ref<-tab[,list(null.value = round(sum(success)/sum(trial),4)), by = list(where1)]
  tab<-merge(tab,ref,by="where1")

  
  # selection
  sel<-tab[,list(success = sum(success)), by = list(where1)]
  sel<-sel[order(-success),]
  sel<- sel[1:maxloc,]
  tab<-tab[where1 %in% sel$where1,]
  tab$trial<-round(tab$trial,0)
  tab$success<-round(tab$success,0)
  tab<-tab[tab$trial*tab$null.value>mintest,]
  
  
  
  tab<-testchi2(tabtest=tab,
                cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                cut_names = c("---","--","-","n.s.","+","++","+++"),
                mintest = mintest)
  
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  if (test==FALSE) {tab$index =tab$salience} else {tab$index=tab$p.value}

  
  p <- plot_ly(tab,
               x = ~when,
               y = ~where1,
               z= ~index,
               sizes = c(0, 250),
               size = ~success,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Location: ',where1,
                             '<br>Date: ',when,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),'%',  
                             '<br /> p.value : ', round(p.value,4)),
               #  name = ~tags,
               type = "heatmap")  %>%
    layout(title = title,
           yaxis = list(title = "Guest countries"),
           xaxis = list(title = "Time"))
  
  p
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
}


#### ---------------- where.where.what ----------------
#' @title  visualize variation of the topic by co-location 
#' @name where.where.what
#' @description create a table of variation of the topic by co-location
#' @param hc an hypercube prepared as data.table
#' @param subtop a subtag of the main tag (default = NA)
#' @param mintest minimum expected size of news for test
#' @param minedge minimum news with topic by edge
#' @param minnode minimum news with topic by node
#' @param title Title of the graphic


where.where.what <- function (hc = hypercube,
                              subtop = NA,
                              mintest = 5,
                              minedge = 2,
                              minnode = 10,
                              title = "What, Where and Where ?")
{
  
  tab<-hc
  if (is.na(subtop)){tab$what <-tab$what !="_no_"}else {tab$what <- tab$what == subtop}
  
  # Palette
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  
  # Create edges
  tab1<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1,where2)]
  tab1$null.value<-sum(tab1$success)/sum(tab1$trial)
  tab1<-testchi2(tabtest=tab1,
                 cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                 cut_names = c("---","--","-","n.s.","+","++","+++"),
                 mintest = mintest)
  tab1<-tab1[order(success),]
  
  # Create nodes
  tab2<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1)]
  tab2$null.value<-sum(tab2$success)/sum(tab2$trial)
  tab2<-testchi2(tabtest=tab2,
                 cut_breaks = c(0,0.0001,0.001,0.05, 0.95,0.999,0.9999,1),
                 cut_names = c("---","--","-","n.s.","+","++","+++"),
                 mintest = mintest)
  tab2<-tab2[order(success),]
  
  
  # Build tibble graph object
  tib_g=tidygraph::tbl_graph(nodes=tab2,edges=tab1) 
  
  # filter
  sel_tib_g <-tib_g %>% activate(edges) %>%
    filter(success > minedge) %>%
    activate(nodes) %>%
    filter(success > minnode) %>%
    mutate(isol = node_is_isolated()) %>%
    filter(isol == FALSE)
  
  ## Create a ggraph layout
  g=sel_tib_g %>% 
    ggraph(layout="kk")
  
  # visualize
  gg<-g + geom_edge_link(aes(edge_width=success, edge_colour = signif),
                         alpha = 0.3 , show.legend=c(TRUE,FALSE,FALSE, FALSE,FALSE)) +
    geom_node_point(aes(colour = signif, size=success), 
                    alpha=0.6) +
    geom_node_label(aes(label = where1, size = 2*sqrt(success)),alpha =1,label.size=0.1)+
    scale_edge_color_manual(values=c("---"=mycol[1],
                                     "--" = mycol[2],
                                     "-" = mycol[3],
                                     "n.s." = mycol[4],
                                     "+" = mycol[5],
                                     "++" = mycol[6],
                                     "+++" = mycol[7])) +
    scale_color_manual( values=c(      "---"=mycol[1],
                                       "--" = mycol[2],
                                       "-" = mycol[3],
                                       "n.s." = mycol[4],
                                       "+" = mycol[5],
                                       "++" = mycol[6],
                                       "+++" = mycol[7])) 
  
  output<-list("table" = tab1, "plot" =gg)
  
  return(output)
}
