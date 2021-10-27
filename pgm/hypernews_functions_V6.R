library(sf)
library(plotly)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(knitr)
library(tidygraph)
library(ggraph)
library(visNetwork)
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

#### ---------------- testchi2 ----------------
#' @title  Compute the average salience of the topic and test significance of deviation
#' @name what
#' @description create a table and graphic of the topic
#' @param tabtest a table with variable trial, success and null.value
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest : Threshold of estimated value requested for chi-square test


testchi2<-function(tabtest=tabtest,
                   minsamp = 20,
                   mintest = 5) 
{
  tab<-tabtest
  n<-dim(tab)[1]
  
  # Compute salience if sample size sufficient (default : N>20)
  tab$estimate <-NA
  tab$salience <-NA
  tab$chi2<-NA
  tab$p.value<-NA
  if (tab$trial > minsamp){ tab$estimate<-round(tab$success/tab$trial,5)
  tab$salience<-tab$estimate/tab$null.value
  
  # Chi-square test if estimated value sufficient (default : Nij* > 5)
  
  for (i in 1:n) {
    if(tab$trial[i]*tab$null.value[i]>=mintest) {  
      test<-prop.test(x=tab$success[i],n=tab$trial[i], p=tab$null.value[i], 
                      alternative = "greater")
      tab$chi2[i]<-round(test$statistic,2)
      tab$p.value[i]<-round(test$p.value,5)
    } 
  }
  }
  return(tab)
}



?prop.test()

#### ---------------- who.what ----------------
#' @title  visualize variation of the topic between media
#' @name who.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


who.what <- function (hc = hypercube,
                      test = FALSE,
                      minsamp = 20,
                      mintest = 5,
                      title = "Who says What ?")
{
  
  tab<-hc
  {tab$what <-tab$what !="_no_"}
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(who)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
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
                             '<br /> Salience : ', round(salience,2),  
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
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


when.what <- function (hc = hypercube,
                       test = FALSE,
                       minsamp = 20,
                       mintest = 5,
                       title = "Who says What ?")
{
  
  tab<-hc
  {tab$what <-tab$what !="_no_"}
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(when)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  p <- plot_ly(tab,
               x = ~as.character(when),
               y = ~estimate*100,
               color= ~index,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Time: ',when,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),  
                             '<br /> p.value : ', round(p.value,4)),
               type = "bar")  %>%
    layout(title = title,
           yaxis = list(title = "% news"),
           barmode = 'stack')
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}


#### ---------------- where.what ----------------
#' @title  visualize spatialization of the topic 
#' @name where.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param map a map with coordinates in lat-long
#' @param proj a projection accepted by plotly
#' @param title Title of the graphic


where.what <- function (hc = hypercube,
                        test = FALSE,
                        minsamp = 20,
                        mintest = 5,
                        map = world_ctr,
                        proj = 'azimuthal equal area',
                        title = "Where said What ?")
{
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  
  tab<-tab[,list(trial = round(sum(news),0),success=round(sum(news*what),0)),by = list(where1)]
  ref <-round(sum(tab$success)/sum(tab$trial),4)
  tab$null.value<-ref
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  
  
  tab<-tab[order(-chi2),]
  
  
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  map<-merge(map,tab,all.x=T,all.y=F,by.x="ISO3",by.y="where1")
  
  
  
  #map2<-map[is.na(map$pct)==F,]
  #map2<-st_centroid(map2)
  #map2<-st_drop_geometry(map2)
  
  
  g <- list(showframe = TRUE,
            framecolor= toRGB("gray20"),
            coastlinecolor = toRGB("gray20"),
            showland = TRUE,
            landcolor = toRGB("gray50"),
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
                              '<br /> Salience : ', round(salience,2),  
                              '<br /> p.value : ', round(p.value,4))) %>%
    
    layout(geo = g,
           title = title)
  
  
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
  
}


#### ---------------- when.who.what ----------------
#' @title  visualize variation of the topic by media through time
#' @name when.who.what
#' @description create a table of variation of the topic by media through time
#' @param hc an hypercube prepared as data.table
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test (default = 5)
#' @param title Title of the graphic


when.who.what <- function (hc = hypercube,
                           test = FALSE,
                           minsamp = 20,
                           mintest = 5,
                           title = "What by Whom and When ?")
{
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  tab<-tab[is.na(when)==F,]
  
  
  
  tab<-tab[,list(trial = sum(news),success=round(sum(news*what),0)),by = list(when,who)]
  ref<-tab[,list(null.value = round(sum(success)/sum(trial),4)), by = list(who)]
  tab<-merge(tab,ref,by="who")
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  
  p <- plot_ly(tab,
               x = ~when,
               y = ~who,
               z= ~index,
               sizes = c(0, 250),
               size = ~success,
               colors= mycol,
               hoverinfo = "text",
               text = ~paste('Date: ',when,
                             '<br> Media: ',who,
                             '<br /> Total news  : ', round(trial,0),
                             '<br /> Topic news : ', round(success,0),
                             '<br /> % observed  : ', round(estimate*100,2),'%',
                             '<br /> % estimated : ', round(null.value*100,2),'%',
                             '<br /> Salience : ', round(salience,2),  
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
#' @param maxloc maximum number of location
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test
#' @param title Title of the graphic


where.who.what <- function (hc = hypercube,
                            maxloc = 15,
                            test = FALSE,
                            minsamp=20,
                            mintest = 5,
                            title = "What by Whom and Where ?")
{
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  
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
  
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  
  
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
                             '<br /> Salience : ', round(salience,2),  
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
#' @param maxloc maximum number of location
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest sample size of estimate for chi-square test
#' @param title Title of the graphic


when.where.what <- function (hc = hypercube,
                             maxloc = 15,
                             test = FALSE,
                             minsamp=20,
                             mintest = 5,
                             title = "What, Where and When ?")
{
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  
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
  
  tab<-testchi2(tabtest=tab,
                minsamp = minsamp,
                mintest = mintest)
  
  
  if (test==FALSE) {tab$index =tab$salience
  tab<-tab[tab$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd")
  } 
  else {tab$index=tab$p.value
  tab<-tab[tab$trial*tab$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  
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
                             '<br /> Salience : ', round(salience,2),  
                             '<br /> p.value : ', round(p.value,4)),
               #  name = ~tags,
               type = "heatmap")  %>%
    layout(title = title,
           yaxis = list(title = "Guest countries"),
           xaxis = list(title = "Time"))
  
  
  output<-list("table" = tab, "plotly" =p)
  
  return(output)
}


#### ---------------- where.where.what ----------------
#' @title  visualize variation of the topic by co-location 
#' @name where.where.what
#' @description create a table of variation of the topic by co-location
#' @param hc an hypercube prepared as data.table
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest minimum expected size of news for test
#' @param minedge minimum news with topic by edge
#' @param minnode minimum news with topic by node
#' @param title Title of the graphic


where.where.what <- function (hc = hypercube,
                              test=FALSE,
                              minsamp = 20,
                              mintest = 5,
                              minedge = 2,
                              minnode = 10,
                              title = "What, Where and Where ?")
{
  
  #test...
  
  
  
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  
  # Palette
  
  
  # Create edges
  tab1<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1,where2)]
  tab1$null.value<-sum(tab1$success)/sum(tab1$trial)
  
  tab1<-testchi2(tabtest=tab1,
                 minsamp = minsamp,
                 mintest = mintest)
  
  if (test==FALSE) {tab1$index =tab1$salience
  tab1<-tab1[tab1$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd") }  else
  {tab1$index=tab1$p.value
  tab1<-tab1[tab1$trial*tab1$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
  
  
  
  tab1<-tab1[order(success),]
  
  # Create nodes
  tab2<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1)]
  tab2$null.value<-sum(tab2$success)/sum(tab2$trial)
  tab2<-testchi2(tabtest=tab2,
                 minsamp = minsamp,
                 mintest = mintest)
  
  if (test==FALSE) {tab2$index =tab2$salience
  tab2<-tab2[tab2$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd") }  else
  {tab2$index=tab2$p.value
  tab2<-tab2[tab2$trial*tab2$null.value>mintest,]
  mycol<-brewer.pal(7,"RdYlBu")
  mycol[4]<-"lightyellow"
  }
  
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
    ggraph(layout="stress")
  
  # visualize
  gg<-g + geom_edge_link(aes(edge_width=success, edge_colour = index),
                         alpha = 0.3 , show.legend=c(TRUE,FALSE,FALSE, FALSE,FALSE)) +
    scale_edge_colour_gradientn(colors = mycol)+
    geom_node_point(aes(colour = index, size=success), 
                    alpha=0.6) +
    scale_color_gradientn(colors =mycol)+
    geom_node_label(aes(label = where1, size = 10*sqrt(success)),alpha =1,label.size=0.1,show.legend = FALSE)
  
  
  output<-list("table" = tab1, "plot" =gg)
  
  return(output)
}



#### ---------------- where.where.what.v2 ----------------
#' @title  visualize variation of the topic by co-location 
#' @name where.where.what
#' @description create a table of variation of the topic by co-location
#' @param hc an hypercube prepared as data.table
#' @param test : visualize test (TRUE) or salience (FALSE)
#' @param minsamp : Threshold of sample size requested for salience computation
#' @param mintest minimum expected size of news for test
#' @param minedge minimum news with topic by edge
#' @param minnode minimum news with topic by node
#' @param title Title of the graphic


where.where.what.v2 <- function (hc = hypercube,
                                 test=FALSE,
                                 minsamp = 20,
                                 mintest = 5,
                                 minedge = 2,
                                 minnode = 10,
                                 title = "What, Where and Where ?")
{
  
  
  
  # hc = hyper$year
  # test=FALSE
  # minsamp = 20
  # mintest = 1
  # minedge = 1
  # minnode = 4
  # title = "What, Where and Where ?"
  
  
  
  
  tab<-hc
  tab$what <-tab$what !="_no_"
  
  # Palette
  
  # Create nodes
  nodes<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1)]
  nodes$null.value<-sum(nodes$success)/sum(nodes$trial)
  nodes <-nodes %>% filter(success > minnode, trial > minsamp)
  nodes<-testchi2(tabtest=nodes,
                  minsamp = minsamp,
                  mintest = mintest)
  
  if (test==FALSE) {nodes$index =nodes$salience
  nodes<-nodes[nodes$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd") }  else
  {nodes$index=nodes$p.value
  nodes<-nodes[nodes$trial*nodes$null.value>mintest,]
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  }
  
  #tab2<-tab2[order(success),]%>% filter(tab2$success>minnode)
  
  
  
  # Create edges
  edges<-tab[where1 !=where2,list(trial = sum(news),success=round(sum(news*what),0)),by = list(where1,where2)]
  edges$null.value<-sum(edges$success)/sum(edges$trial)
  edges <-edges %>% filter(success > minedge, trial > minsamp)
  
  edges<-testchi2(tabtest=edges,
                  minsamp = minsamp,
                  mintest = mintest)
  
  if (test==FALSE) {edges$index =edges$salience
  edges<-edges[edges$trial > minsamp,]
  mycol<-brewer.pal(7,"YlOrRd") }  else
  {edges$index=edges$p.value
  edges<-edges[edges$trial*edges$null.value>mintest,]
  mycol<-rev(brewer.pal(7,"RdYlBu"))
  mycol[4]<-"lightyellow"
  }
  
  
  
  # adjust list of edges to list of nodes and vice versa
  edges <- edges %>% filter(where1 %in% nodes$where1, where2 %in% nodes$where1)
  nodes <- nodes %>% filter(where1 %in% unique(c(edges$where1,edges$where2)))
  
  
  # add numeric code to nodes 
  nodes$label<-as.character(nodes$where1)
  nodes$id<-1:length(nodes$label)
  
  # add numeric code to edges
  iden<-nodes %>% select(from=id, where1=label)
  edges <- merge(edges,iden, by="where1",all.x=T,all.y=F)
  iden<-nodes %>% select(to=id, where2=label)
  edges <- merge(edges,iden, by="where2",all.x=T,all.y=F)
  
  # add size to nodes
  nodes$value  = sqrt(10 *nodes$success/max(nodes$success))
  
  # add size to edges
  edges$value  = sqrt(10 *edges$success/max(edges$success))
  
  # add color to nodes and edges
  n_cols <- 101
  pal <- colorRampPalette(mycol)(n_cols)
  p<-nodes$index/max(nodes$index)
  c_ref = findInterval(p, seq(from = 0, to = 1, length.out = n_cols))
  nodes$color<-pal[c_ref]
  p<-edges$index/max(nodes$index)
  c_ref = findInterval(p, seq(from = 0, to = 1, length.out = n_cols))
  edges$color<-pal[c_ref]
  
  
  # eliminate loops
  edges <- edges[edges$from < edges$to,]
  
  
  net<- visNetwork(nodes, 
                   edges, 
                   main = title)
        #           height = "700px", 
        #           width = "100%") %>%
    visNodes(scaling =list(min =5, max=30, 
                           label=list(min=5,max=30, 
                                      maxVisible = 5)))%>%
    visEdges(scaling =list(min =5, max=30)) %>%
    visOptions(highlightNearest = TRUE,
               #               selectedBy = "group", 
               #               manipulation = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLegend() %>%
    visIgraphLayout(layout ="layout.fruchterman.reingold",smooth = TRUE)
  
  net
  
  
  
  
  
  output<-list("edges" = edges, "nodes" = nodes, "plot" = net)
  
  return(output)
}
