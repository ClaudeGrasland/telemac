library(shiny)
#library(plotly)

wwww_description <- list(
  tags$h4("AUTHORS"),
  tags$p("This tool has been developped by Claude Grasland (1,2,3), Romain Leconte (1,2), Etienne Toureille(2,3,4) and Marta Severo (5) : 
        1. Université de Paris, 2. FR 2007 CIST, 3. UMR 8504 Géographie-cités, 4. CNRS, 5. Université Paris Nanterre"),
  tags$p(),
  tags$h4("SOURCES"),
  tags$p("The data used in this application are derived from the MediaCloud, an open source platform for studying media ecosystems (https://mediacloud.org/).
         Media Cloud is a consortium research project across multiple institutions, including the University of Massachusetts Amherst, Northeastern University, and the Berkman Klein Center for Internet & Society at Harvard University. 
         This research has benefit from a special extraction of the Media Cloud database including not only titles but also first sentences of description collected in RSS flows."),
  tags$h4("FUNDING"),
  tags$p("The Telemac tool has been developped by the authors in the framework of the H2020 ODYCCEUS project. This project has received funding from the European Union's HORIZON 2020 research and innovation program
         under gant n° 732942 (https://www.odycceus.eu). he has also benefit from the support of the French research 
         plateform Huma-Num (https://www.huma-num.fr) for the hosting of the current shiny web application."),
  tags$h4("CONTACT"),
  tags$p("The authors can be contacted by sending an e-mail to (claude.grasland@parisgeo.cnrs.fr)."),
  tags$h4("DISCLAIMER"),
  tags$p("Information provided by TELEMAC are based on scientific research applied to experimental data. 
         The results displayed by the application are the result of logical and prudent research.
         However, to err is human. Therefore the authors of TELEMAC does not warrant that all results extracted 
         from the application in any form, and the research report documets provided are free of errors.")
  )


## HYPERCUBE VISUALIZATION


Exploration <- fluidRow(
  headerPanel(h4("CONTROL PANEL")),
  column(2,
         wellPanel(
           h4("MAIN PARAMETERS"),
             selectInput("topic","WHAT = Topic",
                         choices = c("Migrant and Refugees" = "mobil",
                                     "Borders" = "border"),
                         selected="mobil"),
 
           checkboxGroupInput("host", "WHO = Host country",
                              choices = c("France" = "FRA",
                                          "Germany" = "DEU",
                                           "Italy" = "ITA",
                                          "Spain" = "ESP",
                                          "United Kingdom" = "GBR"),
                              selected = c("FRA","DEU","ITA","ESP","GBR")),

           sliderInput("timeperiod", label="WHEN = Time period",
                       min=as.Date("2014-01-01"), 
                       max=as.Date("2019-12-31"), 
                       value= c( as.Date("2013-06-01"), as.Date("2020-06-30")), 
                       round = 1, step=1, sep=" "),
           selectInput("TimeSpan","WHEN = Time breaks",
                        choices=c("by week"="week","by month"="month","by quarter" = "quarter", "by year"="year"),
                        selected="week"),
           radioButtons("test","WHY = Research question",
                        choices=c("Salience index"=FALSE,"Statistical test"=TRUE),
                        selected=TRUE),
           h4("OTHER PARAMETERS"),
           sliderInput("textsize", label="Text (1 = title, 2-4 = desc.)",
                       min=1, max=4, value= c(1, 2), round = 1, step=1),
           radioButtons("Media","Aggregation of media",
                        choices=c("By newspapers"="who","By country"="who_state"),
                        selected="who_state"),
           sliderInput("minsamp", label="Minimum sample",
                       min=5, 
                       max=100, 
                       value= 20, 
                       round = 1, step=5),
           sliderInput("mintest", label="Minimum test",
                       min=1, 
                       max=20, 
                       value= 5, 
                       round = 1, step=0.5),
           sliderInput("maxloc", label="Top countries on heatmap",
                       min=10, 
                       max=50, 
                       value= 25, 
                       round = 0, step=1),
           sliderInput("minedge", label="Minimum edge size",
                       min=2, 
                       max=20, 
                       value= 2, 
                       round = 0, step=1),
           sliderInput("minnode", label="Minimum node size",
                       min=5, 
                       max=50, 
                       value= 10, 
                       round = 0, step=1),
           

      



         )),
  column(9,
         tabsetPanel(
      #     tabPanel("What",
      #              plotlyOutput("what",height = "500px")),

          tabPanel("Who.What",
                    plotlyOutput("who.what",height = "500px")),

           tabPanel("When.What",
                    plotlyOutput("when.what",height = "500px")),

           tabPanel("Where.What",
                    plotlyOutput("where.what",height = "500px")),

           tabPanel("When.Who.What",
                    plotlyOutput("when.who.what",height = "500px")),

          tabPanel("When.Where.What",
                    plotlyOutput("when.where.what",height = "500px")),

           tabPanel("Where.Who.What",
                    plotlyOutput("where.who.what",height = "500px")),

         tabPanel("Where.Where.What",
                   plotOutput("where.where.what",height = "500px")),

#          tabPanel("Where.Where.What.Dom",
 #                  plotOutput("where.where.what.dom",height = "500px")),

           tabPanel("About",
                    wwww_description)


         )

  ))

credits <-  wellPanel(
  h4("WHAT"))


navbarPage("TELEMAC (Topic Explorator Lab of European Media Agenda applied to Crisis)",
#           header=tags$head(tags$style(HTML(css_string))),

            tabPanel("Exploration",Exploration)

)















