
if (!require("devtools")) install.packages("devtools")
#devtools::install_github("garthtarr/pairsD3")
#devtools::install_github("timelyportfolio/parcoords")
#devtools::install_github("rstudio/d3heatmap")

library(ggplot2)
library(pairsD3)
library(shiny)
library(parcoords)
library(d3heatmap)
library(tibble)
library(dplyr)


##
setwd('/Users/linda/msan622/lesson5/')
a<-read.table('dataset_Facebook.csv',header=TRUE,sep = ";") 
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

select <- c("Page.total.likes","Type","Category","Paid")
controls <-list(tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'numSelector', 
                                   label    = "Select varibles you are interested: ", 
                                   choices  = names(a),
                                   selected = select,
                                   inline   = FALSE)))

controls.2 <-list(tags$div(align = 'left', 
                         class = 'multicol', 
                         checkboxGroupInput(inputId  = 'numSelector.2', 
                                            label    = "Select varibles you are interested: ", 
                                            choices  = names(a),
                                            selected = select,
                                            inline   = FALSE))) 


  
ui<-fluidPage(
  tabsetPanel(
    type='tabs',
    tabPanel("HeatMap",
             fluidRow(tweaks,column(3,
                                    wellPanel(
                                      selectInput(inputId="factors",label="Factor variable:",
                                                  choices=c('Type'=1,'Paid'=2,'Category'=3),multiple=FALSE,selected =1),
                                      sliderInput("k_row"," Row dendrogram",1,min=1,max=5,step = 1),
                                      sliderInput("k_col"," Col dendrogram",1,min=1,max=5,step = 1)
                                      
                                    )),
                      column(9,h3("Correlation Plot"),d3heatmapOutput("heatmapplot"))
                      )),
    tabPanel("Scatter Matrix",
             fluidRow(tweaks,column(3,
                             wellPanel(
                               uiOutput("facselect1"),
                               uiOutput("facselect2"),
                               sliderInput("cex","Size of plotting symbol",3,min=1,max=10),
                               sliderInput("opacity","Opacity of plotting symbol",0.9,min=0,max=1),
                               radioButtons("theme", "Colour theme",
                                            choices = c("Colour"= "colour",
                                                        "Monochrome"="bw")),
                               sliderInput("width","Width and height",600,min=200,max=1200)
                             )),
                      column(9,uiOutput("pairsplot")),
                      column(width = 8, controls.2)
                      )),
    tabPanel("Parallel Coordinate",
             column(3,
                    wellPanel(uiOutput("facselect1.2"),
                              uiOutput("facselect2.2"),
                      selectInput(inputId="brushMode",label="Brush Mode",
                                  choices=c('1D-axes','1D-axes-multi','2D-strums'),
                                  multiple=FALSE,selected ='2D-strums'),
                      sliderInput("alpha","Opacity of plotting symbol",0.9,min=0,max=1)
                    )),
             column(9,parcoordsOutput( "parcoords", width = "800px", height = "400px" )),
             hr(),
             column(width = 8, controls)
            )
    )
 
)


server<- function(input, output,session) {
  ## prevent user select less than 2 more than 4
  observe({
    my_max<-4
    if(length(input$numSelector) > my_max)
    {
      updateCheckboxGroupInput(session, "numSelector", selected= tail(input$numSelector,my_max))
    }
    if(length(input$numSelector) < 4)
    {
      updateCheckboxGroupInput(session, "numSelector", selected=select)
    }
    if(length(input$numSelector.2) > my_max)
    {
      updateCheckboxGroupInput(session, "numSelector.2", selected= tail(input$numSelector.2,my_max))
    }
    if(length(input$numSelector.2) < 4)
    {
      updateCheckboxGroupInput(session, "numSelector.2", selected=select)
    }
  })
  
  ## select data 
  selecteddata <- reactive({
    a <- read.table('dataset_Facebook.csv',header=TRUE,sep = ";") 
    return(a)
    
  })
  ##select heatmapdata
  datainheatmap <-  reactive({
    if (input$factors ==1) {
      a <- read.table('dataset_Facebook.csv',header=TRUE,sep = ";") %>%          
        na.omit() %>%
        group_by(Type) %>%
        summarize_each(funs(mean)) %>%
        column_to_rownames(var='Type') %>%
        cor()
    }
    if (input$factors ==2) {
      a <- read.table('dataset_Facebook.csv',header=TRUE,sep = ";") %>%          
        na.omit() %>%
        select(-c(Type)) %>%
        group_by(Paid) %>%
        summarize_each(funs(mean)) %>%
        column_to_rownames(var='Paid') %>%
        cor()
    }
    if (input$factors ==3) {
      a <- read.table('dataset_Facebook.csv',header=TRUE,sep = ";") %>%          
        na.omit() %>%
        select(-c(Type)) %>%
        group_by(Category) %>%
        summarize_each(funs(mean)) %>%
        column_to_rownames(var='Category') %>%
        cor()
    }
    return(a)
    
  })
  
  ##d3heatmap
  output$heatmapplot <- renderD3heatmap({
    d3heatmap(datainheatmap(),k_row=input$k_row,k_col = input$k_col) 
  })
  
  ## parcoord
  output$parcoords<- renderParcoords({
    parcoords(selecteddata()[,input$numSelector],rownames=F,
              brushMode=input$brushMode,
              reorderable = T,
              margin =list(left=1),
              alpha = input$alpha,
              color = list(
                colorBy= group.2(),
                colorScale = htmlwidgets::JS("d3.scale.category10()"))
            ) 
     })
  ## parcoord
  output$facselect1.2 <- renderUI({
    radioButtons("factor_var_logical.2", label="Colored by a factor variable?",
                 choices = c("Yes" = 1,
                             "No" = 0),
                 selected =1)
  })
  ## parcoord
  output$facselect2.2 <- renderUI({
    conditionalPanel(
      condition = "input.factor_var_logical.2 == 1",
      selectInput(inputId="factor_var.2",label="Factor variable:",
                  choices=c('Type','Paid','Category'),multiple=FALSE,selected ='Type')
    )
  })
  ## parcoord
  group.2 <- reactive({
    if(input$factor_var_logical.2==1){
      return(input$factor_var.2)
    }else return(NULL)
  })


  ## MatrixD3 
  output$pairsplot = renderUI({
    pairsD3Output("pD3",width = input$width,height=input$width)
  })
  ## MatrixD3 
  output$pD3 <- renderPairsD3({
    
    pairsD3(selecteddata()[,input$numSelector.2], subset=subset,group=group(),
            theme = input$theme, opacity = input$opacity, cex = input$cex)
  })
  ## MatrixD3 
  output$facselect1 <- renderUI({
    radioButtons("factor_var_logical", label="Colored by a factor variable?",
                 choices = c("Yes" = 1,
                             "No" = 0),
                 selected =1)
  })
  ## MatrixD3 
  output$facselect2 <- renderUI({
    conditionalPanel(
      condition = "input.factor_var_logical == 1",
      selectInput(inputId="factor_var",label="Factor variable:",
                  choices=c('Type','Paid','Category'),multiple=FALSE,selected ='Type')
    )
  })
  ## MatrixD3 
  group <- reactive({
    if(input$factor_var_logical==1){
      return(selecteddata()[,input$factor_var])
    }else return(NULL)
  })

  
  
}


shinyApp(ui=ui,server=server)

