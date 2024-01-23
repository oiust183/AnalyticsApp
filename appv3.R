library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(factoextra)
library(rstatix)
library(radiant)
library(purrr)
#data(mtcars)




# Define UI for application
ui = fluidPage(
  navbarPage("Analytics App",
             tabPanel("Welcome",
                      tabName = "welcome",
                      icon=icon("door-open"),
                      
                      fluidPage(theme=shinytheme("cerulean"),
                                h1("Welcome to Analytics App!"),
                                br(),
                                p(strong(tags$u("What is this app all about?"))),
                                p("This is an app developed for Marketing Analytics class at Kent State University.
                                  It performs various types of analysis discussed in class usings R."),  
                                br(),
                                
                                p(strong(tags$u("Multiple Regression"))),
                                p("Fits a multiple regression model using the uploaded data file and specified predictors and outcome.
                                  The outcome variables should be numerical."),
                                br(),
                                p(strong(tags$u("Logistic Regression"))),
                                p("Fits a logistic regression model. The outcome should be a 0-1 variable. The results are reported in terms of odds ratios.
                                  R outputs are also depicted."),
                                br(),
                                p(strong(tags$u("Clustering"))),
                                p("The clustering module performs a k-means clustering using two numerical variables (shown by X and Y).
                                  The user can choose 2 to 6 clusters."),
                                br(),
                                p(strong(tags$u("Disclaimer"))),
                                p("Curently, this app does not check for the validity of the inputs. All input files shoud be in csv format."),
                                br(),
                                p(strong(tags$u("Notes"))),
                                p("I would like to thank Spring 2022 Marketing Analytics students for their feedback.
                                  Please email your questions and comments to mmoham19@kent.edu."),
                                br(),
                                
                      )),
             
             tabPanel("Multiple Regression",
                      tabname="regression",
                      icon=icon("calculator"),
                      fileInput(
                        inputId = "filedataR",
                        label = "Upload data. Choose csv file",
                        accept = c(".csv")
                      ),
                     uiOutput("xvariableR"),
                     uiOutput("yvariableR"),
                     br(),
                      dataTableOutput('tableR'), br(),
                     verbatimTextOutput(outputId = "RegOut")
                      
             ),
             tabPanel("Logistic Regression",
                      tabname="Lregression",
                      icon=icon("calculator"),
                      fileInput(
                        inputId = "filedataL",
                        label = "Upload data. Choose csv file",
                        accept = c(".csv")
                      ),
                      uiOutput("xvariableL"),
                      uiOutput("yvariableL"),br(),uiOutput("ResL"),
                      dataTableOutput('tableL'),
                      br(),downloadButton('downloadL',"Make Predictions"),
                      br(),
                      uiOutput("ResLR"),
                      verbatimTextOutput(outputId = "LRegOut")
                      
             ),
             
             tabPanel("Clustering",
                      tabname="clustering",
                      icon = icon("calculator"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput(
                            inputId = "filedata",
                            label = "Upload data. Choose csv file",
                            accept = c(".csv")
                          ),
                          
                          selectInput(
                            inputId = "nk",
                            label = "Number of Clusters",
                            choices = c(2,3,4,5,6,7,8,9,10)
                          ),
                          selectInput(
                            "y_input", 
                            label = h5("Select Segmentation Variables"),multiple = T,
                            ""
                          ),checkboxInput("j1", "Jitter", value = FALSE, width = NULL),
                          # selectInput(
                          #   "x_input", 
                          #   label = h5("Select Y"),
                          #   ""
                          # ),checkboxInput("j2", "Jitter", value = FALSE, width = NULL),
                          actionButton(
                            "rn", "Run k-means"
                          ),br(),br(),
                          HTML('<center><img src="logo.jpg",height=150, width=150></center>'),
                        ),
                        
                        mainPanel(
                          plotOutput("coolplot",height = 450, width = 510),
                          br(), textOutput("txt"),
                          
                          downloadButton('download',"Download this result"),
                          br(),
                          plotOutput("silplot",height = 450, width = 510),
                        )
                      )
             ),
             tabPanel("Conjoint Analysis",
                      tabname="conjoint",
                      icon=icon("calculator"),
                      fileInput(
                        inputId = "filedataC",
                        label = "Upload data. Choose csv file",
                        accept = c(".csv")
                      ),
                      uiOutput("xvariableC"),
                      uiOutput("yvariableC"),
                      uiOutput("RespvariableC"),br(),
                      actionButton(
                        "run", "Run Conjoint"
                      ),
                      br(),
                     # dataTableOutput('tableC'), br(),
                      verbatimTextOutput(outputId = "COut"), br(),
                     downloadButton('downloadC',"Download Importance Weights"),br(),br(),
                     downloadButton('downloadPW',"Download Part-worths"),br(),br(),
                     verbatimTextOutput(outputId = "C2Out")
             )
  ))
# Define server logic 
server <- function(input, output,session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  #-------------------REGRESSION-------------------#
  
  dataR <- reactive({
    req(input$filedataR)
    inData <- input$filedataR
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  
  
  output$xvariableR <- renderUI({
    req(dataR())
    xa<-colnames(dataR())
    pickerInput(inputId = 'xvarR',
                label = 'Select predictors',
                choices = c(xa[1:length(xa)]), selected=xa[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariableR <- renderUI({
    req(dataR())
    ya<-colnames(dataR()) 
    pickerInput(inputId = 'yvarR',
                label = 'Select outcome',
                choices = c(ya[1:length(ya)]), selected=ya[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  lmModel <- reactive({
    req(dataR(),input$xvarR,input$yvarR)
    xR <- as.numeric(dataR()[[as.name(input$xvarR)]])
    yR <- as.numeric(dataR()[[as.name(input$yvarR)]])
    current_formulaR <- paste0(input$yvarR, " ~ ", paste0(input$xvarR, collapse = " + "))
    current_formulaR <- as.formula(current_formulaR)
    modelR <- lm(current_formulaR, data = dataR(), na.action=na.exclude)
    return(modelR)
  })
  
  output$tableR <- renderDataTable({
    req(lmModel())
    resultsR<-data.frame(variables=names(lmModel()$coefficients)
                         ,coefficients=round(lmModel()$coefficients,2))
    resultsR$interpretation=ifelse(resultsR$coefficients>0,
                                   paste0("One unit increase in ",resultsR$variables,
                                          " <strong>increases</strong> the outcom by ",
                                          resultsR$coefficients, " units" ),
                                   paste0("One unit increase in ",resultsR$variables,
                                          " <strong>decreases</strong> the outcom by ",
                                          resultsR$coefficients, " units" ))
    resultsR[1,"interpretation"]=""
    return(resultsR)
    
  },escape = F)
  output$RegOut = renderPrint({
    req(lmModel())
    summary(lmModel())
    })
  #------------------REGRESSION--------------------#
  #------------------Logistic--------------------#
  dataL <- reactive({
    req(input$filedataL)
    inData <- input$filedataL
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  
  
  output$xvariableL <- renderUI({
    req(dataL())
    xa<-colnames(dataL())
    pickerInput(inputId = 'xvarL',
                label = 'Select predictors',
                choices = c(xa[1:length(xa)]), selected=xa[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariableL <- renderUI({
    req(dataL())
    ya<-colnames(dataL()) 
    pickerInput(inputId = 'yvarL',
                label = 'Select outcome',
                choices = c(ya[1:length(ya)]), selected=ya[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  LModel <- reactive({
    req(dataL(),input$xvarL,input$yvarL)
    xL <- as.numeric(dataL()[[as.name(input$xvarL)]])
    yL <- as.numeric(dataL()[[as.name(input$yvarL)]])
    current_formulaL <- paste0(input$yvarL, " ~ ", paste0(input$xvarL, collapse = " + "))
    current_formulaL <- as.formula(current_formulaL)
    modelL <- glm(current_formulaL, data = dataL(), family = "binomial", na.action=na.exclude)
    return(modelL)
  })
  pred_down <- reactive({
    req(LModel(),dataL())
    predictions<-predict(LModel(),newdata=dataL(),type="response")
    dt<-cbind(dataL(),predictions)
    return(dt)
  })
  
  output$ResL<-renderUI({
    req(LModel())
    return(textOutput("Results"))
  })
  output$tableL <- renderDataTable({
    req(LModel())
    resultsL<-data.frame(variables=names(LModel()$coefficients)
                           ,coefficients=round(exp(LModel()$coefficients),2))
    resultsL$interpretation=ifelse(resultsL$coefficients>1,
                                   paste0("the odds of the outcome for ",resultsL$variables," are ",
                                          (resultsL$coefficients-1)*100, " percent <strong>higher</strong>" ),
                                   paste0(" the odds of the outcome for ",resultsL$variables," are ",(1-resultsL$coefficients)*100, " percent <strong>lower</strong>" ))
    resultsL[1,"interpretation"]=""
     return(resultsL)
    
    },escape = F)
 
  output$downloadL <- downloadHandler(
    filename = "predictions.csv", 
    content = function(fname){
      req(pred_down())
      write.csv(pred_down(), fname)
    }
  )
  output$LRegOut = renderPrint({
    req(LModel())
    summary(LModel())
  })
  #------------------Logistic--------------------#
  
  
  
  #------------------Clustering--------------------#
  data <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
  })
  
  nclusters<-reactive(input$nk)
  #output$results <- renderTable(data())
  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=names(data()))
    
  })
  # observe({
  #   updateSelectInput(
  #     session,
  #     "x_input",
  #     choices=names(data()))
  #   
  # })
  observeEvent(input$rn,{
    output$coolplot<-renderPlot({
      # Compute k-means with k = 3
      data <- reactive({
        req(input$filedata)
        read.csv(input$filedata$datapath)
      })
      
      set.seed(123)
      df<-data()
      cs<-6
      res.km <- kmeans(na.omit(df[,input$y_input]),as.numeric(input$nk) ,iter.max = 40, nstart = 3,algorithm="Lloyd")
      # K-means clusters showing the group of each individuals
      output$download <- downloadHandler(
        filename = function(){paste0(input$nk,"segments.csv")}, 
        content = function(fname){
          write.csv(cbind(df%>%drop_na(input$y_input),res.km$cluster), fname, row.names = F)
        }
      )
      pdata<-df[,input$y_input]
      if(input$j1==T)
        pdata<-data.frame(lapply(pdata, jitter))
      # if(input$j2==T)
      #   pdata[,2]<-jitter(pdata[,2])
      fviz_cluster(res.km, data = na.omit(pdata),
                   #  palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
                   geom = "point",
                   ellipse.type = "convex", 
                   ggtheme = theme_bw()
      )
      
      
      
    } )
  })
  
  observeEvent(input$rn,{
    output$silplot<-renderPlot({
      # Compute k-means with k = 3
      data <- reactive({
        req(input$filedata)
        read.csv(input$filedata$datapath)
      })
      
      df<-data()
      
      pdata<-df[,input$y_input]
      
      fviz_nbclust(x = na.omit(pdata), kmeans, method = "silhouette")
      
      
    } )
  })
  #------------------Clustering--------------------#
  #-------------------Conjoint-------------------#
  
  dataC <- reactive({
    req(input$filedataC)
    inData <- input$filedataC
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  
  
  output$xvariableC <- renderUI({
    req(dataC())
    xa<-colnames(dataC())
    pickerInput(inputId = 'xvarC',
                label = 'Select Attributes',
                choices = c(xa[1:length(xa)]), selected=xa[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariableC <- renderUI({
    req(dataC())
    ya<-colnames(dataC()) 
    pickerInput(inputId = 'yvarC',
                label = 'Select Rating Column',
                choices = c(ya[1:length(ya)]), selected=ya[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  output$RespvariableC <- renderUI({
    req(dataC())
    Respa<-colnames(dataC()) 
    pickerInput(inputId = 'RespvarC',
                label = 'Select Respondent Column',
                choices = c(Respa[1:length(Respa)]), selected=Respa[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
 observeEvent(input$run,{
   CModel <- reactive({
     req(dataC(),input$xvarC,input$yvarC,input$RespvarC)
     modelC <- conjoint(dataC()%>%group_by(!!sym(input$RespvarC))%>%filter(all(!is.na(!!sym(input$yvarC))) 
                                                        & n_distinct(!!sym(input$yvarC),na.rm = T)!=1
     )
                          ,rvar=input$yvarC,evar=input$xvarC,by=input$RespvarC)
     return(modelC)
   })
   C2Model <- reactive({
     req(dataC(),input$xvarC,input$yvarC)
     modelC2 <- conjoint(dataC(), rvar = input$yvarC, evar = input$xvarC)
     
     return(modelC2)
   })
   output$COut = renderPrint({
     req(CModel())
     return(CModel()[["IW"]])
   })
   output$C2Out = renderPrint({
     req(C2Model())
     return(summary(C2Model()))
   })
   output$downloadC <- downloadHandler(
     filename = function(){ return("Importance-weights.csv") },
     content = function(fname){
       write.csv(CModel()[["IW"]][,-2], fname, row.names = F)
     }
   )
   output$downloadPW <- downloadHandler(
     filename = function(){ return("Part-worths.csv") },
     content = function(fname){
       write.csv(CModel()[["PW"]][,-2], fname, row.names = F)
     }
   )
 }) 
  
  
  
  
  
  
  
  
  #------------------Conjoint--------------------#
}

# Run the application 
shinyApp(ui = ui, server = server)