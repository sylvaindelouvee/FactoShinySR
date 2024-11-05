# ----------
# CITATION
# ----------
# Brosset, F. & Delouvée, S. (2022). FactoShinySR. R code for factorial analysis of social representations. Cutxan: CERISE (CollaborativE infoRmatIon on Social rEpresentations) Version: 1.1.
# ----------

library(shiny)
library(FactoMineR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(tidyr)
library(shinyjqui)

# creates the full Contingency table 
contingency <- function(X,indexO,indexV){
  X = X %>% select(indexO,indexV)
  X = pivot_longer(X,cols=all_of(indexO),names_to=NULL,values_to="Observations",values_drop_na=TRUE)
  X = pivot_longer(X,cols = !Observations,values_to = "var",names_to = NULL)
  CT = table(X$Observations, X$var)
  CT = as.data.frame.matrix(CT)
  return(CT)
}

#Delete Low occurences
DeleteLow <- function(CT,minOcc){
  return(CT %>% filter(rowSums(.)>minOcc))
}

#Prepares Data for the Plot
CoordForPlot <- function(CA,dim1,dim2,CT,dsc){
  
  #Getting coords and contrib for words ("mots") and other variables ("vars")
  CoordMots = as.data.frame.matrix(CA$row$coord[,c(dim1,dim2)])
  ContribMots = as.data.frame.matrix(CA$row$contrib[,c(dim1,dim2)])
  ContribMots = ContribMots %>% rename(Contrib1 =1,Contrib2 = 2)
  Mots = bind_cols(CoordMots,ContribMots)
  
  CoordVars = as.data.frame.matrix(CA$col$coord[,c(dim1,dim2)])
  ContribVars = as.data.frame.matrix(CA$col$contrib[,c(dim1,dim2)])
  ContribVars = ContribVars %>% rename(Contrib1 =1,Contrib2 = 2)
  Vars = bind_cols(CoordVars,ContribVars)
  
  
  #Adding Fonts
  size = nrow(CT)
  size2 = ncol(CT)
  
  Mots$font = "plain"
  Mots = Mots %>% 
    mutate(font=replace(font, Contrib1>(100/size), "bold")) %>% 
    mutate(font=replace(font, Contrib2>(100/size), "italic")) %>% 
    mutate(font=replace(font, Contrib2>(100/size) & Contrib1>(100/size), "bold.italic"))
  
  Vars$font = "plain"
  Vars = Vars %>% 
    mutate(font=replace(font, Contrib1>(100/size2), "bold")) %>% 
    mutate(font=replace(font, Contrib2>(100/size2), "italic")) %>% 
    mutate(font=replace(font, Contrib2>(100/size2) & Contrib1>(100/size), "bold.italic"))
  
  #Adding Colors
  Vars$VoO = "#CCCCCC" #Gray
  Mots$VoO = "#FFFFFF" #White
  
  #threshold
  if(dsc){
    Mots = Mots %>%
      filter(Contrib1>(100/size)|Contrib2>(100/size))
    Vars = Vars %>%
      filter(Contrib1>(100/size2)|Contrib2>(100/size2))
  }
  
  Coord = bind_rows(Mots,Vars)
  
  return(Coord)
}

# Ploting data
PlotStacked <- function(Coord,CA,DimNumb1,DimNumb2,zoomxf,zoomyf,LblSize){
  
  XLAB = paste(toupper(rownames(CA$eig)[DimNumb1])," ",round(CA$eig[DimNumb1,2],1), "%")
  YLAB = paste(toupper(rownames(CA$eig)[DimNumb2])," ",round(CA$eig[DimNumb2,2],1), "%")
  
  Plot = ggplot(data = Coord,aes(x=Coord[,1],y=Coord[,2],label=rownames(Coord)))+
    geom_point()+
    geom_label_repel(aes(fontface=font),fill=Coord$VoO,label.size=NA,size =LblSize,max.overlaps=50)+
    xlab(XLAB)+
    ylab(YLAB)+
    coord_cartesian(xlim = zoomxf, ylim = zoomyf, expand = FALSE)
  return(Plot)
}


# ui ----
ui <- fluidPage(
  navbarPage("Menu",
             
             
             # Load Data Panel ----
             tabPanel("Load Data",
                      
                      # App title 
                      titlePanel("Uploading Files"),
                      
                      ## Sidebar layout with input and output definitions 
                      sidebarLayout(
                        
                        ## Sidebar panel for inputs 
                        sidebarPanel(
                          
                          ## Input: Select a file 
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          ## Horizontal line 
                          tags$hr(),
                          tags$h6(
                            tags$strong("WARNING :"),
                            tags$br(),
                            tags$strong("Special characters"),
                            "are sometimes not understood by the application",
                            tags$i("(this might be due to the Excel version you are using).")),
                          ## Input: Checkbox if file has header 
                          checkboxInput("header", "Header", TRUE),
                          
                          ## Input: Select separator 
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ";"),
                          
                          ## Input: Select quotes 
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                          ## Horizontal line 
                          tags$hr(),
                          
                          ## Input: Select number of rows to display 
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head"),
                          
                          ## Choosing variables that are observations
                          varSelectizeInput("obs","Observations",multiple = TRUE,character(0)),
                          
                          ## Choosing supplementary variables
                          varSelectizeInput("var","Other Variables",multiple = TRUE,character(0)),
                          tags$h6(
                            tags$br(),
                            tags$strong("WARNING :"),
                            tags$br(),
                            "Loading less than 3 categories in total here will create a single dimension and make the APPLICATION CRASH.",
                            tags$br(),
                            tags$strong("Exemple :"),
                            tags$br(),
                            "loading only GENDER with sole categories MAN and WOMAN")
                        ),
                        
                        ## Main panel for displaying outputs 
                        mainPanel(
                          
                          ## Output: Data file 
                          tableOutput("contents")
                          
                        )
                        
                      )
             ),
             #Plot Panel ----
             
             tabPanel("Plot",
                      pageWithSidebar(
                        headerPanel('Correspondence Analysis on stacked tables for Social Representations'),
                        sidebarPanel(
                          
                          # Slider for choosing the number of occurences
                          sliderInput("occ",
                                      label="Minimum number of occurences",
                                      min=1, max=10, value= 4),
                          sliderInput("LblSize", "Label Size",
                                      min = 1.5, max = 6,
                                      value = 2.5, step = 0.25),
                          
                          # Box for the Deschamps threshold
                          checkboxInput("dsc", "Deschamps Threshold",TRUE),
                          
                          #Choosing dimensions to plot

                          numericInput("Dim1", "Dimension on the X axis", 1),
                          numericInput("Dim2", "Dimension on the Y axis", 2),
                          tags$h6(
                            tags$strong("Note :"),
                            tags$br(),
                            tags$strong("Max Number of dimension is :"),
                            tags$i("Total of non-observations-variables - 1")),
                          
                          # submit button for creating/updating the analysis
                          actionButton("useData", "Create/Update Analysis")
                          
                        ),
                        mainPanel(
                          tags$h4(tags$strong("Select a zone"),
                                  "and double-click to",
                                  tags$strong("zoom in."),
                                  "Double-click to",
                                  tags$strong("zoom out.")),
                          shinyjqui::jqui_resizable(plotOutput("plot1",
                                     dblclick = "plot1_dblclick",
                                     brush = brushOpts(
                                       id = "plot1_brush",
                                       resetOnNew = TRUE
                                     ))),
                          p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                          br(),
                        )
                        )
                      ),
             tabPanel("CA summary",
                      h2("CA Summary"),
                      br(),
                      tags$h5(tags$strong("Open In Browser"),
                              "( up-left corner) to use",
                             tags$strong("Search Word Function")),
                      verbatimTextOutput("summary"),
                      tags$h5("Create/update an analys (in plot section) for the",
                         tags$strong("CA Summary to appear/change"))
                      ),
             # About Panel ----
             tabPanel("About",
                      tags$h4(tags$strong("The data loading module"),
                      "was implemented using the code from this shiny : ",
                      tags$br(),
                      tags$a(href="https://shiny.rstudio.com/gallery/file-upload.html","shiny.rstudio.com/gallery/file-upload.html"),
                      tags$br(),
                      tags$br(),
                      tags$strong("Zooming in and out"),
                      "was implemented using the code from this shiny : ",
                      tags$br(),
                      tags$a(href="https://shiny.rstudio.com/gallery/plot-interaction-zoom.html","shiny.rstudio.com/gallery/plot-interaction-zoom.html"),
                      tags$br(),
                      tags$br(),
                      "This Shiny was inspired by the",
                      tags$strong("FactoShiny Library :"),
                      tags$br(),
                      tags$a(href="http://factominer.free.fr/graphs/factoshiny-fr.html","factominer.free.fr/graphs/factoshiny-fr.html"),
                      tags$br(),
                      tags$br(),
                      tags$strong("Citation :"),
                      tags$br(),
                      "Brosset, F. & Delouvée, S. (2022). FactoShinySR. R code for factorial analysis of social representations. Cutxan: CERISE (CollaborativE infoRmatIon on Social rEpresentations) Version: 1.1."
                      )
             )
             # ----
  )
)


# server function ----
server <- function(input, output) {
  
  values <- reactiveValues(
    df =NULL,
    Coord = NULL,
    CT = NULL,
    CA = NULL,
    Analysis = FALSE,
    zoom = NULL,
    zoomx = NULL,
    zoomy = NULL,
    dimNumb1 = NULL,
    dimNumb2 = NULL,
    Plot = NULL
  )
  
  
  # for loading data ----
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       na.strings="")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    #storing the data
    values$df = df
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # updates The selection of variables and observations depending on the data loaded ----
  observe({
    updateVarSelectizeInput(
      inputId = "obs",
      label = "Observations",
      data = values$df
    )
    updateVarSelectizeInput(
      inputId = "var",
      label = "Other Variables",
      data = values$df
    )
  })
  
  
  #updates zoom values ----
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      values$zoomx <- c(brush$xmin, brush$xmax)
      values$zoomy <- c(brush$ymin, brush$ymax)
      
    } else {
      values$zoomx <- NULL
      values$zoomy <- NULL
    }
  })
  
  #Performs all functions ----
  observeEvent(input$useData,{
    
    values$DimNumb1 = input$Dim1
    values$DimNumb2 = input$Dim2
    
    
    values$CT = contingency(values$df,as.character(input$obs),as.character(input$var))
    values$CT = DeleteLow(values$CT,input$occ)
    nDim = max(ncol(values$CT)-1,1)
    values$CA = CA(values$CT, ncp =nDim ,graph = FALSE)
    values$Coord = CoordForPlot(values$CA,values$DimNumb1,values$DimNumb2,values$CT,input$dsc)
    values$Plot = PlotStacked(values$Coord,values$CA,values$DimNumb1,values$DimNumb2,values$zoomx,values$zoomy,input$LblSize)
    #So the Analysis won't launch on the start
    values$Analysis = TRUE

  },ignoreNULL = TRUE
  )
  
  #plotting the data ----
  output$plot1 <-renderPlot({
    if(values$Analysis){
      print(values$Plot)
    }
    
  })
  
  #Printing the summary
  output$summary <- renderPrint({
    if(values$Analysis){
      nDim = max(ncol(values$CT)-1,1)
      options(max.print= .Machine$integer.max,width = 120)
      summary(values$CA,nbelements = Inf, ncp =nDim)
    }
  })
  
  output$downloadData = downloadHandler(
    filename = function() { 
      paste('graphCA','.png', sep='') 
    },
    content = function(file) {
      ggplot2::ggsave(file,values$Plot)
    },
    contentType='image/png')
  
  output$downloadData1 = downloadHandler(
    filename = function() { 
      paste('graphCA','.jpg', sep='') 
    },
    content = function(file) {
      ggplot2::ggsave(file,values$Plot)
    },
    contentType='image/jpg')
  
  output$downloadData2 = downloadHandler(
    filename = function() { 
      paste('graphCA','.pdf', sep='') 
    },
    content = function(file) {
      ggplot2::ggsave(file,values$Plot)
    },
    contentType=NA)
  

}

# Run the application 
shinyApp(ui = ui, server = server)
