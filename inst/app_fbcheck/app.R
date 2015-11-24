library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)

ui = dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table", icon = icon("dashboard")),
      menuItem(text = "Data Quality", shiny::fileInput(inputId = "upload_fieldbook",accept = ".xlsx",
                                                       label = "Upload Fieldbook"), icon = icon("check-circle"),
               menuSubItem(text = "Checking Fieldbook",tabName = "checking_fieldbook")
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              #fluidRow(box(rHandsontableOutput("hot",width = 2000, height = 400),width = 2000,height = 500,collapsible = TRUE))
              fluidRow(rHandsontableOutput("hot",width = 1000, height = 800),width = 2000,height = 500,collapsible = TRUE)
              #fluidRow(rHandsontableOutput("hot",width = 2000, height = 400))
              #fluidRow(box(rHandsontableOutput("hot3")))
      ),
      tabItem(tabName = "summary",
              #fluidRow(box(rHandsontableOutput("hot",width = 2000, height = 400),width = 2000,height = 500,collapsible = TRUE))
              fluidRow(rHandsontableOutput("hot_summary",width = 1000, height = 800),width = 2000,height = 500,collapsible = TRUE)
              #fluidRow(rHandsontableOutput("hot",width = 2000, height = 400))
              #fluidRow(box(rHandsontableOutput("hot3")))
      )
      
      
      
      
    )
  )
)

server = function(input, output) {
  
  
  fieldbook <- reactive({
    fb_file <- input$upload_fieldbook
    if(is.null(fb_file)){return()}
    #if(!is.null(fb_file)){
    file.copy(fb_file$datapath, paste(fb_file$datapath, ".xlsx", sep=""))
    fieldbook <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook") 
    fieldbook <- as.data.frame(fieldbook)
    inst <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Installation") # reverted to xlsx so all formulas are read as values
    mgt  <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Crop_management")
    mtl  <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Material List")
    mml	 <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Minimal") # reverted to xlsx so all formulas are read as values
    typ	 <-  as.character(mml[mml$Factor=="Type of Trial","Value"])
    plot_size <- as.numeric(inst[stringr::str_detect(inst$Factor,"Plot size"),"Value"])
    plant_den <- as.numeric(inst[stringr::str_detect(inst$Factor,"Planting density"),"Value"])
    
    fieldbook <-  sbformula::sbcalculate(fb = fieldbook, plot.size = plot_size,plant.den = plant_den)   
    print(fieldbook)
    fieldbook
    #}   
  })
  
  output$hot = renderRHandsontable ({
    #rhandsontable(do.call(cbind, lapply(1:20, function(i) data.table(rnorm(10000)))))
    fieldbook_dashboard <- as.data.frame(fieldbook())
    fieldbook_dashboard$PLOT <- as.integer(fieldbook_dashboard$PLOT)
    fieldbook_dashboard$REP <- as.integer(fieldbook_dashboard$REP)

    datadict <- readxl::read_excel(path=fp,sheet="Template for submission",skip=5)
    dict_trait <- datadict$ABBR  
    
    fb_header <- names(fieldbook_dashboard)
    fb_trait <- intersect(dict_trait,fb_header)
    nt <- length(fb_trait)
    
    out_temp <- list() 
    renderer_trait <-  list()
    
    
  for(i in 1:nt){
      
    print(i)
  out_temp[[1]]<- rhandsontable::rhandsontable(data = fieldbook_dashboard,readOnly = FALSE,useTypes = TRUE) #%>%  
  renderer_trait[[i]] <- render_trait(fb_trait[i],datadict)
      
  j <- i+1
  #print(j)
  out_temp[[j]] <- hot_col(hot = out_temp[[i]],col = fb_trait[i] ,readOnly = FALSE,
                               allowInvalid = TRUE,copyable = TRUE, renderer = renderer_trait[[i]])
      
  }
  k <- nt+1
  out_temp[[k]]
    
})
  
  output$hot_summary = renderRHandsontable({
    fieldbook_dashboard <- as.data.frame(fieldbook())
    
    
    
  }) 
  
  
  }

shinyApp(ui, server)