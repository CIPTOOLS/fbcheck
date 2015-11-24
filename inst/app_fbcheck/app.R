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
 
   renderer_NTP <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <1  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_Plant_Vigor <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (value!=1 && value!= 3 && value!=1 && value!= 5 && value!=1 && value!= 7 && value!=1 && value!= 9  ) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_SE <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (value!=1 && value!= 3 && value!=1 && value!= 5 && value!=1 && value!= 7 && value!=1 && value!= 9  ) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_NPH <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_PPH <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_NMTP <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >1000) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_MTWP <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >1000) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_MTWPL <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >1000) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_MTYA <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_MTYNA <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_Tuber_Apper <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (value!=1 && value!= 3 && value!=1 && value!= 5 && value!=1 && value!= 7 && value!=1 && value!= 9  ) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_ATMW <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >2000) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_AVDM <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments); 
    if (value <0  ) {
    td.style.background = 'pink';
    } else if (value >100) {
    td.style.background = 'pink';
    } 
  } " 
  renderer_Chip_Color <-  " function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    if (value!=1 && value!= 2 && value!=1 && value!= 3 && value!=1 && value!= 4 && value!=1 && value!= 5  ) {
    td.style.background = 'pink';
    } 
  } " 
  
    
    rhandsontable::rhandsontable(data = fieldbook_dashboard,readOnly = FALSE,useTypes = TRUE) %>%  
      hot_col(col = 'NTP' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_NTP) %>% 
      hot_col(col = 'Plant_Vigor' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_Plant_Vigor) %>% 
      hot_col(col = 'SE' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_SE) %>% 
      hot_col(col = 'NPH' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_NPH) %>% 
      hot_col(col = 'PPH' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_PPH) %>% 
      hot_col(col = 'NMTP' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_NMTP) %>% 
      hot_col(col = 'MTWP' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_MTWP) %>% 
      hot_col(col = 'MTWPL' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_MTWPL) %>% 
      hot_col(col = 'MTYA' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_MTYA) %>% 
      hot_col(col = 'MTYNA' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_MTYNA) %>% 
      hot_col(col = 'Tuber_Apper' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_Tuber_Apper) %>% 
      hot_col(col = 'ATMW' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_ATMW) %>% 
      hot_col(col = 'AVDM' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_AVDM) %>% 
      hot_col(col = 'Chip_Color' ,readOnly = FALSE,
              allowInvalid = TRUE,copyable = TRUE, renderer =renderer_Chip_Color)
    
    
    
})
  
  output$hot_summary = renderRHandsontable({
    fieldbook_dashboard <- as.data.frame(fieldbook())
    
    
    
  }) 
  
  
  }

shinyApp(ui, server)