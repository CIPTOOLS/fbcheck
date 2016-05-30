#' UI for traittools
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export

fbcheck_ui <- function(type="tab", title="Data Quality and Processing", name="data_processing") {       
  
  #begin data_processing tabItem
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = "Data Quality", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                          #tabsetPanel(
                          tabBox(width = 12,
                            tabPanel("Check", #begin tabset "CHECK"
                                     fluidRow(
                                       shinyFiles::shinyFilesButton('file', 'File select', 'Please select a file',FALSE),
                                       shiny::actionButton("calculate", "Calculate",icon("play-circle-o")),
                                       HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                       shiny::actionLink('exportButton', 'Download data'),
                                       HTML('</div>'),
                                       box(rHandsontableOutput("hot_btable",height = "1400px",width = "1000px"),
                                           height = "3400px",width ="2400px")
                                     ),
                                     
                                     tags$style(type='text/css', "#file { width:150px; margin-top: 25px;}"),
                                     tags$style(HTML('#file {background-color:#0099cc; color: #ffffff}')),  
                                     tags$style(type='text/css', "#calculate { width:150px; margin-top: 25px;}"),
                                     tags$style(HTML('#calculate {background-color:#21b073; color: #ffffff}'))
                                     
                            )#,#end tab Panel "CHECK"
                            
#                             tabPanel("Trait List", #begin Trait List Panel"
#                                      fluidRow(
#                                        box(rHandsontableOutput("hot_td_trait",height = "1400px",width = "1400px"),
#                                            height = "3400px",width ="2400px")
#                                      )#end fluidRow
#                             )
                          )
                          ),
                        br(),
                        br(),
                        br()


  )#End data_processing tabItem
  
}

