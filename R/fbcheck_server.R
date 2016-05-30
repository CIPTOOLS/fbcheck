#' Server component for traittools
#' Returns server side components
#' @author Omar Benites
#' @param input shinyserver input
#' @param output nameo of the output element
#' @param session shinyserver session
#' @param values The reactive values
#' @export 

fbcheck_server <- function(input, output, session, values) {
  
  volumes <- shinyFiles::getVolumes()
  shinyFileChoose(input, 'file', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))

  hot_path <- reactive ({
    
    if(is.null(input$file)){return(NULL)}
    
    validate(
      need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    print(hot_file)
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  hot_params <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Installation")
      #hot_design <- get_fb_param(hot_param,"Experimental design")
      #hot_design <- get_fb_param(hot_param,"Experimental_design")
      hot_design <- get_fb_param(hot_param,"Experimental_design_abbreviation")
      
      
      #hot_design <- get_fb_param(hot_param,"Experimental design")
      hot_design <- get_fb_param(hot_param,"Experimental_design")
      
      #hot_plot_size <- get_fb_param(hot_param,"Plot size (m2)")
      hot_plot_size <- get_fb_param(hot_param,"Plot_size_(m2)")
      
      #hot_plant_den <- get_fb_param(hot_param,"Planting density (plants/Ha)")
      hot_plant_den <- get_fb_param(hot_param,"Planting_density_(plants/Ha)")
      
      hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
                              hot_plant_den =  hot_plant_den)
    }
  })  
  
  hot_crop <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      hot_crop <- get_fb_param(hot_param,"Crop")
    }
  })
  
  hot_trial <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      #hot_crop <- get_fb_param(hot_param,"Type of Trial") #in DataCollector
      hot_trial <- get_fb_param(hot_param,"Type_of_Trial") #in HiDAP
      
    }
  })
  
  ###extra code
  hot_mgt <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){

      hot_mgt <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Crop_management", detectDates = TRUE)
      hot_mgt
      #print(hot_mgt)
    }
  })
  
  hot_mtl <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      #hot_mtl <- reactive_excel_metadata(file_id =hot_file , "Material List")
      #hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material List", detectDates = TRUE)
      hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material_List", detectDates = TRUE)
      hot_mtl
      #print(hot_mtl)
    }
  })

  output$hot_btable = renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_btable = hot_bdata()
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)

      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      #print(DF)
    }
    
    if(!is.null(DF)){
      
      traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_fieldbook.rds")
      crop <- hot_crop()
      trial <- hot_trial()
      print(DF)
      trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      
    }
    #}
    
  })
  
#   output$hot_td_trait = renderRHandsontable({ 
#     td_trait <- orderBy(~ABBR, td_trait)
#     rhandsontable(data = td_trait)
#   })
  
  shiny::observeEvent(input$exportButton,{
    
    withProgress(message = "Downloading Fieldbook and Applying Format...",value= 0,
                 {
                   DF <- readRDS("hot_fieldbook.rds")
                   trait <- get_trait_fb(DF)
                   crop <- hot_crop()
                   trial <- hot_trial()
                   trait_dict <- td_crop
                    #print("DF")

                   hot_design <- as.character(hot_params()$hot_design)
                   
                   if(is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",factor="FACTOR",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                   #print("4")
                   if(!is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                    hot_file <- hot_path() 
                    wb <- openxlsx::loadWorkbook(hot_file)
                   sheets <- readxl::excel_sheets(path = hot_file)
                    if(is.element("Fieldbook",sheets)){    
                     openxlsx::removeWorksheet(wb, "Fieldbook")
                   }
                   if(is.element("Summary",sheets)){    
                     openxlsx::removeWorksheet(wb, "Summary")
                   }
                   openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Fieldbook", x = DF,colNames = TRUE, withFilter = FALSE)
                   openxlsx::addWorksheet(wb = wb,sheetName = "Summary",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Summary", x = summary ,colNames = TRUE, withFilter = FALSE)
                   openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                   
                   traits <- traittools::get_trait_fb(DF)
                   traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = trait_dict)
                   traittools::col_trait_outlier(file = hot_file, sumsheet = "Summary",trait = trait)
                   shell.exec(hot_file)
                   
                 })
    
  })  
  
}