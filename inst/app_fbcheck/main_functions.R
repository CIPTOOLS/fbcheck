fp <-  "C:\\OMAR-2015\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx"
fp_fb <-"C:\\OMAR-2015\\hidap\\inst\\hidap\\data\\potato\\200211\\PTYL200211_CHIARA.xlsx"
fieldbook <- readxl::read_excel(fp_fb,"Fieldbook")  
fieldbook <- as.data.frame(fieldbook)

trait_type <- function(trait,datadict)
{
  tp <- as.character(datadict[datadict$ABBR==trait,c("TYPE")]) 
  stringr::str_trim(tp,side="both")
  
  if(is.na(tp)){
    tp <- "none"
  }
  return(tp)
}


#'@description Function to get the scale of differents trait, 
#'dependig if Its continous/discrete/categorical variable
#'
scale_trait <- function(trait,datadict){
  
  tp <- trait_type(trait = trait,datadict = datadict)
  
  if(tp=="Continuous"||tp=="Discrete"){
    
    ll <- as.numeric(datadict[datadict$ABBR==trait,c("LOWER")])
    ul <- as.numeric(datadict[datadict$ABBR==trait,c("UPPER")])
    output <- list(ll=ll,ul=ul)
  }
  
  if(tp=="Categorical"){
    cat_scale <- datadict[datadict$ABBR == trait, c("CLASS1","CLASS2","CLASS3","CLASS4","CLASS5","CLASS6","CLASS7","CLASS8","CLASS9","CLASS10")]
    pattern <- "= .*$"
    cat_scale <- gsub(pattern=pattern,replacement = "",x = cat_scale)
    cat_scale <- suppressWarnings(as.numeric(cat_scale))
    cat_scale <- as.numeric(stringr::str_trim(cat_scale[!is.na(cat_scale)],side="both"))
    output <- list(cat_scale=cat_scale)
  }
  
  if(tp=="none"){output <- print("none")
  }
  
  invisible(output)
  
}

render_quantitative <- function(ll,ul){
  out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments); 
               if (value <", ll ,"  ) {
               td.style.background = 'pink';
               } else if (value >", ul,") {
               td.style.background = 'pink';
               } 
}", sep="")
    #cat(out,"\n"," ")
  return(out)
  }

render_categorical <- function(scale_condition){ 
  out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (",scale_condition,"  ) {
               td.style.background = 'pink';
               } 
}",sep="")
    #cat(out,"\n"," ")
  return(out)
  } 

render_trait<- function(trait,datadict){

  tp <- trait_type(trait,datadict)  
  scale_trait_values <- scale_trait(trait = trait,datadict = datadict)
  
  if(tp == "Continuous"|| tp == "Discrete"){
    
    ul <- scale_trait_values$ul  
    ll <- scale_trait_values$ll 
    
    #assign_trait <- paste("renderer_",trait," <- ",sep="") 
    render_trait <- render_quantitative(ll=ll,ul=ul)
    #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
    #cat("\"",render_trait,"\"","\n"," ")
    #out <- paste("\"",render_trait,"\"","\n"," ")
    out <- paste(render_trait)
    
  }
  
  if(tp =="Categorical"){
    
    categorical_scale <- scale_trait_values$cat_scale
    n <- length(categorical_scale)
    scale_rule_first <- paste("value!=",categorical_scale[1], sep = "") #1st class of categorical trait
    scale_rule_global <- paste(scale_rule_first,"&&","value!=",categorical_scale[2:n]) %>%  paste(. ,collapse = " && ")
    
    #assign_trait <- paste("renderer_",trait," <- ",sep="") 
    render_trait <- render_categorical(scale_condition = scale_rule_global)
    #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
    #cat("\"",render_trait,"\"","\n"," ")
    out <- paste(render_trait)
  }
  
  if(tp=="none"){
    print("")
  }      
  
  return(out)
}
  
  