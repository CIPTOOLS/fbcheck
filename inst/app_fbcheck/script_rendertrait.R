
print_trait_hotcol <- function(drtotal,nt,datadict,trait){

  sink("render_trait_hotcol1.R")
  cat( "rhandsontable::rhandsontable(data = fieldbook,readOnly = FALSE,useTypes = TRUE) %>% ","\n")
  for(i in 1:nt){
    
    if(i!=nt){    
      cat(paste("hot_col(col = '",trait[i],"' ,readOnly = FALSE,
                allowInvalid = TRUE,copyable = TRUE, renderer =",paste("renderer_",trait[i],
                                                                       sep="",")")," %>% ","\n" ,sep=""))
    } 
    if(i==nt){
      cat(paste("hot_col(col = '",trait[i],"' ,readOnly = FALSE,
                allowInvalid = TRUE,copyable = TRUE, renderer =",paste("renderer_",trait[i],
                                                                       sep="",")"),"\n" ,sep=""))
    }
    
  }
  sink()
  
}

