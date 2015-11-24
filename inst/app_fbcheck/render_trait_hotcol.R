rhandsontable::rhandsontable(data = fieldbook,readOnly = FALSE,useTypes = TRUE) %>%  
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
