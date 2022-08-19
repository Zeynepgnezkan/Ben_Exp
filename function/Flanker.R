# Function for Flanker 


flanker <- function(data_dir = "Data/Flanker"){
  
  get_files<- function(dir= "", file_ext= ".csv"){
    
    if(dir== ""){
      dir= getwd()
    }
    
    # get a list of all file in dir:
    all_files<- list.files(dir)
    # remove non-asc files (if present)
    all_files<- all_files[grepl(file_ext, all_files)]
    
    # sort files by number in string
    get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
    num<- get_num(all_files) #change
    
    if(!is.na(num[1])){
      all_files<- all_files[order(num, all_files)]
    }
    # convert to directory string for each data file:
    if(length(all_files)>0){
      all_dirs<- NULL
      for(i in 1:length(all_files)){
        all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
      }
      
      message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
      return(all_dirs)
    }else{
      stop(paste("Found no", file_ext, "files in the specified directory!"))
    }
  } # end of get_files()
  data = NULL
  dataALL<- get_files(data_dir)
  for(i in 1:length(dataALL)){#each subj
    data1 <- read_csv(dataALL[i])
    data1 <- data1[ , -which(names(data1) %in% c("arrow_1","arrow_2","arrow_3","arrow_5","arrow_6","arrow_7","target",
                                              "practice.thisTrialN","practice.thisN","practice.thisIndex","practice.thisRepN",
                                              "trials.thisRepN","trials.thisTrialN","trials.thisN","trials.thisIndex",
                                              "trials_2.thisRepN","trials_2.thisTrialN","trials_2.thisN","trials_2.thisIndex",
                                              "trials_3.thisRepN","trials_3.thisTrialN","trials_3.thisN","trials_3.thisIndex",
                                              "instr_text.started","instr_text.stopped","instr_resp.keys","instr_resp.rt",
                                              "polygon.started","polygon.stopped",
                                              "arrow1_.started", "arrow1_.stopped","arrow2_.started", "arrow2_.stopped",
                                              "arrow3_.started", "arrow3_.stopped","arrow5_.started", "arrow5_.stopped",
                                              "arrow6_.started", "arrow6_.stopped","arrow7_.started", "arrow7_.stopped",
                                              "target_p.started", "target_p.stopped","key_resp_P.started","key_resp_P.stopped","key_resp_P.keys","key_resp_P.corr",
                                              "key_resp_P.rt","text_2.started","text_2.stopped","text_3.started","text_3.stopped",
                                              "text_4.started","text_4.stopped","key_resp_2.keys","key_resp_2.rt","key_resp_2.stopped",
                                              "key_resp_2.started","arrow1.started","arrow1.stopped","arrow2.started","arrow2.stopped",
                                              "arrow3.started","arrow3.stopped","arrow5.started","arrow5.stopped","arrow6.started","arrow6.stopped",
                                              "arrow7.started","arrow7.stopped","target_t.started","target_t.stopped","key_resp.started",
                                              "key_resp.stopped","text_5.started","text_5.stopped","key_resp_3.keys","key_resp_3.rt",
                                              "key_resp_3.started","key_resp_3.stopped","psychopyVersion","frameRate","date","session",
                                              "text_6.started","text_6.stopped","key_resp_4.keys","key_resp_4.rt",
                                              "key_resp_4.started","key_resp_4.stopped"
                                              ))]
    
    if(i == 1){
      data1 <- data1[ , -which(names(data1) %in% c("...93"))]
    }else{
      data1 <- data1[ , -which(names(data1) %in% c("...99"))]
    }
    
    #clean NAs
    data1 <- na.omit(data1)
    data<- rbind(data1, data)
  }#End Subj
  
  return(data)
  
}#END OF FUNC
