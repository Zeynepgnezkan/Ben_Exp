

preproc<- function(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test", maxtrial=1450, ResX=1920, DPP= 0.0247){
  options(scipen=999)
  
  #steal some fun from my Professor,
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  # get a list of .asc files from a user-provided directory
  get_files<- function(dir= "", file_ext= ".asc"){
    
    if(dir== ""){
      dir= getwd()
    }
    
    # get a list of all file in dir:
    all_files<- list.files(dir)
    # remove non-asc files (if present)
    all_files<- all_files[grepl(file_ext, all_files)]
    # remove txt files (of present):
    all_files<- all_files[!grepl(".txt", all_files)]
    
    # sort files by number in string
    get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
    num<- get_num(all_files)
    
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
  
  
  # check if user provided data dir:
  if(length(data_dir)==0){
    data_dir= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_dir'")
  }
  
  # Get data file names:
  dataASC<- get_files(data_dir)
  

  data<- NULL
  #fixations <- NULL
  ## PROCESS ##
  
  for(i in 1:length(dataASC)){ # for each subject..
    
    cat("\n"); cat(sprintf("Loading data file: %s", dataASC[i]))
    dataF<- readLines(dataASC[i]) # load asc file;
    cat(". Done"); cat("\n")
    
    library(stringr)
    
    ### get trial names:
    ID<- which(grepl('TRIALID', dataF));
    trial_text<- dataF[ID]
    trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
    trials<- gsub(" ", "", trials)
    itemN <- as.numeric(str_match(trials, pattern = '\\d{1,3}'))
    cond <- str_match(trials, pattern = '_(\\w{3,9})')[,2]
    
### get start and end times ###
    
    start<- which(grepl('DISPLAY ON', dataF))
    trial_start_t<- which(grepl('start_trial', dataF))

    end<- which(grepl('DISPLAY OFF', dataF))
    trial_end_t  <- which(grepl('stop_trial', dataF))
   

    
    trial_db<- data.frame(cond, itemN , start, end, ID, trial_start_t,trial_end_t)

    trial_db$filename<- dataASC[i]
    
    curr_file<- unlist(strsplit(dataASC[i], '/'))
    curr_file<- curr_file[length(curr_file)]
    curr_file_split<- unlist(strsplit(curr_file, '_'))
    curr_file_split<- curr_file_split[2]
    trial_db$subject<- get_num(curr_file_split)
    trial_db$seq <- 1:nrow(trial_db)
    
    
    ntrials<- nrow(trial_db)
    cat(sprintf("Processing trial: "));
    for(j in 1:ntrials){
      temp<- data.frame(sub=NA, item=NA, cond=NA, seq = NA, trial_start = NA, trial_end = NA,
                        target_word_n = NA,target_word = NA, target_changed = NA,Dis_on_t = NA,
                        Dis_off_t = NA, boundary = NA, DC_start_t = NA, DC_end_t = NA,
                        boundary_t = NA ,Display_time = NA, Display_lat = NA,
                        sacc_start_t = NA, sacc_end_t = NA, sacc_dur = NA, sacc_start_x = NA,
                        sacc_end_x = NA, sacc_ampl = NA,peak_vel = NA, avg_vel = NA, blink = NA, blink_dur = NA,
                        fix_start_t = NA, fix_end_t = NA, fix_dur = NA, question = NA,corrAns = NA,
                        key_resp = NA, RT_q = NA, accuracy = NA,corr_sacc = NA, jhook = NA)
                        
                        #corrsacc_dur = NA, corrsacc_ampl = NA, corrsacc_lat = NA)
                        
    
               
      cat(toString(j)); cat(" ")
      db<- trial_db[j,]
      trialF<- dataF[db$trial_start_t:db$trial_end_t]
      trialInfo<- dataF[db$ID:db$start]
      
      # generic info about trial
      temp$sub<- db$subject
      temp$item<- trial_db$itemN[j]
      temp$cond<- trial_db$cond[j]
      temp$seq<- trial_db$seq[j]
      temp$trial_start<- get_num(trialF[1])
      temp$trial_end<- get_num(trialF[length(trialF)])
      
      temp$Dis_on_t <- get_num(trialF[which(grepl('DISPLAY ON', trialF))])
      temp$Dis_off_t <- get_num(trialF[which(grepl('DISPLAY OFF', trialF))])
      
      #target word and number and displayed one
      target_word_number <- trialF[which(grepl('target_word_nr', trialF))]
      target_word_number <- as.data.frame(do.call( rbind, strsplit(target_word_number, ' ' )))
      temp$target_word_n <- target_word_number$V4
      
      target_word_str <- trialF[which(grepl('var target ', trialF))]
      target_word_str <- as.data.frame(do.call( rbind, strsplit(target_word_str, ' ' )))
      temp$target_word <- target_word_str$V4
      
      #boundary
      DC_boundary <- trialF[which(grepl('var boundary ', trialF))]
      DC_boundary <- as.data.frame(do.call( rbind, strsplit(DC_boundary, ' ' )))
      temp$boundary <- DC_boundary$V4
      
      #boundary time
      x <- which(grepl(temp$Dis_on_t,trialF))
      y <- which(grepl(temp$Dis_off_t,trialF))
      
      trialShort<- trialF[x[1]:y[1]]
      samples <- trialShort
      samples <- samples[!grepl("EFIX", samples)]
      samples <- samples[!grepl("SFIX", samples)]
      samples <- samples[!grepl("ESACC", samples)]
      samples <- samples[!grepl("SSACC", samples)]
      samples <- samples[!grepl("MSG", samples)]
      samples <- samples[!grepl("EBLINK", samples)]
      samples <- samples[!grepl("SBLINK", samples)]
      samples <-  as.data.frame(do.call( rbind, strsplit( samples, '\t' ) ))
      samples$V2<- as.numeric(samples$V2)
      samples <- subset(samples, V2 > as.numeric(temp$boundary))
      
      temp$boundary_t <- as.numeric(samples$V1[1])
      
      if(temp$cond == 'ben'){
      target_word_chg <- trialF[which(grepl('var changed ', trialF))]
      target_word_chg <- as.data.frame(do.call( rbind, strsplit(target_word_chg, ' ' )))
      temp$target_changed <- target_word_chg$V4
      
      # Display change infos 
          
      temp$DC_start_t <- get_num(trialF[which(grepl('DC started', trialF))])
      
      DC_end_t <- trialF[which(grepl('DC completed', trialF))]
      DC_end_t <- as.data.frame(do.call( rbind, strsplit(DC_end_t, ' ' )))
      temp$Display_time <- round(as.numeric(DC_end_t$V5),3)
      temp$DC_end_t <- get_num(DC_end_t$V1)
      
      # Display latency
      
      
      
      temp$Display_lat <- temp$DC_start_t - temp$boundary_t
      
      } #end of if ben
    
      # Extract all saccade events from trial file:
      # all E flags:
      EsaccFlags<- which(grepl('ESACC', trialF))
      EsaccStrings<- trialF[EsaccFlags]
      
      sacc<- as.data.frame(do.call( rbind, strsplit( EsaccStrings, '\t' ) ))
      sacc$V1<- get_num(sacc$V1)
      
      
      #get between display on and off
      sacc<- subset(sacc, V1>= temp$Dis_on_t)
      sacc<- subset(sacc, V1<= temp$Dis_off_t)
      #get only the ones cross boundary
      sacc_cross <- subset(sacc, as.numeric(sacc$V6) > as.numeric(temp$boundary))
      
      #boundary cross saccade infos
      
      temp$sacc_start_t <- as.numeric(sacc_cross$V1[1])
      temp$sacc_end_t <- as.numeric(sacc_cross$V2[1])
      temp$sacc_dur <- temp$sacc_end_t - temp$sacc_start_t
      temp$sacc_start_x <- as.numeric(sacc_cross$V4[1])
      temp$sacc_end_x <- as.numeric(sacc_cross$V6[1])
      temp$sacc_ampl<- abs(temp$sacc_end_x - temp$sacc_start_x)*DPP
      
      
      #Blink
      # Between start of trial and end of the first saccade crosses the boundary 

      
      #### maybe we can change it as end of the previous fixation before crossing boundary and
      #### end time of first saccade who crosses boundary
      
      x <- which(grepl(temp$sacc_start_t,trialF))
      y <- which(grepl(temp$sacc_end_t,trialF))
      
      trialShort<- trialF[x[1]:y[1]]
      samples <- trialShort
      samples <- samples[!grepl("EFIX", samples)]
      samples <- samples[!grepl("SFIX", samples)]
      samples <- samples[!grepl("ESACC", samples)]
      samples <- samples[!grepl("SSACC", samples)]
      samples <- samples[!grepl("MSG", samples)]
      samples <- samples[!grepl("SBLINK", samples)]
      samples <-  as.data.frame(do.call( rbind, strsplit( samples, '\t' ) ))
      samples$V4<- as.numeric(samples$V4)
      
      blinkzero <- which(samples$V4==0)
      
      if (length(blinkzero) == 0){
        temp$blink <- 'NO'
      }else{
        temp$blink <- 'YES'
      }
      
      #blink duration
      if(temp$blink == 'YES'){
        blinkEFlag <- which(grepl('EBLINK',trialShort))
        blinkEString <- as.data.frame(do.call( rbind, strsplit( trialShort[blinkEFlag], '\t' ) ))
        blinkEString$V1 <- get_num(blinkEString$V1)
        temp$blink_dur<- as.numeric(blinkEString$V2) - blinkEString$V1
        
      }
      
      #fixations
      EFixFlags<- which(grepl('EFIX', trialF))
      EFixStrings<- trialF[EFixFlags]
      
      all_fix <-as.data.frame(do.call( rbind, strsplit( EFixStrings, '\t' ) ))
      all_fix$V1 <- get_num(all_fix$V1)
      
      #fixations after display on and boundary
      all_fix <- subset(all_fix, V1 > as.numeric(temp$Dis_on_t))
      all_fix1 <- subset(all_fix, as.numeric(all_fix$V4) > as.numeric(temp$boundary))
      
      #first fix after boundary
      temp$fix_start_t <- as.numeric(all_fix1$V1[1])
      temp$fix_end_t <- as.numeric(all_fix1$V2[1])
      temp$fix_dur <- temp$fix_end_t - temp$fix_start_t

      
      #Questions
      Question <- trialF[which(grepl('var question ', trialF))]
      Question <- as.data.frame(do.call( rbind, strsplit(Question, ' ' )))
      if(Question$V4 != 'NA'){
        temp$question <- 'Yes'
      }else{
        temp$question <- 'No'
      }
      
      if(temp$question == 'Yes'){
        
        #correct answer
        corrAns_q <- trialF[which(grepl('var response ', trialF))]
        corrAns_q <- as.data.frame(do.call( rbind, strsplit(corrAns_q, ' ' )))
        temp$corrAns <- corrAns_q$V4
        
        #key response                          
        Q_resp_key <- trialF[which(grepl('var response_keyboard_response ', trialF))]
        Q_resp_key <- as.data.frame(do.call( rbind, strsplit(Q_resp_key, ' ' )))
        temp$key_resp <- Q_resp_key$V4
        
        #RT
        RT_ques <- trialF[which(grepl('var response_time ', trialF))]
        RT_ques <- as.data.frame(do.call( rbind, strsplit(RT_ques, ' ' )))
        temp$RT_q <- round(as.numeric(RT_ques$V4),3)
        
        #accuracy
        if(temp$corrAns == 'right'){
          if(temp$key_resp == 'right'){
            temp$accuracy <- 1
          }else{
            temp$accuracy <- 0
          }
        }else{
          if(temp$corrAns == 'left'){
            if(temp$key_resp == 'left'){
            temp$accuracy <- 1
          }else{
            temp$accuracy <- 0
          }
        }
      }  

        
      }#end of question if
      
      ## Velocity 
      sacc_st<- which(grepl(temp$sacc_start_t, trialF))
      sacc_st<- sacc_st[1] 
      sacc_end<- which(grepl(temp$sacc_end_t, trialF))[1]
      sacc_samples<- trialF[sacc_st:sacc_end]
      
      # remove flags from samples data:
      sacc_samples<- sacc_samples[!grepl("EFIX", sacc_samples)]
      sacc_samples<- sacc_samples[!grepl("ESACC", sacc_samples)]
      sacc_samples<- sacc_samples[!grepl("SSACC", sacc_samples)]
      sacc_samples<- sacc_samples[!grepl("MSG", sacc_samples)]
      sacc_samples <-  as.data.frame(do.call( rbind, strsplit( sacc_samples, '\t' ) )) # V2 is xpos
      
      x= as.numeric(as.character(sacc_samples$V2)) # x pos vector
      vel <- rep(0, length(x))
      vel[2:(length(x)-1)] <- abs(1000/2*(x[3:(length(x))] - x[1:(length(x)-2)])/(1/DPP))
      temp$peak_vel<- max(abs(vel))
      temp$avg_vel<- mean(abs(vel))
      
      #Corrective Sacc 
      begin <- which(grepl(temp$sacc_end_t,trialF))
      end <- which(grepl(temp$Dis_off_t,trialF))
      TrialS <- trialF[begin[1]:end[1]]
      TrialS <- TrialS[!grepl("EFIX", TrialS)]
      TrialS <- TrialS[!grepl("SFIX", TrialS)]
      TrialS <- TrialS[!grepl("ESACC", TrialS)]
      TrialS <- TrialS[!grepl("SSACC", TrialS)]
      TrialS <- TrialS[!grepl("MSG", TrialS)]
      TrialS <- TrialS[!grepl("END", TrialS)]
      
      TrialS <-  as.data.frame(do.call( rbind, strsplit( TrialS, '\t' ) )) 
      
      if(temp$cond == 'ben'){
        corrsacc <- which(TrialS$V2 < as.numeric(temp$boundary))
        if(length(corrsacc) != 0){
          subX <- subset(sacc_cross, as.numeric(sacc_cross$V2) > temp$sacc_end_t)
          subY <- subset(TrialS, as.numeric(TrialS$V2) < as.numeric(temp$boundary))
          subX <- subset(sacc_cross, as.numeric(sacc_cross$V6) < as.numeric(temp$boundary))
          temp$corr_sacc <- 'Yes'
        }
      }

      ## J Hook ##
      
     #  for(h in 1:nrow(sacc)){
     #    #sacc1
     #    begin <- which(grepl(sacc$V1[h],trialF))
     #    end <- which(grepl(sacc$V2[h],trialF))
     #    TrialS <- trialF[begin[1]:end[1]]
     #    TrialS <- TrialS[!grepl("SBLINK", TrialS)]
     #    TrialS <- TrialS[!grepl("MSG", TrialS)]
     #    TrialS <- TrialS[!grepl("SSAC", TrialS)]
     #    TrialS <- TrialS[!grepl("ESAC", TrialS)]
     #    TrialS <- TrialS[!grepl("EBLINK", TrialS)]
     #    TrialS <-  as.data.frame(do.call( rbind, strsplit( TrialS, '\t' ) ))
     #    blinkcheck <- which(as.numeric(TrialS$V4) == 0)
     #    boundarycheck <- which(as.numeric(TrialS$V2) > as.numeric(temp$boundary))
     #    
     #    library(dplyr)
     #    
     # for(g in 1:nrow(TrialS)){
     #  if(as.numeric(sacc$V6[h]) > as.numeric(sacc$V4[h])){
     #   if((length(blinkcheck) == 0) && (!is.na(TrialS$V2[g+1])) && (length(boundarycheck) != 0)){
     #     TrialS2 <- subset(TrialS, as.numeric(TrialS$V2) > as.numeric(temp$boundary))
     #     if((as.numeric(TrialS2$V2[g]) > as.numeric(TrialS2$V2[g+1])) && (!is.na(TrialS2$V2[g+1]))){
     #       temp$jhook <- 'Yes'
     #       break;
     #   }
     # }
     #  }
     #   
     # 
     #     }#end for trials
     #    }#sacc for end
     # 
      
      #get the first sacc cross boundary
      
         begin <- which(grepl(sacc_cross$V1[1],trialF))
         end <- which(grepl(sacc_cross$V2[1],trialF))
         TrialS <- trialF[begin[1]:end[1]]
         TrialS <- TrialS[!grepl("SBLINK", TrialS)]
         TrialS <- TrialS[!grepl("MSG", TrialS)]
         TrialS <- TrialS[!grepl("SSAC", TrialS)]
         TrialS <- TrialS[!grepl("ESAC", TrialS)]
         TrialS <- TrialS[!grepl("EBLINK", TrialS)]
         TrialS <-  as.data.frame(do.call( rbind, strsplit( TrialS, '\t' ) ))
         blinkcheck <- which(as.numeric(TrialS$V4) == 0)
         boundarycheck <- which(as.numeric(TrialS$V2) > as.numeric(temp$boundary))
         
      for(g in 1:nrow(TrialS)){
        if((as.numeric(sacc_cross$V6[1]) > as.numeric(sacc_cross$V4[1])) && 
           temp$cond == 'ben' &&
           (length(blinkcheck) == 0) &&
           (!is.na(TrialS$V2[g+1])) &&
           (length(boundarycheck) != 0)){
          
          TrialS2 <- subset(TrialS, as.numeric(TrialS$V2) > as.numeric(temp$boundary))
          
          if((as.numeric(TrialS2$V2[g]) > as.numeric(TrialS2$V2[g+1])) && (!is.na(TrialS2$V2[g+1]))){
            temp$jhook <- 'Yes'
            break;
          
        }
      }
    }#end for trials
      
      
      
      
      
      data<- rbind(data, temp)
      
      
    }
  
  }
 
  return(data)
  
}
