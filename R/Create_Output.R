#' A Function to generate diagnostic plots and interpretable output for a list of 1 to N model output objects from running the ContentStructure model.
#' 
#' @param data_list A vector containing the names of organization data files as in the function to run the model. Should only contain names of those organizations for which output has already been created. 
#' @param only_generate_summaries If TRUE, then only generate a one-page-per-cluster pdf summary of model output for each county, otherwise generate a ton of output.
#' @param data_directory The directory where all .Rdata files are stored -- also where all output will be stored.
#' @param print_agg_stats If TRUE, then will print aggregate level stats for the county across all cluster simultaneously. I have not found this to be terribly useful.
#' @param using_county_email_data Logical if you are using North Carolina County Government email data that are properly formatted to produce aggregate level output. 
#' @param Topic_Model_Burnin The number of iterations of Gibbs sampling that should be discarded before calculated Geweke statistic to determine model convergence. You will simply want to set it pretty low and then look at the trace to determine where you should set it to provide evidence of convergence.
#' @param Thin The number of iterations to skip in the MH for LSM chain when generating output. Set to 1 as default does not thin but can be set higher to make plotting easier if you took a lot of samples.
#' @param Skip The number of MH for LSM iterations to skip when generating out (if your burnin was not long enough).
#' @return A list object with the following structure. The first entry is a sublist which contains three objects: a vector containing the full vocabulary, an email-word matrix aggregated across all organizations, and a metadata data frame -- again aggregated across all emails in all organizations. The second entry in the outer list is a list with one entry for each organization with a sublist containing data about that organization and emails sent within it. Only returns if you are using North Carolina County Government Email data. 
#' @export
Create_Output <- function(data_list,
                          only_generate_summaries = T, 
                          data_directory ,
                          print_agg_stats = F,
                          using_county_email_data = F,
                          Topic_Model_Burnin = 50,
                          Skip = 0, 
                          Thin = 1){
  
  setwd(data_directory)
  #     source("./Scripts/CPME_Generate_Diagnostics.R")
  #     library(slam)
  
  #loop over all counties in list:
  for(i in 1:length(data_list)){
    cat("Current County:",data_list[i],"--",i,"of",length(data_list), " \n")
    #load data
    load( paste(data_directory,data_list[i],".Rdata", sep = ""))
    
    if(using_county_email_data){
      temp <- Generate_Model_Diagnsotics(input_file = paste("Sample_",data_list[i],sep = "") ,
                                         LS_Actor = 8, 
                                         out_directory = data_directory, 
                                         vocab = vocabulary, 
                                         output_name = paste("Output_",data_list[i],sep = ""),
                                         Thin_Itterations = Thin,
                                         skip_first = Skip,
                                         Author_Attributes = author_attributes,
                                         proportion_in_confidence_contour  = 0.9, 
                                         topic_model_burnin = Topic_Model_Burnin, 
                                         pretty_name = data_list[i],
                                         only_print_summaries = only_generate_summaries,
                                         print_agregate_level_stats = print_agg_stats, 
                                         used_county_email_data = using_county_email_data
                                         )
      
      #testing
      #temp <- test[[1]]
      #temp <- test[[2]]
      #if we are on the first iteration, make the return object equal to temp, oterwise, append
      if(i == 1){
        county_data <- list(temp)
        word_mat <- temp[[3]]
        metadata <- word_mat[,1:3]
        len <- length(word_mat[1,])
        nr <-nrow(word_mat[,14:len])
        nc <-ncol(word_mat[,14:len])
        word_mat <- matrix(as.numeric(word_mat[,14:len]),nrow = nr, ncol= nc)
        all_vocab <- temp[[4]]
        all_words <- as.simple_triplet_matrix(word_mat)
      }else{
        county_data <- append(county_data,list(temp))
        word_mat <- temp[[3]]
        metadata <- rbind(metadata,word_mat[,1:3])
        len <- length(word_mat[1,])
        nr <-nrow(word_mat[,14:len])
        nc <-ncol(word_mat[,14:len])
        word_mat <- matrix(as.numeric(word_mat[,14:len]),nrow = nr, ncol= nc)
        
        cur_vocab <- temp[[4]]
        new_vocab <- unique(rbind(all_vocab,cur_vocab))
        #the new words that the current vocab is adding
        addition <- new_vocab[-(1:length(all_vocab[,1])),]
        #create a block of zeros to append to the right side of the existing matrix (becasue those documents do not use the new words)
        add_word_matrix <- simple_triplet_zero_matrix(nrow =nrow(all_words),ncol= length(addition))
        #stick them together
        all_words <- cbind(all_words,add_word_matrix)
        
        #now create a new matrix with all the right row indicies to 
        current_word_matrix <- simple_triplet_zero_matrix(nrow = nrow(word_mat),ncol= length(new_vocab[,1]))
        for(j in 1:length(cur_vocab[,1])){
          index <- which(new_vocab[,1] == cur_vocab[j,1])
          current_word_matrix[,index] <- word_mat[,j]
        }
        
        #update
        all_words <- rbind(all_words,current_word_matrix)
        all_vocab <- new_vocab
        tv <- unlist(all_vocab[,1])
        colnames(all_words) <- tv
      }
    }else{
      #if we are not using the county email dataset
      Generate_Model_Diagnsotics(input_file = paste("Sample_",data_list[i],sep = "") ,LS_Actor = 8, out_directory = "~/Dropbox/PINLab/Projects/Denny_Working_Directory/2011_Analysis_Output/", vocab = vocabulary, output_name = paste("Output_",data_list[i],sep = ""), Thin_Itterations = 1,skip_first = 2000,Author_Attributes = author_attributes,Clusters_to_Pretty_Print=c(1,2,3,4),proportion_in_confidence_contour  = 0.9, topic_model_burnin = 2500, pretty_name = data_list[i],only_print_summaries = only_generate_summaries,print_agregate_level_stats = print_agg_stats,used_county_email_data = using_county_email_data)
    }
      
    
    
    }
  
  if(using_county_email_data){
    token_master_data <- list(tv,all_words,metadata)
    toreturn <- list(token_master_data,county_data)
  }else{
    toreturn  <- "nothing to return"
  }
    
  return(toreturn)
}