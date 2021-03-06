#' A Function to generate diagnostic plots and interpretable output for a list of 1 to N model output objects from running the ContentStructure model.
#'
#' @param output_names An optional (vector) of file names that can be used to name the output .pdf files generated by this function (if save_results == TRUE) . Useful if you would like your output files to look like Org_Plot_X.pdf instead of Org_Results_of_9-30-15_Plot_X.pdf, for example.  
#' @param Estimation_Results A list object returned by Run_Full_Model that will be used to generate output. Can be NULL if load_results_from_file == TRUE in which case intermediate results saved to disk will be used instead. 
#' @param only_generate_summaries If TRUE, then only generate a one-page-per-cluster pdf summary of model output for each county, otherwise generate a ton of output.
#' @param print_agg_stats If TRUE, generates a plot comapring topics frequency across all clusters and a trace plot of th topic model log likelihood -- very useful.
#' @param Topic_Model_Burnin The number of iterations of Gibbs sampling that should be discarded before calculated Geweke statistic to determine model convergence. You will simply want to set it pretty low and then look at the trace to determine where you should set it to provide evidence of convergence.
#' @param Skip The number of MH for LSM iterations to skip when generating out (if your burnin was not long enough).
#' @param Thin The number of iterations to skip in the MH for LSM chain when generating output. Set to 1 as default does not thin but can be set higher to make plotting easier if you took a lot of samples.
#' @param load_results_from_file A logical which defaults to FALSE. If TRUE, then the function will load data named by data_name output from Run_Full_Model (note that save_results_to_file must be set to TRUE in this function) in the data_directory and use this to generate output. 
#' @param estimation_output_list A vector containing the names of organization data files as in the function to run the model. Should only contain names of those organizations for which output has already been created. Only for use with county government datasets (not for public use).
#' @param estimation_output_directory The directory where all .Rdata files generated by Run_Full_Model() are stored. Defaults to NULL if we are not reading in any intermediate data from disk. 
#' @param using_county_email_data Logical if you are using North Carolina County Government email data that are properly formatted to produce aggregate level output. 
#' @param save_results  Defaults to FALSE, if TRUE, then output_names and output_directory must be supplied and .pdfs will be created for all plots. 
#' @param output_directory A directory where we wish to save the output of this function.
#' @return A list object with 4 entries and the following structure: Cluster_Data contains cluster level data including top words and mixing parameters (with standard errors) if applicable. Actor_Data contains actors level data (all of the Auth_Attr dataframe) plus average latent positions for each actor in each dimension (2 currently), for each cluster. Token_Data contains the counts of each token for each topic, along with the edge counts for that topic and the cluster assignment for it. Vocabulary simply holds the vocabulary as a vector for easy handling.
#' @export
Create_Output <- function(output_names = NULL,
                          Estimation_Results = NULL,
                          only_generate_summaries = T, 
                          print_agg_stats = T,
                          Topic_Model_Burnin = 50,
                          Skip = 0, 
                          Thin = 1,
                          load_results_from_file = F,
                          estimation_output_list = NULL,
                          estimation_output_directory = NULL,
                          using_county_email_data = F,
                          save_results = F,
                          output_directory = NULL
                          ){
  
  if(save_results){
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
    lastchar <- substrRight(estimation_output_directory,1)
    
    if(lastchar == "/"){
      #we are all set
    }else{
      #add a trailing slash to we save to right place
      estimation_output_directory <- paste(estimation_output_directory,"/",sep = "")
    }
    
    lastchar2 <- substrRight(output_directory,1)
    
    if(lastchar2 == "/"){
      #we are all set
    }else{
      #add a trailing slash to we save to right place
      output_directory <- paste(output_directory,"/",sep = "")
    }
    
    #if no pretty output names were provided
    if(is.null(output_names)){
      if(using_county_email_data){
        output_names <- estimation_output_list
      }else{
        output_names <- "Test"
      }
    }
    
    setwd(output_directory)
  }
  #loop over all counties in list:
  if(using_county_email_data){
    for(i in 1:length(estimation_output_list)){
      
      #load data
      if(load_results_from_file){
        cat("Current County:",estimation_output_list[i],"--",i,"of",length(estimation_output_list), " \n")
        before <- ls()
        load(paste(estimation_output_directory,estimation_output_list[i],".Rdata", sep = ""))
        after <- ls()
        after <- after[-which(after == "before")]
        estimation_name <- which(after %in% before == FALSE)
        Estimation_Results <- get(after[estimation_name])
        rm(list = after[estimation_name])
      }else{
        Estimation_Results <- estimation_output_list[[i]]
      }

      temp <- Generate_Model_Diagnsotics(
                LS_Actor = 8, 
                out_directory = output_directory, 
                output_name = output_names[i],
                Thin_Itterations = Thin,
                skip_first = Skip,
                proportion_in_confidence_contour  = 0.9, 
                topic_model_burnin = Topic_Model_Burnin, 
                only_print_summaries = only_generate_summaries,
                print_agregate_level_stats = print_agg_stats, 
                used_county_email_data = using_county_email_data,
                Estimation_Results = Estimation_Results,
                save_results = save_results
      )
      
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
        all_words <- slam::as.simple_triplet_matrix(word_mat)
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
        add_word_matrix <- slam::simple_triplet_zero_matrix(nrow =nrow(all_words),ncol= length(addition))
        #stick them together
        all_words <- cbind(all_words,add_word_matrix)
        
        #now create a new matrix with all the right row indicies to 
        current_word_matrix <- slam::simple_triplet_zero_matrix(nrow = nrow(word_mat),ncol= length(new_vocab[,1]))
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
      
    }#end of loop
  }else{
   
    #if we are not using the county email dataset we can only go one at a time
    toreturn <- Generate_Model_Diagnsotics(
                   LS_Actor = 8, 
                   out_directory = output_directory, 
                   output_name =  output_names[1],
                   Thin_Itterations = Thin,
                   skip_first = Skip,
                   proportion_in_confidence_contour  = 0.9, 
                   topic_model_burnin = Topic_Model_Burnin, 
                   only_print_summaries = only_generate_summaries,
                   print_agregate_level_stats = print_agg_stats, 
                   used_county_email_data = using_county_email_data,
                   Estimation_Results = Estimation_Results,
                   save_results = save_results
                   )
  
  # now we clean up the intermediate datasets and save everything
  }
  
  if(using_county_email_data){
    token_master_data <- list(tv,all_words,metadata)
    toreturn <- list(token_master_data,county_data)
  }else{
    cat("Returning Dataset \n")
  }
    
  return(toreturn)
}