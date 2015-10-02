#' A Function to run the ContentStructure model to convergence for one dataset.
#' 
#' @useDynLib ContentStructure
#' @importFrom Rcpp evalCpp
#' @param Auth_Attr A dataframe with one row for each unique sender/reciever and containing atleast one column with the ID of each sender/reciever and any number of additional varaibles which will be ignored unless specified as a binary attribute for which the user would like to calculate mixing parameter estimates by specifying the mixing_variable.
#' @param Doc_Edge_Matrix A matrix with one row for each email and one column which records the index of the sender of the email (indexed from 1) followed by one column for each unique sender/receiver in the dataset.
#' @param Doc_Word_Matrix A matrix with one row for each email and one column for each unique word in the vocabulary that records the number of times each word was used in each document. 
#' @param Vocab A vector containing every unique term in the vocabulary an corresponding in length to the number of columns in the Doc_Word_Matrix.
#' @param main_iterations The number of iterations of Gibbs sampling for the LDA part of the model. We have found 4,000 seems to work well.
#' @param sample_step_burnin The number of iterations of burnin that should be completed before sampling the latent space parameters when running MH for the LSM to convergence.
#' @param sample_step_iterations The total number of iterations to run MH for the LSM for (before thinning).
#' @param sample_step_sample_every How many iterations to skip when thinning the MH for the LSM chain in our MH for the LSM sample step.
#' @param topics The number of topics to use
#' @param clusters The number of topic clusters to use.
#' @param latent_space_dimensions THe number of dimensions to be included in the latent space model. Note that plotting is only currently supported for two dimensions.
#' @param run_MH_only If TRUE, then we only rerun MH for the LSM to convergence
#' @param mixing_variable if not NULL, specifies the name of the binary variable in the author_attributes dataset that will be used to estimate mixing parameter effects.
#' @param Seed Sets the seed in R and C++ for replicability.
#' @param save_results_to_file A logical value indicating whether intermediate results should be saved to file or whether they will be return to the R session.
#' @param output_directory This is where all output will be saved. Defaults to NULL if save_results_to_file == FALSE.
#' @param output_filename The name of the .Rdata file you would like to save model output in. Defaults to NULL if save_results_to_file == FALSE.
#' @param Main_Estimation_Results A list object returned by previous model estimation to be supplied if the user wishes to select run_MH_only == TRUE. Useful if the user would like to specify a greater number of iterations for the final step of LSM estimation.
#' @return Does not return anything, just saves everything to our data_directory folder.
#' @export
Run_Full_Model <- function(Auth_Attr, 
                           Doc_Edge_Matrix,
                           Doc_Word_Matrix, 
                           Vocab,
                           main_iterations = 4000, 
                           sample_step_burnin = 2000000, 
                           sample_step_iterations = 8000000,
                           sample_step_sample_every = 2000,
                           topics = 10,
                           clusters = 2,
                           latent_space_dimensions = 2,
                           run_MH_only = F,
                           mixing_variable = NULL,
                           Seed = 123456,
                           save_results_to_file = FALSE,
                           output_directory = NULL,
                           output_filename = NULL,
                           Main_Estimation_Results = NULL
                           ){
    
    if(save_results_to_file){
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }
      
      lastchar <- substrRight(output_directory,1)
      
      if(lastchar == "/"){
        #we are all set
      }else{
        #add a trailing slash to we save to right place
        output_directory <- paste(output_directory,"/",sep = "")
      }
    }
    

    num_bin_mix_vars <- 0
    if(!is.null(mixing_variable)){
      num_bin_mix_vars <- 1
    }
    
    if(!run_MH_only){
        Results <- Run_Inference(
         Number_Of_Iterations = main_iterations, 
         Base_Alpha =0.1, 
         Base_Beta = 0.01, 
         Number_Of_Topics = topics, 
         Topic_Step_Itterations = 1, 
         Sample_Step_Itterations = 1000, 
         Proposal_Variance = 0.5, 
         seed = Seed, 
         Number_of_Clusters = clusters,
         Itterations_Before_Cluster_Assingment_Updates = 2,
         Adaptive_Metropolis_Target_Accept_Rate = 0.2,
         slice_sample_alpha_step_size = 1,
         MH_prior_standard_deviation = 5,
         Slice_Sample_Alpha = F,
         Number_of_Binary_Mixing_Parameters = num_bin_mix_vars, 
         Mixing_Variable = mixing_variable,
         Author_Attributes= Auth_Attr, 
         Document_Edge_Matrix = Doc_Edge_Matrix,
         Document_Word_Matrix = Doc_Word_Matrix, 
         Vocabulary = Vocab,
         Latent_Dimensions = latent_space_dimensions)
    }else{
      Results <- Main_Estimation_Results
    }
      
    Results <-  Run_MH_To_Convergence(
                  sample_step_burnin = sample_step_burnin,
                  itterations = sample_step_iterations,
                  sample_every = sample_step_sample_every, 
                  prop_var = 1,
                  set_proposal_variance = F,
                  adaptive_metropolis_update_every = 1000, 
                  use_adaptive_metropolis = 1, 
                  MH_prior_standard_deviation = 5 ,
                  seed = Seed,
                  Main_Estimation_Results = Results)
    
    # add in raw input information so that we can access it later
    Results$author_attributes = Auth_Attr
    Results$document_edge_matrix = Doc_Edge_Matrix
    Results$document_word_matrix = Doc_Word_Matrix
    Results$vocabulary = Vocab
    Results$mixing_variable = mixing_variable
    Results$latent_space_dimensions = latent_space_dimensions
    Results$num_topics = topics
    Results$num_actors = nrow(Auth_Attr)
    
    if(save_results_to_file){
      save(Results, file = paste(output_directory,output_filename,".Rdata",sep = ""))
    }
    return(Results)
}
