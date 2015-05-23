#' A Function to run the ContentStructure model to convergence for one dataset.
#' 
#' @param data_name The exact name of the .Rdata file containing the relevant files necessary to run the model.
#' @param main_iterations The number of iterations of Gibbs sampling for the LDA part of the model. We have found 4,000 seems to work well.
#' @param sample_step_burnin The number of iterations of burnin that should be completed before sampling the latent space parameters when running MH for the LSM to convergence.
#' @param sample_step_iterations The total number of iterations to run MH for the LSM for (before thinning).
#' @param sample_step_sample_every How many iterations to skip when thinning the MH for the LSM chain in our MH for the LSM sample step.
#' @param topics The number of topics to use
#' @param clusters The number of topic clusters to use
#' @param data_directory The directory where our data_name file is stored. This is also where all output will be saved
#' @param run_MH_only If TRUE, then we only rerun MH for the LSM to convergence
#' @param mixing_variable if not NULL, specifies the name of the binary variable in the author_attributes dataset that will be used to estimate mixing parameter effects.
#' @param Auth_Attr A dataframe with one row for each unique sender/reciever and containing atleast one column with the ID of each sender/reciever and any number of additional varaibles which will be ignored unless specified as a binary attribute for which the user would like to calculate mixing parameter estimates by specifying the mixing_variable.
#' @param Doc_Edge_Matrix A matrix with one row for each email and one column which records the index of the sender of the email (indexed from 1) followed by one column for each unique sender/receiver in the dataset.
#' @param Doc_Word_Matrix A matrix with one row for each email and one column for each unique word in the vocabulary that records the number of times each word was used in each document. 
#' @param Vocab A vector containing every unique term in the vocabulary an corresponding in length to the number of columns in the Doc_Word_Matrix.
#' @param Seed Sets the seed in R and C++ for replicability
#' @return Does not return anything, just saves everything to our data_directory folder.
#' @export
Run_Full_Model <- function(data_name,  
                           main_iterations = 4000, 
                           sample_step_burnin = 2000000, 
                           sample_step_iterations = 8000000,
                           sample_step_sample_every = 2000,
                           topics = 10,
                           clusters = 2,
                           data_directory,
                           run_MH_only = F,
                           mixing_variable = NULL,
                           Auth_Attr = author_attributes, 
                           Doc_Edge_Matrix = document_edge_matrix ,
                           Doc_Word_Matrix = document_word_matrix, 
                           Vocab = vocabulary,
                           Seed = 1234){
  
    
    
    #load data
    #load( paste(data_directory,data_name,".Rdata", sep = ""))
    
    #assign global variables
#     document_edge_matrix <<- document_edge_matrix
#     document_word_matrix <<- document_word_matrix
#     vocabulary <<- vocabulary
#     author_attributes <<- author_attributes
    
    num_bin_mix_vars <- 0
    if(!is.null(mixing_variable)){
      num_bin_mix_vars <- 1
    }
    
    
    #print(ls())
    if(!run_MH_only){
        Results <- Run_Inference(Number_Of_Iterations = main_iterations, 
                                 Base_Alpha =0.1, 
                                 Base_Beta = 0.01, 
                                 Number_Of_Topics = topics, 
                                 Topic_Step_Itterations = 1, 
                                 Sample_Step_Itterations = 1000, 
                                 output_file = data_name,
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
                                 output_folder_path = data_directory,
                                 output_file = data_name)
    }
      
    Run_MH_To_Convergence(input_file = paste("Model_Output_",data_name,sep = ""),
                          output_file = paste("Sample_",data_name,sep = ""),
                          sample_step_burnin = sample_step_burnin,
                          itterations = sample_step_iterations,
                          sample_every = sample_step_sample_every, 
                          prop_var = 1,
                          set_proposal_variance = F,
                          adaptive_metropolis_update_every = 1000, 
                          use_adaptive_metropolis = 1, 
                          MH_prior_standard_deviation = 5 ,
                          data_dir = data_directory ,
                          seed = Seed)

}
