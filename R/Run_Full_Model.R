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
#' @return Does not return anything, just saves everything to our data_directory folder.
#' @export
Run_Full_Model <- function(data_name,  
                           main_iterations = 4000, 
                           sample_step_burnin = 2000000, 
                           sample_step_iterations = 8000000,
                           sample_step_sample_every = 2000,
                           topics = 40,
                           clusters = 4,
                           data_directory,
                           run_MH_only = F,
                           mixing_variable = NULL){
  
    
    
    #load data
    load( paste(data_directory,data_name,".Rdata", sep = ""))
    
    #assign global variables
    document_edge_matrix <<- document_edge_matrix
    document_word_matrix <<- document_word_matrix
    vocabulary <<- vocabulary
    author_attributes <<- author_attributes
    
    num_bin_mix_vars <- 0
    if(!is.null(mixing_variable)){
      num_bin_mix_vars <- 1
    }
    
    
    #print(ls())
    if(!run_MH_only){
        Results <- Run_Inference(Number_Of_Iterations = main_iterations, Base_Alpha =0.1, Base_Beta = 0.01, Number_Of_Topics = 50, Topic_Step_Itterations = 1, Sample_Step_Itterations = 1000, output_file = data_name ,Proposal_Variance = 0.5, seed = 1234, Number_of_Clusters = clusters ,Itterations_Before_Cluster_Assingment_Updates = 2, Adaptive_Metropolis_Target_Accept_Rate = 0.2, slice_sample_alpha_step_size = 1,MH_prior_standard_deviation = 5,Slice_Sample_Alpha = F,Number_of_Binary_Mixing_Parameters = num_bin_mix_vars, Mixing_Variable = mixing_variable)
    }
      
    Run_MH_To_Convergence(input_file = paste("Model_Output_",data_name,sep = "") ,data_source = data_name, output_file = paste("Sample_",data_name,sep = ""), data_directory = "~/Dropbox/PINLab/Projects/Denny_Working_Directory/2011_Analysis_Output/",sample_step_burnin = sample_step_burnin,itterations = sample_step_iterations,sample_every = sample_step_sample_every, prop_var = 1,set_proposal_variance = F,adaptive_metropolis_update_every = 1000, use_adaptive_metropolis = 1, MH_prior_standard_deviation = 5)

}
