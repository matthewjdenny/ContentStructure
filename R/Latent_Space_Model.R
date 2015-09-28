Latent_Space_Model <- function(MH_iterations,
                               sample_every, 
                               burnin,
                               proposal_variance,
                               MH_prior_standard_deviation, 
                               LS_Dimensions = 2,
                               seed = 12345,
                               adaptive_metropolis_updates = NULL,
                               input_sociomatrix = NULL,
                               input_incidence_matrix = NULL,
                               attribute_data = NULL,
                               mixing_variables = NULL){
  
  # set the seed
  set.seed(seed)

  # clusters  = 1 since we are running on a single latent space.
  Clusters <-1
  
  # check to see what kind of data we have and if it is valid
  if(is.null(input_sociomatrix) & is.null(input_incidence_matrix)){
    stop("You must supply either an input_sociomatrix or input_incidence_matrix to estimate a latent space model!")
  }
  
  if(is.null(attribute_data) & !is.null(mixing_variables)){
    stop("You must supply attribute_data which contains columns named equivalently to all mixing_variables provided.")
  }
  
    
  clp <- array(0,c(Latent_Spaces,Clusters,Actors))
  
  cat("Running Model... \n")
  
  Result_List <- MH_Sampler(itterations,
                            as.integer(Actors), 
                            as.integer(Topics),
                            as.integer(Latent_Spaces),
                            Proposal_Variances,
                            Cluster_Topic_Assignments,
                            Topic_Present_Edge_Counts,
                            Topic_Absent_Edge_Counts,
                            LSPs,
                            clp,
                            Intercepts,
                            Betas,
                            Number_of_Betas,
                            Beta_Indicator_Array,
                            sample_step_burnin,
                            as.integer(Clusters),
                            sample_every,
                            adaptive_metropolis_update_every,
                            use_adaptive_metropolis,
                            MH_prior_standard_deviation,
                            seed)
  
  Main_Estimation_Results[[9]] <- Result_List[[1]]
  Main_Estimation_Results[[15]] <- Result_List[[5]]
  Main_Estimation_Results[[16]] <- Result_List[[7]]
  Main_Estimation_Results[[17]] <- Result_List[[6]]
  Main_Estimation_Results[[18]] <- Result_List[[4]]
  Main_Estimation_Results[[19]] <- Result_List[[2]]
  Main_Estimation_Results[[20]] <- Result_List[[3]]
  
  #assign names to list object
  names(Main_Estimation_Results) <- c("token_topic_assignments",
                                      "topic_present_edge_counts",
                                      "topic_absent_edge_counts",
                                      "token_type_topic_counts",
                                      "nothing_will_eventually_remove",
                                      "number_of_documents",
                                      "number_of_iterations",
                                      "Gibbs_per_iteration_will_eventually_remove",
                                      "number_of_LSM_MH_iterations", 
                                      "number_of_clusters",
                                      "cluster_proposal_variances",
                                      "cluster_accept_rates",
                                      "LDA_log_likelihood_trace",
                                      "topic_cluster_assignments",
                                      "LSM_cluster_intercepts",
                                      "LSM_cluster_mixing_parameters",
                                      "LSM_actor_latent_positions",
                                      "cluster_whether_accepted",
                                      "cluster_proposed_likelihoods",
                                      "cluster_current_likelihoods",
                                      "number_possible_mixing_parameter_values",
                                      "mixing_parameter_type_indicator_array",
                                      "intial_mixing_parameter_values")
  
  clust_assigns <- Main_Estimation_Results$topic_cluster_assignments
  nrows <- nrow(clust_assigns)
  cat("Topic Cluster Assignments: \n",clust_assigns[nrows,],"\n", "If all topics are assigned to one cluster, you may want to rerun with a new seed as this may indicate that the model jumped to a degenerate distribution over cluster assignments...\n", sep = "")
  
  if(save_results_to_file){
    cat("Saving Results... \n")
    save(Main_Estimation_Results, file=paste(data_dir,output_file,".Rdata",sep = ""))
  }
  return(Main_Estimation_Results)
}# end of ouuter function definition 


