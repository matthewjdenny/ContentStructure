Run_MH_To_Convergence <- function(input_file,
                                  output_file, 
                                  itterations,
                                  sample_every, 
                                  data_dir,
                                  sample_step_burnin,
                                  post_burin_variance_multiplier, 
                                  prop_var,
                                  set_proposal_variance,
                                  adaptive_metropolis_update_every, 
                                  use_adaptive_metropolis, 
                                  MH_prior_standard_deviation, 
                                  seed,
                                  save_results_to_file,
                                  Main_Estimation_Results){
    
    set.seed(seed)
    if(save_results_to_file){
      load(paste(data_dir,input_file,".Rdata", sep = ""))
    }
    #extract current metropolis results
    first_return <- 13
    Topic_Model_Results <- Main_Estimation_Results[1:5]
    Model_Parameters <- Main_Estimation_Results[6:first_return]
    Cluster_Topic_Assignments <- Main_Estimation_Results[[14]]
    len <- length(Cluster_Topic_Assignments[,1])
    Cluster_Topic_Assignments<- Cluster_Topic_Assignments[len,]
    Metropolis_Results <- Main_Estimation_Results[15:20]
    Number_of_Betas <- Main_Estimation_Results[[21]]
    Beta_Indicator_Array <- Main_Estimation_Results[[22]]
    
    #remove the first skip_first itterations of each sublist and recombine
    Itterations <- Model_Parameters[[4]]
    
    #get model information and extract data
    Latent_Spaces <- length(Metropolis_Results[[3]][,1,1])/length(Metropolis_Results[[1]][,1])
    Clusters <- Model_Parameters[[5]]
    Topics <- length(Cluster_Topic_Assignments)
    Actors <- length(Metropolis_Results[[3]][1,1,])
    Token_Topic_Assignments <- Topic_Model_Results[[1]]
    Topic_Present_Edge_Counts <- Topic_Model_Results[[2]]
    Topic_Absent_Edge_Counts <- Topic_Model_Results[[3]]
    Word_Type_Topic_Counts <- Topic_Model_Results[[4]]
    Proposal_Variances <- Model_Parameters[[6]][Model_Parameters[[2]],]
    LSPs <- Metropolis_Results[[3]][(2*Itterations-1):(2*Itterations),,]
    Intercepts <- Metropolis_Results[[1]][Itterations,]
    Betas <- Metropolis_Results[[2]][,,Itterations]
    
    #If we are manually settign the proposal variance
    if(set_proposal_variance== T){
        Proposal_Variances <- rep(prop_var,Clusters)
    }
    #get the total number of tokens assigned to each topic
    Topic_Token_Totals <- apply(Word_Type_Topic_Counts,2,sum)
    #get the total number of present edges assigned to each topic
    Email_Assignments <- apply(Topic_Present_Edge_Counts,3,sum)
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


