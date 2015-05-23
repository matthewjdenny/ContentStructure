Run_MH_To_Convergence <- function(input_file = "Test",
                                  data_source = "McDowell_2011_Data", 
                                  output_file = "Test", 
                                  itterations = 1200000,
                                  sample_every = 100, 
                                  data_dir = "~/Dropbox/PINLab/",
                                  sample_step_burnin = 200000,
                                  post_burin_variance_multiplier = 0.1, 
                                  prop_var = 1,
                                  set_proposal_variance = F,
                                  adaptive_metropolis_update_every = 100, 
                                  use_adaptive_metropolis = 1, 
                                  MH_prior_standard_deviation = 2, 
                                  seed = 1234){
    
    set.seed(seed)
#     library(RcppArmadillo)
#     library(BH)
    
    
    load(paste(data_dir,input_file,".Rdata", sep = ""))
    
    #set.seed(seed)
#     Rcpp::sourceCpp("./Scripts/CPME_Inference_MH.cpp")
    
    print("Loading Data")
    #extract current metropolis results
    first_return <- 13
    Topic_Model_Results <- Return_List[1:5]
    Model_Parameters <- Return_List[6:first_return]
    Cluster_Topic_Assignments <- Return_List[[14]]
    len <- length(Cluster_Topic_Assignments[,1])
    Cluster_Topic_Assignments<- Cluster_Topic_Assignments[len,]
    #Last_Cluster_Topic_Assignments <- Cluster_Topic_Assignments[Model_Parameters[[2]],]
    Metropolis_Results <- Return_List[15:20]
    Number_of_Betas <- Return_List[[21]]
    Beta_Indicator_Array <- Return_List[[22]]
    
    load(paste(data_dir,data_source,".Rdata", sep = ""))
    
    
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
    cat("Proposal Variances:",Proposal_Variances,"\n")
    LSPs <- Metropolis_Results[[3]][(2*Itterations-1):(2*Itterations),,]
    Intercepts <- Metropolis_Results[[1]][Itterations,]
    Betas <- Metropolis_Results[[2]][,,Itterations]
    
    #If we are manually settign the proposal variance
    if(set_proposal_variance== T){
        Proposal_Variances <- rep(prop_var,Clusters)
    }
    print(Cluster_Topic_Assignments)
    print(apply(Topic_Present_Edge_Counts,1,sum))
    #print(Topic_Present_Edge_Counts)
    #get the total number of tokens assigned to each topic
    Topic_Token_Totals <- apply(Word_Type_Topic_Counts,2,sum)
    #get the total number of present edges assigned to each topic
    Email_Assignments <- apply(Topic_Present_Edge_Counts,3,sum)
    clp <- array(0,c(Latent_Spaces,Clusters,Actors))
    
    print("Running Model")
    
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

    Return_List[[9]] <- Result_List[[1]]
    Return_List[[15]] <- Result_List[[5]]
    Return_List[[16]] <- Result_List[[7]]
    Return_List[[17]] <- Result_List[[6]]
    Return_List[[18]] <- Result_List[[4]]
    Return_List[[19]] <- Result_List[[2]]
    Return_List[[20]] <- Result_List[[3]]

    print("saving")
    save(Return_List, file=paste(data_dir,output_file,".Rdata",sep = ""))

}# end of ouuter function definition 


