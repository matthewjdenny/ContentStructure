# Defines function which runs main analysis

Run_Inference <- function(Number_Of_Iterations, 
                          Base_Alpha, 
                          Base_Beta, 
                          Number_Of_Topics, 
                          Author_Attributes, 
                          Document_Edge_Matrix,
                          Document_Word_Matrix, 
                          Vocabulary,
                          Latent_Dimensions, 
                          Topic_Step_Itterations, 
                          Sample_Step_Itterations, 
                          output_file,
                          Proposal_Variance, 
                          seed, 
                          output_folder_path, 
                          Number_of_Clusters,
                          Itterations_Before_Cluster_Assingment_Updates, 
                          Adaptive_Metropolis_Target_Accept_Rate, 
                          slice_sample_alpha_step_size, 
                          Slice_Sample_Alpha, 
                          MH_prior_standard_deviation, 
                          Number_of_Binary_Mixing_Parameters,
                          Mixing_Variable,
                          save_results_to_file){
    
    #== set working driectory and source all functions ===#
    set.seed(seed)

    #== Initialize all variables, latent spaces edge assingments and topic assignments ==#

    #if we are slice smpling alhpa then set the control variable to one, otherwise leave it at zero
    SS_Alpha <- 0
    if(Slice_Sample_Alpha){
        SS_Alpha <- 1
    }
    
    Latent_Space_Intercepts <- rep(0, Number_of_Clusters) 
    temp <- Proposal_Variance #create vector of proposal variances
    Proposal_Variance <- rep(temp, Number_of_Clusters)
    # The number of documents is equal to the number of rows 
    Number_Of_Documents <- length(Document_Word_Matrix[,1]) 
    # This is used to shrink the proposal variace of the metropolis hastings portion of the algorithm 
    Metropolis_Hastings_Control_Parameter <- 0 
    Number_Of_Authors <- length(Author_Attributes[,1]) 
    # The number of unique words in the corpus
    Number_Of_Words <- length(Vocabulary[,1]) 
    # LDA hyper-parameter
    Beta <- Base_Beta*Number_Of_Words 
    
    # We define alpha to be a vector so that it can accomodate an asymmetric base measure in the future
    Alpha_Base_Measure_Vector <- rep(Base_Alpha/Number_Of_Topics,Number_Of_Topics)
    # Make a vector of document authors
    Document_Authors <- Document_Edge_Matrix[,1] 
    # Remove the authors from the docuemnt edge matrix
    Document_Edge_Matrix <- Document_Edge_Matrix[,-1] 
    cat("Initializing Topic Assignments... \n")

    Token_Topic_Assignments <- vector(length = Number_Of_Documents, mode = "list")
    for(d in 1:Number_Of_Documents){
        cur_token_assignments <- sample(1:Number_Of_Topics,sum(Document_Word_Matrix[d,]),replace= T) #samples from a discrete uniform distribution
        Token_Topic_Assignments[[d]] <- cur_token_assignments
    }

    cat("Initiailizing Latent Space Positions... \n")
    Latent_Space_Positions <- array(rnorm( n = Latent_Dimensions*Number_of_Clusters*Number_Of_Authors, mean = 0,sd = 1),c(Latent_Dimensions,Number_of_Clusters,Number_Of_Authors))
    
    #initialize a datastructure to keep a number of topics by number of unique words matrix 
    cat("Initializing Word Type Topic Counts... \n")
    Word_Type_Topic_Counts <- matrix(0,nrow = Number_Of_Words, ncol = Number_Of_Topics)

    Token_Word_Types <- vector(length = Number_Of_Documents, mode = "list")
    for(d in 1:Number_Of_Documents){
        #create a vector of word types for each token
        word_indexes <- which(Document_Word_Matrix[d,] > 0)
        word_counts <- as.numeric(Document_Word_Matrix[d,word_indexes])
        already <- F
        for(i in 1:length(word_indexes)){
            if(!already){
                already <- T
                word_types <- rep(word_indexes[i],word_counts[i])
            }else{
                word_types <- c(word_types,rep(word_indexes[i],word_counts[i]))
            }
        }
        Token_Word_Types[[d]] <- word_types
        #now get the token topic assignemnts for this document
        current_doc_assignments <- Token_Topic_Assignments[[d]]
        #now go through and increment based in intial draws
        for(i in 1:length(current_doc_assignments)){
            Word_Type_Topic_Counts[word_types[i],current_doc_assignments[i]] <- Word_Type_Topic_Counts[word_types[i],current_doc_assignments[i]] + 1
        }
    }
    cat("Initializing Mixing Parameters... \n")
    MP <- Initialize_Mixing_Parameters(
            num_mixing_parameters = Number_of_Binary_Mixing_Parameters,
            num_clusters = Number_of_Clusters,
            num_authors = Number_Of_Authors,
            author_attributes = Author_Attributes,
            mixing_attribute = Mixing_Variable)
  
    Number_of_Betas <- MP[[1]]
    Beta_Indicator_Array <- MP[[2]]
    Betas <-  MP[[3]]

    Topic_Cluster_Assignments <- rep(0,Number_Of_Topics)
    for(k in 1:Number_Of_Topics){
        Topic_Cluster_Assignments[k] <- round(runif(1, min = 1, max = Number_of_Clusters),0)
    }
    
    cat("Running Model... \n")
    #==================== MAIN Function ====================#                             
    
    Return_List <- Main_Sampler(
        Number_Of_Iterations,
        Topic_Step_Itterations,
        Sample_Step_Itterations,
        as.integer(Number_Of_Authors), 
        Number_Of_Topics,
        Number_of_Clusters,
        Latent_Dimensions,
        Number_Of_Documents,
        Proposal_Variance,
        Topic_Cluster_Assignments,
        Latent_Space_Positions, 
        array(0,c(Latent_Dimensions,Number_Of_Topics,Number_Of_Authors)),
        Latent_Space_Intercepts,
        Betas,
        Number_of_Betas,
        Beta_Indicator_Array,
        as.matrix(Document_Edge_Matrix),
        Token_Topic_Assignments,
        Token_Word_Types,
        Document_Authors,
        Beta,
        Alpha_Base_Measure_Vector,
        Word_Type_Topic_Counts,
        apply(Word_Type_Topic_Counts,2,sum),
        as.integer(Number_Of_Words),
        Itterations_Before_Cluster_Assingment_Updates,
        Adaptive_Metropolis_Target_Accept_Rate,
        slice_sample_alpha_step_size,
        SS_Alpha,
        MH_prior_standard_deviation,
        seed
    )
    
    Main_Estimation_Results <- append(Return_List,MP)
    #get things ready to return a model object with all of the relevant info 
    if(save_results_to_file){
      save(Main_Estimation_Results, file = paste(output_folder_path,"Model_Output_",output_file,".Rdata",sep = ""))
    }
    return(Main_Estimation_Results)
} # End of Run_Analysis definition





