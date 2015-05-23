# Defines function which runs main analysis

Run_Inference <- function(Number_Of_Iterations, 
                          Base_Alpha, 
                          Base_Beta, 
                          Number_Of_Topics, 
                          Author_Attributes= author_attributes, 
                          Document_Edge_Matrix = document_edge_matrix ,
                          Document_Word_Matrix = document_word_matrix, 
                          Vocabulary = vocabulary,
                          Latent_Dimensions = 2, 
                          Topic_Step_Itterations, 
                          Sample_Step_Itterations = 10, 
                          output_file,
                          Proposal_Variance = 0.5, 
                          seed = 1234, 
                          output_folder_path = "~/Dropbox/PINLab/", 
                          Number_of_Clusters,
                          Itterations_Before_Cluster_Assingment_Updates = 5, 
                          Adaptive_Metropolis_Target_Accept_Rate = 0.3, 
                          slice_sample_alpha_step_size = 1, 
                          Slice_Sample_Alpha = F, 
                          MH_prior_standard_deviation = 2, 
                          Number_of_Binary_Mixing_Parameters = 1,
                          Mixing_Variable = "Gender"){
    
    #================ set working driectory and source all functions ====================#
#     require(Rcpp)
#     require(RcppArmadillo)
    set.seed(seed)
    
    
    #================= Initialize all variables, latent spaces edge assingments and topic assignments ==============#
    
    
    #if we are slice smpling alhpa then set the control variable to one, otherwise leave it at zero
    SS_Alpha <- 0
    if(Slice_Sample_Alpha){
        SS_Alpha <- 1
    }
    
    Latent_Space_Intercepts <- rep(0, Number_of_Clusters) #this is set to 10 becasue it can only get smaller
    
    temp <- Proposal_Variance #create vector of proposal variances
    Proposal_Variance <- rep(temp, Number_of_Clusters)
    
    Number_Of_Documents <- length(Document_Word_Matrix[,1]) # the number of documents is equal to the number of rows 
    
    Metropolis_Hastings_Control_Parameter <- 0 #this is used to shrink the proposal variace of the metropolis hastings portion of the algorithm 
    
    Number_Of_Authors <- length(Author_Attributes[,1]) 
    
    Number_Of_Words <- length(Vocabulary[,1]) #the number of unique words in the corpus
    
    Beta <- Base_Beta*Number_Of_Words 
    
    #we define alpha to be a vector so that it can accomodate an asymmetric base measure in the future
    Alpha_Base_Measure_Vector <- rep(Base_Alpha/Number_Of_Topics,Number_Of_Topics)
    
    Document_Authors <- Document_Edge_Matrix[,1] #make a vector of document authors
    
    Document_Edge_Matrix <- Document_Edge_Matrix[,-1] # remove the authors from the docuemnt edge matrix
    print("Initializing Topic Assignments...")

    Token_Topic_Assignments <- list()
    for(d in 1:Number_Of_Documents){
        cur_token_assignments <- sample(1:Number_Of_Topics,sum(Document_Word_Matrix[d,]),replace= T) #samples from a discrete uniform distribution
        Token_Topic_Assignments <- append(Token_Topic_Assignments,list(cur_token_assignments))
    }
    
    print("Initiailizing Latent Space Positions...")
    Latent_Space_Positions <- array(rnorm( n = Latent_Dimensions*Number_of_Clusters*Number_Of_Authors, mean = 0,sd = 1),c(Latent_Dimensions,Number_of_Clusters,Number_Of_Authors))
    
    #initialize a datastructure to keep a number of topics by number of unique words matrix 
    print("Initializing Word Type Topic Counts...")
    Word_Type_Topic_Counts <- matrix(0,nrow = Number_Of_Words, ncol = Number_Of_Topics)

    Token_Word_Types <- list()
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
        Token_Word_Types <- append(Token_Word_Types,list(word_types))
        #now get the token topic assignemnts for this document
        current_doc_assignments <- Token_Topic_Assignments[[d]]
        #now go through and increment based in intial draws
        for(i in 1:length(current_doc_assignments)){
            Word_Type_Topic_Counts[word_types[i],current_doc_assignments[i]] <- Word_Type_Topic_Counts[word_types[i],current_doc_assignments[i]] + 1
        }
    }
    print("Initializing Betas...")
    #initialize betas
    MP <- Initialize_Mixing_Parameters(num_mixing_parameters = Number_of_Binary_Mixing_Parameters,
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
    
    print("Running Model...")
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
    
    Return_List <- append(Return_List,MP)
    #get things ready to return a model object with all of the relevant info 
    save(Return_List, file = paste(output_folder_path,"Model_Output_",output_file,".Rdata",sep = ""))
    
    return(Return_List)
} # End of Run_Analysis definition





