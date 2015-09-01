

Initialize_Mixing_Parameters <- function(num_mixing_parameters,
                                         num_clusters,
                                         num_authors,
                                         author_attributes,
                                         mixing_attribute){
  
  if(num_mixing_parameters == 1){
    print("checking to ensure that the mixing parameter selected exists...")
    #determine which column of the author attribute table to use
    if(ncol(author_attributes) > 1){
      names <- colnames(author_attributes)
      index <- which(names == mixing_attribute)
      if(is.na(index)){
        stop("The mixing parameter you selected does not exist!")
      }
    }else{
      index <- 1
    }

    Number_of_Betas <- 4
    Betas <- matrix(runif(num_clusters*Number_of_Betas),nrow =num_clusters,ncol = Number_of_Betas)
    
    #generate indicator array
    Beta_Indicator_Array <- array(0,c(num_authors,num_authors,Number_of_Betas))
    
    #check to make sure there are only 2 possible values for attribute
    values <- unique(author_attributes[,index])
    if(length(values) != 2){
      cat("You are using a variable that is not binary as a mixing parameter. The variable is", mixing_attribute," and it takes on values:",values, "\n")
      stop()
    }
    
    for(j in 1:num_authors){
      for(k in 1:num_authors){
        if(author_attributes[j,index] == values[1] & author_attributes[k,index] == values[1]){
          Beta_Indicator_Array[j,k,1] = 1   
        }
        if(author_attributes[j,index] == values[1] & author_attributes[k,index] == values[2]){
          Beta_Indicator_Array[j,k,2] = 1   
        }
        if(author_attributes[j,index] == values[2] & author_attributes[k,index] == values[1]){
          Beta_Indicator_Array[j,k,3] = 1   
        }
        if(author_attributes[j,index] == values[2] & author_attributes[k,index] == values[2]){
          Beta_Indicator_Array[j,k,4] = 1   
        }
      }
    }    
  }else{
    #we assume we are not using any mixing parameters
    Number_of_Betas <- 4
    Betas <- matrix(0,nrow =num_clusters,ncol = Number_of_Betas)
    Number_of_Betas <- 0
    Beta_Indicator_Array <- array(0,c(num_authors,num_authors,4))
  }
  
  return(list(Number_of_Betas,Beta_Indicator_Array,Betas))
}
