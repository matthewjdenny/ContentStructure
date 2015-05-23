#' A Function to run the ContentStructure model to convergence for multiple datasets in parallel.
#' 
#' @param name_vector A vector containing the exact names of the .Rdata files containing the relevant files necessary to run the model for each organization.
#' @param Data_Directory The directory where all .Rdata files are stored -- also where all output will be stored.
#' @param MAX_CPUs The maximum number of datasets that can be analyzed in parallel -- should be less than or equal to the number of cores on your machine.
#' @param Main_Iterations The number of iterations of Gibbs sampling for the LDA part of the model. We have found 4,000 seems to work well.
#' @param Sample_Step_Burnin The number of iterations of burnin that should be completed before sampling the latent space parameters when running MH for the LSM to convergence.
#' @param Sample_Step_Iterations The total number of iterations to run MH for the LSM for (before thinning).
#' @param Sample_Step_Sample_Every How many iterations to skip when thinning the MH for the LSM chain in our MH for the LSM sample step.
#' @param Topics The number of topics to use
#' @param Clusters The number of topic clusters to use
#' @return Does not return anything, just saves everything to our data_directory folder.
#' @export
Cluster_Run_Models <- function(name_vector, Data_Directory,MAX_CPUs = 4,Main_Iterations = 4000, Sample_Step_Burnin = 2000000, Sample_Step_Iterations = 8000000,Topics = 40, Clusters = 4,Sample_Step_Sample_Every = 2000){

  setwd(Data_Directory)
  
  num_counties <- length(name_vector)
  if(num_counties < MAX_CPUs){
    numcpus <- num_counties
  }else{
    numcpus <- MAX_CPUs
  }
  
  sfInit(parallel=TRUE, cpus=numcpus ) 
  if(sfParallel()){
    cat( "Running in parallel mode on", sfCpus(), "nodes.\n" )
  }else{
    cat( "Running in sequential mode.\n" )
  }
  
  #export all packages currently loaded in your R session
  for (i in 1:length(.packages())){
    eval(call("sfLibrary", (.packages()[i]), character.only=TRUE))
  }
  
  #export a list of R data objects that your function will need
  sfExport("name_vector")
  
  Wrapper <- function(index){
    setwd(Data_Directory)
    #         source("./Scripts/CPME_Run_Full_Model.R")
    Run_Full_Model(data_name = name_vector[index],  
                   main_iterations = Main_Iterations, 
                   sample_step_burnin = Sample_Step_Burnin, 
                   sample_step_iterations = Sample_Step_Iterations,
                   topics = Topics,
                   clusters = Clusters,
                   data_directory = Data_Directory,
                   sample_step_sample_every = Sample_Step_Sample_Every 
                   )
    return(1)
  }
  
  indexes <- 1:num_counties
  result <- sfClusterApplyLB(indexes,Wrapper)
  sfStop()
}