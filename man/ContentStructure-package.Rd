\name{ContentStructure-package}
\alias{ContentStructure-package}
\alias{ContentStructure}
\docType{package}
\title{
\packageTitle{ContentStructure}
}
\description{
\packageDescription{ContentStructure}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ContentStructure}
\packageIndices{ContentStructure}
To use this function, first call the Run_Full_Model function to actually run the ContentStructure model on an organizational email corpora. Then run the Create_Output function to actually generate output that you can look at. At this point, the Create_Output function does not support outputting a tidy aggregate level dataset unless you are using the North Carolina County Government dataset, however, this functionality will be added in the future. The package also only currently supports 0-1 binary covariates, although further support is on the way.
}
\author{
\packageAuthor{ContentStructure}
Maintainer: \packageMaintainer{ContentStructure}
}
\references{
First Publication:

Krafft, P., Moore, J., Desmarais, B. A., & Wallach, H. (2012). Topic-partitioned multinetwork embeddings. In Advances in Neural Information Processing Systems Twenty-Five. Retrieved from http://machinelearning.wustl.edu/mlpapers/paper_files/NIPS2012_1288.pdf

The appropriate publication to cite for this package is still in preparation.
}
\examples{
# Load in necessary data
data(vocabulary)
data(author_attributes)
data(document_edge_matrix)
data(document_word_matrix)

# Run Model
Estimation_Results <- Run_Full_Model(
  main_iterations = 100, 
  sample_step_burnin = 2000, 
  sample_step_iterations = 8000,
  sample_step_sample_every = 20,
  topics = 6,
  clusters = 2,
  mixing_variable = "Gender",
  Auth_Attr = author_attributes, 
  Doc_Edge_Matrix = document_edge_matrix ,
  Doc_Word_Matrix = document_word_matrix, 
  Vocab = vocabulary,
  Seed = 123456
  )

  # Generate Output
Data <- Create_Output(
  output_names = "Testing",
  Estimation_Results = Estimation_Results,
  print_agg_stats = TRUE,
  Topic_Model_Burnin = 50,
  Skip = 0, 
  Thin = 2
  )
}