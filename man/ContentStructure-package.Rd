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
To use this function, first call the Cluster_Run_Models function to actually run the ContentStructure model on any number of organizational email corpora. Then run the Generate_Diagnostics_All_Datasets function to actually generate output that you can look at. At this point, the Generate_Diagnostics_All_Datasets function does not support outputting a tidy aggregate level dataset unless you are using the North Carolina County Government dataset, however, this functionality will be added in the future. The package also only currently supports 0-1 binary covariates, although further support is on the way.
}
\author{
\packageAuthor{ContentStructure}
Maintainer: \packageMaintainer{ContentStructure}
}
\references{
First Publication:

Krafft, P., Moore, J., Desmarais, B. A., & Wallach, H. (2012). Topic-partitioned multinetwork embeddings. In Advances in Neural Information Processing Systems Twenty-Five. Retrieved from http://machinelearning.wustl.edu/mlpapers/paper_files/NIPS2012_1288.pdf

The appropriate publication to cite for this pacakge is still in preparation.
}