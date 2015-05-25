# ContentStructure

## Model Overview 

An R package which implements an extension to the TPME model of Krafft et al. (2012) [[Available here](http://dirichlet.net/pdf/krafft12topic-partitioned.pdf)].  The three main extensions we make to model are detailed below:

*  Instead of assigning each edge to an individual token and thus a single topic as in Krafft et al. (2012), we now assign each edge to the distribtuion over topics implied by all tokens in the current document.
*  We integrate covariates into the Latent Space Network Model (LSM) portion of the TPME to situate it as a full generalization of the LSM developed by Hoff Raftery and Handcock [[Available Here](http://www.stat.cmu.edu/~brian/905-2009/all-papers/hoff-raftery-handcock-2002-jasa.pdf)]
*  We implement Latent Class clustering of topics (and their associated latent spaces) so that communication that follows similar patterns of interaction and content is automatically clustered together. Selecting a smaller number of clusters also addresses potential problems with a lack of sufficient data associated with each latent space producing estimates of latent positions that are too dispersed to be interpretable.

A paper detailing the extended and generalized model is currently under revision. A link to the manuscript will be posted as soon as it is available.

## Installation

To install this package from git, you will need to Hadley Wickham's devtools package installed.

    install.packages("devtools")
    library("devtools")
    
Now we can install from Github using the following line:

    devtools::install_github("matthewjdenny/ContentStructure")

I have  had success installing with R 3.2.0+ installed but if you do not have the latest version of R installed, it should work as long as you install the dependencies first with the following block of code:

    install.packages( pkgs = c("BH","coda","RcppArmadillo","gridBase",
    "gplots","ggplot2","slam","snowfall","vegan"), dependencies = TRUE)

If all went well, check out the `?ContentStructure` help file to see a full working example with info on how the data should look. 

## Basic Useage

The package provides two functions: `Run_Full_Model()` and `Create_Output()` which will actually run the model for text valued communication networks and then generate descriptive and diagnostic output. To run this model, you will need to provide input similar to the example `vocabulary`, `author_attributes`, `document_edge_matrix`, and `document_word_matrix`  dataframes included as examples with the package. You will need to format the data similarly or estimation will not work, and we recommend giving the same names to your files. This version of the package currently supports the inclusion of up to one binary author covariate in the model, whch can be specified by giving the variable name as it appears in the `author_attributes` object. Also of note, when specifying the `data_directory` parameter in these fuctions, be sure to include a `/` after the name of the folder you wish to use for output/data as the function expects this. The sampling code for the Latent Space Model (LSM) portion of the model is pretty fast so we recommend specifying a relatively large number of iterations for the final step of the model where the LSM is run to convergence. We have found that  `sample_step_burnin = 1000000`, `sample_step_iterations = 5000000` and `sample_step_sample_every = 2000` seem to work pretty well for publication quality results with networks of 20-30 people. 

## Example

Here is some example code that will run the model an generate output on a toy dataset of 121 emails between 20 department managers. The `Create_Output()` function returns a list object that has the following members: `Cluster_Data` contains cluster level data including top words and mixing parameters (with standard errors) if applicable. `Actor_Data` contains actors level data (all of the `Auth_Attr` dataframe) plus average latent positions for each actor in each dimension (2 currently), for each cluster. `Token_Data` contains the counts of each token for each topic, along with the edge counts for that topic and the cluster assignment for it. `Vocabulary` simply holds the vocabulary as a vector for easy handling. We have found these aggregate level statistics to be helpful in further analysis.

    # set working directory and load the library 
    setwd("~/Working/Directory/")
    library(ContentStructure)
    # load in necessary data (you will need all 4 objects)
    data(vocabulary)
    data(author_attributes)
    data(document_edge_matrix)
    data(document_word_matrix)
    # run model
    Run_Full_Model(data_name = "Test",  
                   main_iterations = 100, 
                   sample_step_burnin = 2000, 
                   sample_step_iterations = 8000,
                   sample_step_sample_every = 20,
                   topics = 6,
                   clusters = 2,
                   data_directory = "~/Working/Directory/",
                   run_MH_only = F,
                   mixing_variable = "Gender",
                   Auth_Attr = author_attributes, 
                   Doc_Edge_Matrix = document_edge_matrix ,
                   Doc_Word_Matrix = document_word_matrix, 
                   Vocab = vocabulary
                   )
    Data <- Create_Output(data_name = "Test",
                          only_generate_summaries = T, 
                          data_directory = "~/Working/Directory/",
                          print_agg_stats = T,
                          using_county_email_data = F,
                          Topic_Model_Burnin = 50,
                          Skip = 0, 
                          Thin = 1,
                          Used_MP = T,
                          MP_Name = "Gender",
                          Auth_Attr = author_attributes,
                          Vocabulary = vocabulary
                          )
            
So far, this package has been tested successfully on OSX 10.9.5 and CentOS 6.6. Please email me at <mzd5530@psu.edu> if you have success on another OS or run into any problems.
