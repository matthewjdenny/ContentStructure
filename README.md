# ContentStructure

NOTE: **This package is still under development and is very much a work in progress. Please do not use in any publication without express consent of the authors. PLEASE REPORT ANY BUGS OR ERRORS TO <mzd5530@psu.edu>**. 

## Model Overview 

An R package which implements an extension to the TPME model of Krafft et al. (2012) [[Available here](http://dirichlet.net/pdf/krafft12topic-partitioned.pdf)].  The three main extensions we make to model are detailed below:

*  Instead of assigning each edge to an individual token and thus a single topic as in Krafft et al. (2012), we now assign each edge to the distribtuion over topics implied by all tokens in the current document.
*  We integrate covariates into the Latent Space Network Model (LSM) portion of the TPME to situate it as a full generalization of the LSM developed by Hoff Raftery and Handcock [[Available Here](http://www.stat.cmu.edu/~brian/905-2009/all-papers/hoff-raftery-handcock-2002-jasa.pdf)]
*  We implement Latent Class clustering of topics (and their associated latent spaces) so that communication that follows similar patterns of interaction and content is automatically clustered together. Selecting a smaller number of clusters also addresses potential problems with a lack of sufficient data associated with each latent space producing estimates of latent positions that are too dispersed to be interpretable.

A paper detailing the extended and generalized model is currently under revision. A link to the manuscript will be posted as soon as it is available.

## Installation

### Requirements for using C++ code with R

Note that if you are using a Mac, you will need to start by making sure you have Xcode + developer tools installed or you will not be able to compile the C++ code that is used in the samplers for this package. You will need to go here: <https://developer.apple.com/xcode/downloads/> and then select the link to the additional downloads page which will prompt you to enter you apple ID. This will let you download the developer tools. This requirement is not unique to this package, but is necessary for all packages that use Rcpp.  
  
If you are using a Windows machine, you will need to make sure you have the latest release of R (3.2.0+) and will also need to install the `Rtools` library before you can use any packages with C++ code in them. It is also highly advised that you use [RStudio](http://www.rstudio.com/) to download and install the package as it seems to play nicer with Rcpp under Windows. You may also want to visit [this blog post](https://cdrv.wordpress.com/2013/01/12/getting-compilers-to-work-with-rcpp-rcpparmadillo/) which has more information on making C++ work with R under Windows. 
  
If you are using a Linux distro, make sure you have a C++ complier installed, but in general, you should not run into as many issues

### Installing The Package
  
To install this package from Github, you will need to Hadley Wickham's devtools package installed.

    install.packages("devtools")
    library("devtools")
    
Now we can install from Github using the following line:

    devtools::install_github("matthewjdenny/ContentStructure")

I have  had success installing with R 3.2.0+ installed but if you do not have the latest version of R installed, or run into some install errors (please email if you do), it should work as long as you install the dependencies first with the following block of code:

    install.packages( pkgs = c("BH","coda","RcppArmadillo","gridBase",
    "gplots","slam","vegan"), dependencies = TRUE)

If all went well, check out the `?ContentStructure` help file to see a full working example with info on how the data should look. 

## Basic Useage

The package provides two functions: `Run_Full_Model()` and `Create_Output()` which will actually run the model for text valued communication networks and then generate descriptive and diagnostic output. To run this model, you will need to provide input similar to the example `vocabulary`, `author_attributes`, `document_edge_matrix`, and `document_word_matrix`  dataframes included as examples with the package. You will need to format the data similarly or estimation will not work, and we recommend giving the same names to your files. This version of the package currently supports the inclusion of up to one binary author covariate in the model, whch can be specified by giving the variable name as it appears in the `author_attributes` object. Also of note, when specifying the `data_directory` parameter in these fuctions, be sure to include a `/` after the name of the folder you wish to use for output/data as the function expects this. The sampling code for the Latent Space Model (LSM) portion of the model is pretty fast so we recommend specifying a relatively large number of iterations for the final step of the model where the LSM is run to convergence. We have found that  `sample_step_burnin = 1000000`, `sample_step_iterations = 5000000` and `sample_step_sample_every = 2000` seem to work pretty well for publication quality results with networks of 20-30 people. 

## Example

Here is some example code that will run the model an generate output on a toy dataset of 121 emails between 20 department managers. 

    ## set working directory, dataset name, and load the library 
    mywd <- "~/Dropbox/Testing"
    mydataset <- "Test"
    setwd(mywd)
    library(ContentStructure)  
    
    #load in necessary data
    data(vocabulary)
    data(author_attributes)
    data(document_edge_matrix)
    data(document_word_matrix)  
      
    # run model
    Run_Full_Model(data_name = mydataset,  
                   main_iterations = 100, 
                   sample_step_burnin = 2000, 
                   sample_step_iterations = 8000,
                   sample_step_sample_every = 20,
                   topics = 6,
                   clusters = 2,
                   data_directory = mywd,
                   run_MH_only = F,
                   mixing_variable = "Gender",
                   Auth_Attr = author_attributes, 
                   Doc_Edge_Matrix = document_edge_matrix ,
                   Doc_Word_Matrix = document_word_matrix, 
                   Vocab = vocabulary,
    			   Seed = 123456
                   )  
                     
    Data <- Create_Output(data_name = mydataset,
              		  only_generate_summaries = T, 
                      data_directory = mywd,
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
                          
## Output

The `Create_Output()` function will save a number of PDF's in the `data_directory`, which will often be the most interesting a visually interpretable output, but it alo returns a list object that has the following members: 

* `Cluster_Data` contains cluster level data including top words and mixing parameters (with standard errors) if applicable.
* `Actor_Data` contains actors level data (all of the `Auth_Attr` dataframe) plus average latent positions for each actor in each dimension (2 currently), for each cluster. 
* `Token_Data` contains the counts of each token for each topic, along with the edge counts for that topic and the cluster assignment for it. 
* `Vocabulary` simply holds the vocabulary as a vector for easy handling. We have found these aggregate level statistics to be helpful in further analysis.

The model also saves a `MCMC_Output_data_name.Rdata` file which contains a `Return_List` R list object holding the raw output from the MCMC chain with the following members:

* `$token_topic_assignments`
* `$topic_present_edge_counts`
* `$topic_absent_edge_counts`
* `$token_type_topic_counts`
* `$number_of_documents`
* `$number_of_iterations`
* `$number_of_LSM_MH_iterations` 
* `$number_of_clusters`
* `$cluster_proposal_variances`
* `$cluster_accept_rates`
* `$LDA_log_likelihood_trace`
* `$topic_cluster_assignments`
* `$LSM_cluster_intercepts`
* `$LSM_cluster_mixing_parameters`
* `$LSM_actor_latent_positions`
* `$cluster_whether_accepted`
* `$cluster_proposed_likelihoods`
* `$cluster_current_likelihoods`
* `$number_possible_mixing_parameter_values`
* `$mixing_parameter_type_indicator_array`
* `$intial_mixing_parameter_values`

## Testing
            
So far, this package has been tested successfully on OSX 10.9.5 and CentOS 6.6. Please email me at <mzd5530@psu.edu> if you have success on another OS or run into any problems.
