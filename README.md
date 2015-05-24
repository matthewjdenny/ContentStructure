# ContentStructure
An R package which implements an extension to the TPME model of Krafft et al. (2012). 

To install this package from git, you will need to Hadley Wickham's devtools package installed.

    install.packages("devtools")
    library("devtools")
    
Now we can install from Github using the following line:

    devtools::install_github("matthewjdenny/ContentStructure")

I have  had success installing with R 3.2.0+ installed but if you do not have the latest version of R installed, it should work as long as you install the dependencies first with the following block of code:

    install.packages( pkgs = c("BH","coda","RcppArmadillo","gridBase",
    "gplots","ggplot2","slam","snowfall","vegan"), dependencies = TRUE)

If all went well, check out the `?ContentStructure` help file to see a full working example with info on hos the data should look. Here is some example code that will run the model an generate output on a toy dataset of 121 emails between 20 department managers.

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
    Create_Output(data_name = "Test",
                  only_generate_summaries = T, 
                  data_directory = "~/Working/Directory/",
                  print_agg_stats = F,
                  using_county_email_data = F,
                  Topic_Model_Burnin = 50,
                  Skip = 0, 
                  Thin = 1,
                  Used_MP = T,
                  MP_Name = "Gender",
                  Auth_Attr = author_attributes,
                  Vocabulary = vocabulary
                  )
            
So far, this package has been tested successfully on OSX 10.9.5 and CentOS 6.6. Please email me at <mzd5530@psu.edu> if you have success on another OS.
