
Generate_Model_Diagnsotics <- function(skip_first ,
                                       topic_model_burnin , 
                                       pretty_name , 
                                       only_print_summaries, 
                                       print_agregate_level_stats, 
                                       used_binary_mixing_attribute, 
                                       binary_mixing_attribute_name , 
                                       used_county_email_data,
                                       out_directory ,
                                       Thin_Itterations ,
                                       LS_Actor , 
                                       vocab,
                                       output_name,
                                       Author_Attributes,
                                       save_results,
                                       proportion_in_confidence_contour  = 0.9,
                                       load_results_from_file = F,
                                       input_folder_path = NULL,
                                       input_file = NULL,
                                       Estimation_Results = NULL
                                        ){

        UMASS_BLUE <- rgb(51,51,153,255,maxColorValue = 255)
        UMASS_RED <- rgb(153,0,51,255,maxColorValue = 255)
        UMASS_GREEN <- rgb(0,102,102,255,maxColorValue = 255)
        UMASS_YELLOW <- rgb(255,255,102,255,maxColorValue = 255)
        UMASS_ORANGE <- rgb(255,204,51,255,maxColorValue = 255)
        Main_Estimation_Results <- NULL
        if(load_results_from_file){
          print("Loading Data...")
          print(paste(input_folder_path,input_file,".Rdata", sep = ""))
          load(paste(input_folder_path,input_file,".Rdata", sep = ""))
          Estimation_Results <- Main_Estimation_Results
        }
        print("Extracting Reduced Data")
        first_return <- 13
        Topic_Model_Results <- Estimation_Results[1:5]
        Model_Parameters <- Estimation_Results[6:first_return]
        Cluster_Topic_Assignments <- Estimation_Results[[14]]
        Last_Cluster_Topic_Assignments <- Cluster_Topic_Assignments[Model_Parameters[[2]],]
        Metropolis_Results <- Estimation_Results[15:20]
        
        Latent_Spaces <- length(Metropolis_Results[[3]][,1,1])/length(Metropolis_Results[[1]][,1])
        skip_first= skip_first+1
        #remove the first skip_first itterations of each sublist and recombine
        Itterations <- Model_Parameters[[4]]
        cat("Raw Number of Iterations:",Itterations,"\n")
        
        #' check to make sure that the user has specified a valid number of 
        #' iterations to skip. 
        if(skip_first >= Itterations){
          stop("You must specify Skip < the number of samples saved from the last iteration of Metropolis Hasings.")
        }
        Metropolis_Results[[1]] <- Metropolis_Results[[1]][skip_first:Itterations,]
        Metropolis_Results[[2]] <- Metropolis_Results[[2]][,,skip_first:Itterations]
        Metropolis_Results[[3]] <- Metropolis_Results[[3]][(2*skip_first-1):(2*Itterations),,]
        Metropolis_Results[[4]] <- Metropolis_Results[[4]][skip_first:Itterations,]
        Metropolis_Results[[5]] <- Metropolis_Results[[5]][skip_first:Itterations,]
        Metropolis_Results[[6]] <- Metropolis_Results[[6]][skip_first:Itterations,]
        
        
        Itterations <- Model_Parameters[[4]] - skip_first +1
        # generate vector to thin latent space positions by
        base <- seq(1, 2*Itterations,2*Thin_Itterations)
        base2 <- seq(2, 2*Itterations,2*Thin_Itterations)

        ordered <- rep(0,2*length(base))
        counter <- 1
        for(k in 1:length(base)){
            ordered[counter] <- base[k]
            counter <- counter +1
            ordered[counter] <- base2[k]
            counter <- counter +1
        }
     
        #thin out the data by taking every Thin_Itterations itteration for the metropolis step
        Metropolis_Results[[1]] <- Metropolis_Results[[1]][seq(1, Itterations,Thin_Itterations),]
        Metropolis_Results[[2]] <- Metropolis_Results[[2]][,,seq(1, Itterations,Thin_Itterations)]
        Metropolis_Results[[3]] <- Metropolis_Results[[3]][ordered,,]
        Metropolis_Results[[4]] <- Metropolis_Results[[4]][seq(1, Itterations,Thin_Itterations),]
        Metropolis_Results[[5]] <- Metropolis_Results[[5]][seq(1, Itterations,Thin_Itterations),]
        Metropolis_Results[[6]] <- Metropolis_Results[[6]][seq(1, Itterations,Thin_Itterations),]

        Itterations <- Itterations/Thin_Itterations
        #get model information and extract data
        selected_depth <- Itterations - floor(proportion_in_confidence_contour*Itterations)

        Clusters <- Model_Parameters[[5]]
        Topics <- length(Last_Cluster_Topic_Assignments)
        Actors <- length(Metropolis_Results[[3]][1,1,])
        Token_Topic_Assignments <- Topic_Model_Results[[1]]
        Topic_Present_Edge_Counts <- Topic_Model_Results[[2]]
        Topic_Absent_Edge_Counts <- Topic_Model_Results[[3]]
        Word_Type_Topic_Counts <- Topic_Model_Results[[4]]
        Edge_Topic_Assignments <- Topic_Model_Results[[5]]
        Proposal_Variances <- Model_Parameters[[6]][Model_Parameters[[2]],]
        Cluster_Topic_Assigns <- Last_Cluster_Topic_Assignments
        
        Clusters_to_Pretty_Print <- unique(Cluster_Topic_Assigns)
        cat("Clusters to Print:", Clusters_to_Pretty_Print, "\n")
        # Get the total number of tokens assigned to each topic
        Topic_Token_Totals <- apply(Word_Type_Topic_Counts,2,sum)
        # Get the total number of present edges assigned to each topic
        Email_Assignments <- apply(Topic_Present_Edge_Counts,3,sum)
        num_words <- length(vocab[,1])
        
        # This list will be used to store lists of three vectors: word indicies in descending order of likelihood, word probability in topic, actual word. 
        Topic_Top_Words <- list()
        Top_Ten_Words <- rep("",Topics)
        Top_Four_Words <- rep("",Topics)
        Topic_Top_Ten_Words <- matrix("",ncol = 10,nrow = Topics)
        for(i in 1:Topics){
            indicies <- order(Word_Type_Topic_Counts[,i],decreasing = TRUE)
            probabilities <- Word_Type_Topic_Counts[indicies,i]/Topic_Token_Totals[i]
            #reduce to only words with non-zero probability
            indicies <- indicies[which(probabilities > 0)]
            probabilities <- probabilities[which(probabilities > 0)]
            top_words <- vocab[indicies,]
            Topic_List <- list(top_words,probabilities,indicies)
            Topic_Top_Words <- append(Topic_Top_Words,list(Topic_List))
            #add top words
            words <- ""
            words4 <- ""
            for(j in 1:5){
                if(!is.na(top_words[j])){
                    words <- paste(words,top_words[j],", ",sep = "")
                }
            }
            words <- paste(words," \n",sep = "")
            for(j in 6:10){
                if(!is.na(top_words[j])){
                    words <- paste(words,top_words[j],", ",sep = "")
                }
            }
            for(j in 1:4){
                words4 <- paste(words4,top_words[j],", ",sep = "")
            }
            for(j in 1:10){
                Topic_Top_Ten_Words[i,j] <- top_words[j]
            }
            Top_Ten_Words[i] <- words
            Top_Four_Words[i] <- words4
        }
        data <- list(Top_Four_Words,Cluster_Topic_Assigns)
        
        if(print_agregate_level_stats){
            save(data,file=paste(out_directory ,output_name,"Topic_Top_Words.Rdata", sep = ""))
        }
        
        #print out the top words for each topic in each cluster
        for(i in 1:Clusters){
            print(i)
            inds <- which(data[[2]] == i)
            print(data[[1]][inds])
        }

        #this list will be used to store lists of three vectors: word indicies in descending order of likelihood, word probability in topic, actual word. 
        Cluster_Top_Words <- list()
        Cluster_Ten_Words <- rep("",Clusters)
        Cluster_Twenty_Words <- rep("",Clusters)
        Cluster_Four_Words <- rep("",Clusters)
        
        Cluster_Top_Twenty_Words <- matrix("",ncol = 20,nrow = Clusters)
        for(k in 1:Clusters){
            
            probabilities <- rep(0,length(Word_Type_Topic_Counts[,1]))
            for(i in 1:Topics){
                if(Cluster_Topic_Assigns[i] == k){
                    probabilities <- probabilities + Word_Type_Topic_Counts[,i]
                }
            }
            #reduce to only words with non-zero probability
            indicies <- order(probabilities,decreasing = TRUE)
            indicies <- indicies[which(probabilities > 0)]
            probabilities <- probabilities[which(probabilities > 0)]
            top_words <- vocab[indicies,]
            #print(top_words)
            Cluster_List <- list(top_words,probabilities,indicies)
            Cluster_Top_Words <- append(Cluster_Top_Words,list(Cluster_List))
            #add top words
            words <- ""
            words4 <- ""
            words20 <- ""
            for(j in 1:5){
                if(!is.na(top_words[j])){
                    words <- paste(words,top_words[j],", ",sep = "") 
                }
            }
            words <- paste(words," \n",sep = "")
            for(j in 6:10){
                if(!is.na(top_words[j])){
                    words <- paste(words,top_words[j],", ",sep = "") 
                }
            }
            for(j in 1:4){
                words4 <- paste(words4,top_words[j],", ",sep = "")
            }
            for(j in 1:20){
                words20 <- paste(words20,top_words[j],", ",sep = "")
                Cluster_Top_Twenty_Words[k,j] <- top_words[j]
            }

            Cluster_Ten_Words[k] <- words
            Cluster_Four_Words[k] <- words4
            Cluster_Twenty_Words[k] <- words20
        }
        
        #display current accept rate
        Accept_Rates <- Metropolis_Results[[4]]
        
        for(i in 1:Clusters){
            print(paste("Current accept rate for cluster", i, "is:", sum(Accept_Rates[,i])/Itterations))
        }

        print("Generating Topic Model Log Likelihoods")
        if(print_agregate_level_stats){
          log_likelihoods  <- Model_Parameters[[8]]
          len <- length(log_likelihoods)
          geweke_log_likelihoods <- log_likelihoods[topic_model_burnin:len]
            
          if(save_results){
            pdf(paste(out_directory,output_name,"_Topic_Model_Log_Likelihood.pdf", sep = ""),height=3,width=5,pointsize=7)
            par(mfrow = c(1,1), mar = c(5,5,4,1))
            plot( y =geweke_log_likelihoods,x= topic_model_burnin:len, pch = 19, col = UMASS_BLUE, main = paste("Un-Normalized Topic Model Log Likelihood \n"," Geweke Statistic for Last",len-topic_model_burnin,"Iterations:", round(coda::geweke.diag(geweke_log_likelihoods)$z,2)  ), xlab = "Iteration",ylab = "Log Likelihood",cex.lab=2, cex.axis=1.4, cex.main=1.4)
            dev.off()
          }else{
            par(mfrow = c(1,1), mar = c(5,5,4,1))
            plot( y =geweke_log_likelihoods,x= topic_model_burnin:len, pch = 19, col = UMASS_BLUE, main = paste("Un-Normalized Topic Model Log Likelihood \n"," Geweke Statistic for Last",len-topic_model_burnin,"Iterations:", round(coda::geweke.diag(geweke_log_likelihoods)$z,2)  ), xlab = "Iteration",ylab = "Log Likelihood",cex.lab=2, cex.axis=1.4, cex.main=1.4)
            Sys.sleep(3)
          }
           
        }

        #calculate teh proportion of edges assigned to each cluster
        cluster_proportions <- rep(0,Clusters)
        for(j in 1:Clusters){
            cluster_indexes <- which(Last_Cluster_Topic_Assignments == j)
            #create matrix to add to
            
            slice <- matrix(0, Actors,Actors)
            if(length(cluster_indexes) > 0){
                for(i in cluster_indexes){
                    slice <- slice + Topic_Present_Edge_Counts[,,i]
                }
            }
            cluster_proportions[j] <- sum(slice)/sum(Topic_Present_Edge_Counts)
        }
        
        if(print_agregate_level_stats){
            print("Generating Cluster Edge Proportions")
            pdf(paste(out_directory,output_name,"_Cluster_Edges.pdf", sep = ""),height=4,width=4,pointsize=7) 
            par(mfrow = c(1,1), mar = c(5,5,4,1))
            plot(x = 1:Clusters, y =cluster_proportions,main = pretty_name,pch = 19, col = UMASS_BLUE, axes = T, xlab = "Cluster", ylab = "Proportion of Edges",cex = 2,cex.lab=2, cex.axis= 1.8, cex.main=2.2, xaxt = "n",ylim =c(0,max(cluster_proportions)) )
            axis(1, at = seq(1,Clusters, by = 1), las=1, cex.axis= 1.8)
            box(which = "plot")
            for(j in 1:Clusters){
                lines(c(j,j) , c(0,cluster_proportions[j]), col = UMASS_BLUE, lwd = 1.5)
            }
            dev.off()
        }

        #======= plotting function definitions =======#
        #function to plot intercept over time
        plot_intercepts <- function(intercept){
            intercepts <- Metropolis_Results[[1]][,intercept]
            plot(intercepts, main = paste("Topic:",intercept,"Geweke:",round(coda::geweke.diag(intercepts)$z,2)),ylab= "Value",pch = 20)
        }

        #function to plot betas over time
        plot_betas <- function(Cluster){
            betas <- Metropolis_Results[[2]][Cluster,,]
            betas <- t(betas)
            matplot(betas, main = paste("Cluster:",Cluster,"Geweke","\n MM:",round(coda::geweke.diag(betas[,1])$z,2) , "MF:",round(coda::geweke.diag(betas[,2])$z,2) ,"\n FM:",round(coda::geweke.diag(betas[,3])$z,2) ,"FF:",round(coda::geweke.diag(betas[,4])$z,2)),ylab= "Value",pch = 20)
        }

        #function to plot latent spaces for a given actor
        plot_LS_Positions <- function(cluster){
            LSP <- matrix(0,nrow=Itterations, ncol= Latent_Spaces)
            for(i in 1:Itterations){
                LSP[i,1] <- Metropolis_Results[[3]][2*i-1,cluster,LS_Actor]
                LSP[i,2] <- Metropolis_Results[[3]][2*i,cluster,LS_Actor]
            }
            corel <- cor(LSP[,1], LSP[,2])
            matplot(LSP, main = paste("Cluster:",cluster, "\n LS Correlations:", round(corel,3),"\n Geweke - LS1:",round(coda::geweke.diag(LSP[,1])$z,2),"LS2:",round(coda::geweke.diag(LSP[,2])$z,2)),ylab= "Value",pch = 20)
        }
        
        plot_Cluster_present_edge_Network <- function(Cluster){
            #get all topics assigned to cluster
            cluster_indexes <- which(Last_Cluster_Topic_Assignments == Cluster)
            #create matrix to add to
            slice <- matrix(0, Actors,Actors)
            if(length(cluster_indexes) > 0){
                for(i in cluster_indexes){
                    slice <- slice + Topic_Present_Edge_Counts[,,i]
                }
            }
            coordinates <- cbind(Metropolis_Results[[3]][2*Itterations-1,Cluster,],Metropolis_Results[[3]][2*Itterations,Cluster,])
            coordinates <- as.data.frame(coordinates)
            
            gend <- Author_Attributes$Gender
            colors <- rep("", length(gend))
            for(l in 1:length(gend)){
                if(gend[l] == "M" | gend[l] == "Male"){
                    colors[l] <- "purple"
                }else{
                    colors[l] <- "orange"
                }
            }
            
            plot(coordinates,main = paste("Topic:",Cluster,"Number of Edges:",sum(slice),"\n", Cluster_Ten_Words[Cluster]),pch = 20, col = "red")
            #add in lines between actors
            for(i in 1:Actors){
                for(j in 1:Actors){
                    if(slice[i,j] >0){
                        lines(c(coordinates[i,1],coordinates[j,1]) , c(coordinates[i,2],coordinates[j,2]), col = "black")
                    }
                }
            }
            points(coordinates,col = colors, pch = 19, cex = 2)
            
        }
        
        plot_Cluster_absent_edge_Network <- function(Cluster){
            #get all topics assigned to cluster
            cluster_indexes <- which(Last_Cluster_Topic_Assignments == Cluster)
            #create matrix to add to
            slice <- matrix(0, Actors,Actors)
            if(length(cluster_indexes) > 0){
                for(i in cluster_indexes){
                    slice <- slice + Topic_Absent_Edge_Counts[,,i]
                }
            }
            coordinates <- cbind(Metropolis_Results[[3]][2*Itterations-1,Cluster,],Metropolis_Results[[3]][2*Itterations,Cluster,])
            coordinates <- as.data.frame(coordinates)
            plot(coordinates,main = paste("Topic:",Cluster,"Number of Edges:",sum(slice),"\n", Cluster_Four_Words[Cluster]),pch = 20, col = "red")
            #add in lines between actors
            for(i in 1:Actors){
                for(j in 1:Actors){
                    if(slice[i,j] >0){
                        lines(c(coordinates[i,1],coordinates[j,1]) , c(coordinates[i,2],coordinates[j,2]), col = "black")
                    }
                }
            }
            
        }
        
        #make plots will all top words for one per page output
        plot_Topic_Network_Full <- function(Cluster){
            slice <- Topic_Present_Edge_Counts[,,Cluster]
            coordinates <- cbind(Metropolis_Results[[3]][2*Itterations-1,Cluster,],Metropolis_Results[[3]][2*Itterations,Cluster,])
            coordinates <- as.data.frame(coordinates)
            plot(coordinates,main = paste("Cluster:",Cluster,"Number of Edges:",sum(slice),"\n", Cluster_Ten_Words[topic]),pch = 20, col = "red")
            #add in lines between actors
            for(i in 1:Actors){
                for(j in 1:Actors){
                    if(slice[i,j] >0){
                        lines(c(coordinates[i,1],coordinates[j,1]) , c(coordinates[i,2],coordinates[j,2]), col = "black")
                    }
                }
            }
            
        }
        
        #function to plot log likelihoods over time
        plot_likelihoods <- function(Cluster){
            likelihoods <- cbind(Metropolis_Results[[5]][,Cluster],Metropolis_Results[[6]][,Cluster])
            matplot(likelihoods, main = paste("Log Likelihoods of Current and Proposed Positions over Time For Cluster", Cluster, "\n Geweke Statistic:", round(coda::geweke.diag(likelihoods[,1])$z,2)),ylab= "Value",pch = 20)
        }
        
        #function to plot log ratios vs log uniform draws over time
        plot_ratio_lud <- function(Cluster){
            #likelihoods <- matrix(0,ncol = 2, nrow = Itterations)
            accept <- rep(0,Itterations)
            likelihoods <- rep(0,Itterations)
            lud <- rep(0,Itterations)
            colors <- rep("",Itterations)
            for(i in 1:Itterations){
                likelihoods[i] <- Metropolis_Results[[5]][i,Cluster] - Metropolis_Results[[6]][i,Cluster] #- Metropolis_Results[[6*Itterations+i]]
                accept[i] <- Accept_Rates[i,Cluster]
                if(accept[i] == 1){
                    col <- "blue"
                }else{
                    col <- "red"
                }
                colors[i] <- col
                
                accepted <- accept
                
            }
            print(paste("Average Accepted Prob:",mean(accepted)))
            len <- length(accepted)
            print(paste("Average Accepted Prob last 10 percent:",mean(accepted[(len - len/10):len])))
            print(paste("Average Log Ratio:",mean(likelihoods)))
            print(paste("Average Log Uniform Draw:",mean(lud)))
            plot(loess.smooth(x = 1:length(accepted), y = accepted, family = "gaussian"), main = "Log Likelihood Ratio of Accepted Proposals",ylab= "Value",xlab = "Iterations",pch = 20)
            plot(likelihoods, main = "Red represents rejected proposals, blue represents accepted proposals",ylab= "Value",pch = 20, col = colors)
        }

############## Pretty Plotting Functions for Infographics
        Pretty_Present_Edge_Network_Plots <- function(Cluster, return_coords = F){
            print(paste("generating pretty network plots for cluster:", Cluster))
            #get all topics assigned to cluster
            cluster_indexes <- which(Last_Cluster_Topic_Assignments == Cluster)
            #create matrix to add to
            
            slice <- matrix(0, Actors,Actors)
            if(length(cluster_indexes) > 0){
                for(i in cluster_indexes){
                    slice <- slice + Topic_Present_Edge_Counts[,,i]
                }
                
            }
            
            number_of_emails <- 0 
            #see how many documents are represented
            for(k in 1:length(Token_Topic_Assignments)){
                temp <- Token_Topic_Assignments[[k]]
                in_this_email <- 0
                for(j in 1:length(temp)){
                    for(i in cluster_indexes){
                        if(temp[j] == i){
                            in_this_email <- 1
                        }
                    } 
                }
                number_of_emails <-number_of_emails + in_this_email
            }
            
            #generate mean coordinates
            base <- cbind(Metropolis_Results[[3]][2*1-1,Cluster,],Metropolis_Results[[3]][2*1,Cluster,])
            mean_coordinates <- matrix(0, nrow = Actors, ncol = 2)
            for(i in 1:Itterations){
                temp <- cbind(Metropolis_Results[[3]][2*i-1,Cluster,],Metropolis_Results[[3]][2*i,Cluster,])
                rotated <- vegan::procrustes(base,temp,scale=F)$Yrot
                mean_coordinates <- mean_coordinates + rotated
            }
            
            #take the average
            mean_coordinates <- mean_coordinates/Itterations
            mean_coordinates <- as.data.frame(mean_coordinates)
            
            #deal with case where we do or do not have mixing parameters included
            if(used_binary_mixing_attribute){
              attr_index <- which(colnames(Author_Attributes) == binary_mixing_attribute_name)
              attr <- Author_Attributes[,attr_index]
              unique_attrs <- unique(attr)
              colors2 <- rep("yellow", length(attr))
              colors3 <- rep("yellow", length(attr))
              shapes <- rep(0, length(attr))
              for(l in 1:length(attr)){
                if(attr[l] == unique_attrs[1]){
                  colors2[l] <- rgb(153,0,51,255,maxColorValue = 255)
                  colors3[l] <- rgb(153,0,51,20,maxColorValue = 255)
                  shapes[l] <- 15
                }else{
                  colors2[l] <- rgb(102,102,204,255,maxColorValue = 255)
                  colors3[l] <- rgb(102,102,204,20,maxColorValue = 255)
                  shapes[l] <- 17
                }
              }
            }else{
              colors2 <- rep(rgb(102,102,204,255,maxColorValue = 255), Actors)
              colors3 <- rep(rgb(102,102,204,20,maxColorValue = 255), Actors)
              shapes <- rep(15, Actors)
            }

            #generate edge colors
            linecolor <- colorRampPalette(c("grey90", "black"))
            cols <- linecolor(21)
            min <- min(slice)
            max <- max(slice)
            difference <- max - min 
            vec <- seq(min,max,difference/20)
            
            colors <- rep("white",length(slice))
            for(j in 1:length(slice)){
                already <- F
                for(k in 1:length(vec)){
                    if(!already){
                        if(slice[j] == 0){
                            colors[j]<- cols[k]
                            already <- T
                        }else{
                            if(vec[k] >= slice[j]){
                                if((k + 1) > length(vec)){
                                    index <- length(vec)
                                }else{
                                    index <- k+1
                                }
                                colors[j]<- cols[index]
                                already <- T
                            }
                        }
                    }
                }
            }

            par(bg = "white") 

            plot(mean_coordinates,main = paste("Cluster:",Cluster,"Fraction of Edges:",round(sum(slice)/sum(Topic_Present_Edge_Counts),3), "--- Number of Emails Represented:",number_of_emails," of ",length(Token_Topic_Assignments), "\n", "Darker edges indicate more communication"),pch = 20, col = "white", axes = F, xlab = "", ylab = "")
            box(which = "plot")

            storage <- matrix(0,nrow = Itterations, ncol = 2*Actors)
            base <- cbind(Metropolis_Results[[3]][2*1-1,Cluster,],Metropolis_Results[[3]][2*1,Cluster,])
            for(i in 1:Itterations){
                temp <- cbind(Metropolis_Results[[3]][2*i-1,Cluster,],Metropolis_Results[[3]][2*i,Cluster,])
                rotated <- vegan::procrustes(base,temp,scale=F)$Yrot
                
                counter <- 1
                for(j in 1:Actors){
                    storage[i,2*j-1] <- rotated[j,1]
                    storage[i,2*j] <- rotated[j,2]
                    points(x= rotated[j,1],y= rotated[j,2],col = colors3[j], pch = shapes, cex = .4)  
                }   
            }
            
            #add in lines between actors in order from dimmest to brightest
            total <- length(which(slice >0))
            
            #iff there are any edges assigned to this cluster
            if(total > 0){
              ordering <- order(slice, decreasing = F)
              correct_ordering <- rep(0,length(ordering))
              for(i in 1:length(ordering)){
                correct_ordering[ordering[i]] <- i
              }
              add_matrix <- matrix(correct_ordering,ncol = Actors,nrow = Actors)
              colormat <- matrix(colors,ncol = Actors,nrow = Actors)

              counter <- 1
              for(l in 1:length(ordering)){
                for(i in 1:Actors){
                  for(j in 1:Actors){
                    if(add_matrix[i,j] == l & slice[i,j] > 0){
                      lines(c(mean_coordinates[j,1],mean_coordinates[i,1]) , c(mean_coordinates[j,2],mean_coordinates[i,2]), col = colormat[i,j], lwd = 1.5)
                    }
                  }
                }
              }
            }
            
            
            
            
            points(mean_coordinates,col = colors2, pch = shapes, cex = 1.5)
            if(used_binary_mixing_attribute){
              legend("topright", inset=.05,c(unique_attrs[1], unique_attrs[2]), fill=c(rgb(153,0,51,255,maxColorValue = 255),rgb(102,102,204,255,maxColorValue = 255)), horiz=F, bg = "white")
            }
            
            if(return_coords){
                return(mean_coordinates)
            }
            
        }

        Pretty_Beta_Estimates <- function(Cluster){
          attr_index <- which(colnames(Author_Attributes) == binary_mixing_attribute_name)
          attr <- Author_Attributes[,attr_index]
          unique_attrs <- unique(attr)
            betas <- matrix(0,ncol = 4, nrow = Itterations)
            for(i in 1:Itterations){
                betas[i,] <- Metropolis_Results[[2]][Cluster,,i]
            }
            boxplot(betas, col = c("grey","grey", "grey", "grey"), names = c(
              paste(unique_attrs[1],"-",unique_attrs[1]," (Fixed)",sep = ""), paste(unique_attrs[1],"-",unique_attrs[2],sep = ""),paste(unique_attrs[2],"-",unique_attrs[1],sep = ""), paste(unique_attrs[2],"-",unique_attrs[2],sep = "")),ylab = "Mixing Parameter Estimate", main = "Mixing Parameter Estimates")
        }

        Pretty_Topic_Top_Words <- function(Cluster){
            
            cluster_indexes <- which(Last_Cluster_Topic_Assignments == Cluster)
            
            clust_email_assignments <- Email_Assignments[cluster_indexes]
            clust_top_ten_words <- Top_Ten_Words[cluster_indexes]
            
            #find the largest 10
            temp <- clust_email_assignments
            num_topics <- min(10,length(cluster_indexes))
            indexes <- rep(0,num_topics)
            for(i in 1:num_topics){
                maxemail <- which(temp == max(temp))
                indexes[i] <- maxemail
                temp[maxemail] <- 0
            }
            
            clust_email_assignments <- clust_email_assignments[indexes]
            clust_top_ten_words <- clust_top_ten_words[indexes]
            
            bp <- gplots::barplot2(clust_email_assignments, beside = TRUE, horiz = TRUE, col = "grey", border= "grey", ylab = "", xlab = "Number of Edges Assigned to Topic",main =paste("Top Words for Top",num_topics,"Topics")) 
            text(0,bp,clust_top_ten_words,cex=1,pos=4)# label on the bars themselves 
        }
    

        Pretty_Cluster_Likelihood <- function(Cluster){
            likelihoods <- cbind(Metropolis_Results[[5]][,Cluster],Metropolis_Results[[6]][,Cluster])
            if(used_binary_mixing_attribute){
              bets <- length(which(abs(unlist(beta_list[(4*(Cluster-1)+2):(4*Cluster)])) < 1.645))
            }
            
            
            ls <- length(which(abs(unlist(LS_list[(Actors*2*(Cluster-1)+1):(Actors*2*Cluster)])) < 1.645))
            num_ls <- length((Actors*2*(Cluster-1)+1):(Actors*2*Cluster))
            
            ints <- intercept_list[[Cluster]]
            
            if(used_binary_mixing_attribute){
              matplot(likelihoods, main = paste(pretty_name,": Trace Plot of Log Likelihoods For Cluster", Cluster, "\n Geweke Statistic:", round(coda::geweke.diag(likelihoods[,1])$z,2),paste(" --- Percent Betas Converged ($z < 1.645$):",round(bets/3,3)), "\n",paste(" Percent LS Positions Converged ($z < 1.645$):",round(ls/num_ls,3)),"  --- Intercept Geweke Statistic:",round(ints,3) ),ylab= "Log Likelihood",pch = 20)
            }else{
              matplot(likelihoods, main = paste(pretty_name,": Trace Plot of Log Likelihoods For Cluster", Cluster, "\n Geweke Statistic:", round(coda::geweke.diag(likelihoods[,1])$z,2),"\n",paste(" Percent LS Positions Converged ($z < 1.645$):",round(ls/num_ls,3)),"  --- Intercept Geweke Statistic:",round(ints,3) ),ylab= "Log Likelihood",pch = 20)
            }
            
            legend("bottomright",c("Proposed", "Current"), fill=c("black","red"), horiz=T)
            
        }


        Pretty_Cluster_Trace <- function(Cluster){
            likelihoods <- Metropolis_Results[[6]][,Cluster]
            
            plot(likelihoods, main = paste("Trace Plot of Log Likelihoods", "\n Geweke Statistic:", round(coda::geweke.diag(likelihoods)$z,2)),ylab= "Log Likelihood", type="n")
            lines(likelihoods)
            
        }

        # ======= Generate Plots and save as PDFs =========#      
        make_plots <- function(Width, Height, parrow,parcol){
            #generate pdf of intercepts
            print("Plotting Intercepts...")
            pdf(file=paste(out_directory,output_name,"_Intercepts.pdf",sep = ""), width = Width, height = Height)
            par(mfrow= c(parrow,parcol))
            sapply(1:Clusters,plot_intercepts)
            dev.off()
            
            if(used_binary_mixing_attribute){
              #generate pdf of intercepts 
              print("Plotting Betas...")
              pdf(file=paste(out_directory,output_name,"_Betas.pdf",sep = ""), width = Width, height = Height)
              par(mfrow= c(parrow,parcol))
              sapply(1:Clusters,plot_betas)
              dev.off()
            }
            
            
            #generate pdf of intercepts 
            print("Plotting Latent Space Positions...")
            pdf(file=paste(out_directory,output_name,"_LS_Positions.pdf",sep = ""), width = Width, height = Height)
            par(mfrow= c(parrow,parcol))
            sapply(1:Clusters,plot_LS_Positions)
            dev.off()

            pdf(paste(out_directory,output_name,"_Likelihoods.pdf", sep = ""),height=Height,width=Width,pointsize=7)
            par(mfrow= c(parrow,parcol))
            sapply(1:Clusters,plot_likelihoods) 
            dev.off()
            
            #generate network plots one per page
            print("Plotting Networks...")
            pdf(file=paste(out_directory,output_name,"_Cluster_Absent_Edge_Networks.pdf",sep = ""), width = Width, height = Height)
            par(mfrow= c(parrow,parcol))
            sapply(1:Clusters,plot_Cluster_absent_edge_Network)
            dev.off()
            
            pdf(file=paste(out_directory,output_name,"_Cluster_Present_Edge_Networks.pdf",sep = ""), width = Width, height = Height)
            par(mfrow= c(parrow,parcol))
            sapply(1:Clusters,plot_Cluster_present_edge_Network)
            dev.off()
        }
        
        #stolen from the R cookbook
        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)
            
            numPlots = length(plots)
            
            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols), byrow= T)
            }
            
            if (numPlots==1) {
                print(plots[[1]])
                
            } else {
                # Set up the page
                grid::grid.newpage()
                grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                    # Get the i,j matrix positions of the regions that contain this subplot
                    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                    
                    print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                    layout.pos.col = matchidx$col))
                }
            }
        }
        
        if(!only_print_summaries){
            #now actually generate the plots based on the number of topics
            if(Clusters <= 4){
                make_plots(12,12,2,2)
            }else if(Clusters <= 9 ){
                make_plots(12,12,3,3)
            }else if(Clusters <= 10){
                make_plots(15,9,2,5)
            }else if(Clusters <= 20){
                make_plots(25,25,4,5)
            }else{
                make_plots(50,25,6,10)   
            }
            
            par(mfrow= c(1,1))
        }
        
        
        
        # ======= Now generate top words and topic model diagnostics ====== #
        
        print("Generating Topic Model Output")

        if(!only_print_summaries){
            pdf(paste(out_directory,output_name,"_Top_Words.pdf", sep = ""),height=12,width=10,pointsize=7)
            par(mfrow= c(1,1))
            bp <- gplots::barplot2(Email_Assignments, beside = TRUE, horiz = TRUE, col = "lightblue1", border= "lightblue1", ylab = "Topic:", xlab = "Number of Edges Assigned to Topic",main = paste("Top Words by Number of Words Assigned to Topic for ",output_name,sep = "")) 
            text(0,bp,Top_Ten_Words,cex=1,pos=4)# label on the bars themselves 
            dev.off()

            pdf(paste(out_directory,output_name,"_Log_Ratio_LUD.pdf", sep = ""),height=8,width=12,pointsize=7)
            par(mfrow= c(5,2))
            sapply(1:Clusters,plot_ratio_lud) 
            dev.off() 
        }

        print("Outputing Geweke statistics..")
        
        intercept_list <- list()
        beta_list <- list()
        LS_list <- list()
        t= 1
        for(t in 1:Clusters){
            Cluster <- t
            print(paste("Current Cluster: ", t))
            #Intercepts
            intercepts <- Metropolis_Results[[1]][,t]
            int <- as.numeric(coda::geweke.diag(intercepts)$z)
            intercept_list <- append(intercept_list,int)
            
            #Betas
            if(used_binary_mixing_attribute){
              betas <- matrix(0,ncol = 4, nrow = Itterations)
              for(i in 1:Itterations){
                betas[i,] <- Metropolis_Results[[2]][t,,i]
              }
              bets <- rep(0,4)
              for(i in 1:4){
                bets[i] <- as.numeric(coda::geweke.diag(betas[,i])$z)
              } 
              beta_list <- append(beta_list,bets)
            }

            #generate vegan::procrustes transformed positions
            storage <- matrix(0,nrow = Itterations, ncol = 2*Actors)
            base <- cbind(Metropolis_Results[[3]][2*1-1,Cluster,],Metropolis_Results[[3]][2*1,Cluster,])
            for(i in 1:Itterations){
                temp <- cbind(Metropolis_Results[[3]][2*i-1,Cluster,],Metropolis_Results[[3]][2*i,Cluster,])
                rotated <- vegan::procrustes(base,temp,scale=F)$Yrot
                
                counter <- 1
                for(j in 1:Actors){
                    storage[i,2*j-1] <- rotated[j,1]
                    storage[i,2*j] <- rotated[j,2]
                }   
            }

            #Latent Space Positions
            lat <- matrix(0,nrow=Actors, ncol= Latent_Spaces)
            for(a in 1:Actors){
                LSP <- matrix(0,nrow=Itterations, ncol= Latent_Spaces)
                for(i in 1:Itterations){
                    LSP[i,1] <- storage[i,2*a-1]
                    LSP[i,2] <- storage[i,2*a]
                }
                #calcaulte statistic
                for(j in 1:Latent_Spaces){
                    lat[a,j] <- as.numeric(coda::geweke.diag(LSP[,j])$z)
                }
            }
            LS_list <- append(LS_list,lat)
            
        }
        
        ints <- length(which(abs(unlist(intercept_list)) < 1.645))
        print(paste("Percent Intercepts Converged ($z < 1.645$):",ints/length(intercept_list), "Number:", ints))
        
        if(used_binary_mixing_attribute){
          bets <- length(which(abs(unlist(beta_list)) < 1.645))
          print(paste("Percent Betas Converged ($z < 1.645$):",bets/length(beta_list), "Number:", bets))
        }
        
        
        ls <- length(which(abs(unlist(LS_list)) < 1.645))
        print(paste("Percent LS Positions Converged ($z < 1.645$):",ls/length(LS_list), "Number:", ls))
        
        if(used_binary_mixing_attribute){
          total <- ints + bets +ls
        }else{
          total <- ints +ls
        }
        print(paste("Percent All Parameters Converged ($z < 1.645$):",total/(length(beta_list)+ length(LS_list) + length(intercept_list)), "Number:", total))
        

        for(clust in Clusters_to_Pretty_Print){
            
            
            if(!only_print_summaries){
                
              if(used_binary_mixing_attribute){
                pdf(paste(out_directory,output_name,"_Pretty_Plot_Cluster",clust,".pdf", sep = ""),height=8,width=10,pointsize=7)
                
                layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
                       widths=c(3,1), heights=c(1,2))
                
                Pretty_Cluster_Likelihood(clust)
                Pretty_Beta_Estimates(clust)
                Pretty_Present_Edge_Network_Plots(clust)
                Pretty_Topic_Top_Words(clust)
                dev.off()
              }else{
                pdf(paste(out_directory,output_name,"_Pretty_Plot_Cluster",clust,".pdf", sep = ""),height=8,width=10,pointsize=7)
                
                layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
                       widths=c(3,1), heights=c(1,2))
                
                Pretty_Cluster_Likelihood(clust)
                plot.new()
                Pretty_Present_Edge_Network_Plots(clust)
                Pretty_Topic_Top_Words(clust)
                dev.off()
              }

                pdf(paste(out_directory,output_name,"_Network",clust,".pdf", sep = ""),height=6,width=6,pointsize=7)
                
                
                par(mfrow = c(1,1))
                Pretty_Present_Edge_Network_Plots(clust)
                dev.off()
                
              if(used_binary_mixing_attribute){
                pdf(paste(out_directory,output_name,"_Beta_and_Trace",clust,".pdf", sep = ""),height=6,width=3,pointsize=7)
                par(mfrow = c(2,1))
                Pretty_Cluster_Trace(clust)
                Pretty_Beta_Estimates(clust)
                dev.off()
              }

                pdf(paste(out_directory,output_name,"_clust_topics",clust,".pdf", sep = ""),height=5,width=4,pointsize=7)
                par(mfrow= c(1,1))
                Pretty_Topic_Top_Words(clust)
                dev.off()
            }
        }

        par(mfrow= c(1,1))

        Actor_Dataset <- Author_Attributes
        #print one documen for whole county
        pdf(paste(out_directory,output_name,"_All_Clusters.pdf", sep = ""),height=8,width=10,pointsize=7)
        layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(3,1), heights=c(1,2))
         
        for(clust in Clusters_to_Pretty_Print){
            Pretty_Cluster_Likelihood(clust)
            if(used_binary_mixing_attribute){
              Pretty_Beta_Estimates(clust)
            }else{
              plot.new()
            }
            
            Actor_Dataset <- cbind(Actor_Dataset, Pretty_Present_Edge_Network_Plots(clust,T))
            Pretty_Topic_Top_Words(clust)
        }

        dev.off()

      if(used_binary_mixing_attribute){
        attr_index <- which(colnames(Author_Attributes) == binary_mixing_attribute_name)
        attr <- Author_Attributes[,attr_index]
        unique_attrs <- unique(attr)
        #generate asortativity - token dataset
        cat("Generating Datasets... \n")
        get_beta_estimates <- function(Cluster){
          betas <- matrix(0,ncol = 4, nrow = Itterations)
          for(i in 1:Itterations){
            betas[i,] <- Metropolis_Results[[2]][Cluster,,i]
          }
          mean_se <- as.data.frame(matrix(0,nrow=4, ncol = 2))
          mean_se <- cbind(c("1-1", "1-2","2-1", "2-2"),mean_se)
          names(mean_se) <- c("Tie","Parameter_Estimate","SE")
          for(j in 1:length(mean_se[,1])){
            mean_se[j,2] <- mean(betas[,j])
            mean_se[j,3] <- sd(betas[,j]) 
          }
          return(mean_se[,2:3])
        }
        
        beta_averages <- matrix(0, ncol = 4,nrow = Clusters)
        beta_se <- matrix(0, ncol = 4,nrow = Clusters)
        for(i in 1:Clusters){
          result <- get_beta_estimates(i)
          beta_averages[i,] <- result[,1]
          beta_se[i,] <- result[,2] 
        }

        Clusters_intercepts <- matrix(0,ncol = 2,nrow= Clusters)
        for(c in 1:Clusters){
          intercepts <- Metropolis_Results[[1]][,c]
          for(i in 1:Itterations){
            intercepts[i] <- Metropolis_Results[[1]][i,c]
          }
          Clusters_intercepts[c,1] <- mean(intercepts)
          Clusters_intercepts[c,2] <- sd(intercepts)
        }
        county <- matrix(pretty_name,ncol = 1,nrow = Clusters)
        county2 <- matrix(pretty_name,ncol = 1,nrow = Actors)
        county3 <- matrix(pretty_name,ncol = 1,nrow = Topics)
        
        Cluster_Data <- cbind(county,Clusters_intercepts,beta_averages,beta_se,Cluster_Top_Twenty_Words)
         cols1 <- c("Organization","Intercept","Intercept_SE",paste(unique_attrs[1],"-",unique_attrs[1],sep = ""), paste(unique_attrs[1],"-",unique_attrs[2],sep = ""),paste(unique_attrs[2],"-",unique_attrs[1],sep = ""), paste(unique_attrs[2],"-",unique_attrs[2],sep = ""),paste(unique_attrs[1],"-",unique_attrs[1],"_SE",sep = ""), paste(unique_attrs[1],"-",unique_attrs[2],"_SE",sep = ""),paste(unique_attrs[2],"-",unique_attrs[1],"_SE",sep = ""), paste(unique_attrs[2],"-",unique_attrs[2],"_SE",sep = ""),"Top1","Top2","Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10","Top11","Top12","Top13","Top14","Top15","Top16","Top17","Top18","Top19","Top20")

        colnames(Cluster_Data) = cols1
        Actor_Dataset <- cbind(county2,Actor_Dataset)
        cad <- c("Organization",colnames(Author_Attributes))
        for(i in Clusters_to_Pretty_Print){
          cad <- c(cad, paste("C",i,"_D1",sep = ""),paste("C",i,"_D2",sep = ""))
        }
        colnames(Actor_Dataset) <- cad
        
        Token_Data <- cbind(county3,Cluster_Topic_Assigns,Email_Assignments,Topic_Top_Ten_Words, t(Word_Type_Topic_Counts))
        t1 <- c("Organization","Cluster","Edges","Top1","Top2","Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10")
        temp2 <- c(t1,unlist(vocab))
        temp2 <- unlist(temp2)
        colnames(Token_Data) <- temp2
        
        Output <- list(Cluster_Data = Cluster_Data , Actor_Data = Actor_Dataset, Token_Data = Token_Data, Vocabulary = vocab)
        return(Output)
      }else{
        ##if we did not use a mixing parameter
        #generate asortativity - token dataset
        print("Generating Dataset")
        
        Clusters_intercepts <- matrix(0,ncol = 2,nrow= Clusters)
        for(c in 1:Clusters){
          intercepts <- Metropolis_Results[[1]][,c]
          for(i in 1:Itterations){
            intercepts[i] <- Metropolis_Results[[1]][i,c]
          }
          Clusters_intercepts[c,1] <- mean(intercepts)
          Clusters_intercepts[c,2] <- sd(intercepts)
        }
        county <- matrix(pretty_name,ncol = 1,nrow = Clusters)
        county2 <- matrix(pretty_name,ncol = 1,nrow = Actors)
        county3 <- matrix(pretty_name,ncol = 1,nrow = Topics)
        
        Cluster_Data <- cbind(county,Clusters_intercepts,Cluster_Top_Twenty_Words)
        colnames(Cluster_Data) = c("County","Intercept","Intercept_SE","Top1","Top2","Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10","Top11","Top12","Top13","Top14","Top15","Top16","Top17","Top18","Top19","Top20")

        Actor_Dataset <- cbind(county2,Actor_Dataset)
        cad <- c("Organization",colnames(Author_Attributes))
        for(i in Clusters_to_Pretty_Print){
          cad <- c(cad, paste("C",i,"_D1",sep = ""),paste("C",i,"_D2",sep = ""))
        }
        colnames(Actor_Dataset) <- cad
        Token_Data <- cbind(county3,Cluster_Topic_Assigns,Email_Assignments,Topic_Top_Ten_Words, t(Word_Type_Topic_Counts))
        #colnames(Token_Data) <- 
        t1 <- c("Organization","Cluster","Edges","Top1","Top2","Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10")
        temp2 <- c(t1,unlist(vocab))
        temp2 <- unlist(temp2)
        colnames(Token_Data) <- temp2
        
        Output <- list(Cluster_Data = Cluster_Data , Actor_Data = Actor_Dataset, Token_Data = Token_Data, Vocabulary = vocab)
        return(Output)
      }
}#end of function definition




