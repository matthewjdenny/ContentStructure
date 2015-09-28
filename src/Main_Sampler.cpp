// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
#include <boost/random.hpp>
#include <boost/random/uniform_01.hpp>
#include <math.h>
#include <cmath>
#include "Normal_Distribution_No_Assert.cpp"
#include "Discrete_Distribution_No_Assert.cpp"

using std::pow;
using std::exp;
using std::sqrt;
using std::log;
using namespace Rcpp;

// [[Rcpp::export]]
List Main_Sampler(
    int number_of_outer_itterations,
    int number_of_Gibbs_itterations,
    int number_of_MH_itterations,
    int number_of_actors, 
    int number_of_topics,
    int number_of_clusters,
    int number_of_latent_dimensions,
    int number_of_documents,
    arma::vec proposal_variance,
    arma::vec topic_cluster_assignments,
    NumericVector clp, 
    NumericVector plp,
    arma::vec current_intercepts,
    arma::mat betas,
    int number_of_betas,
    NumericVector indicator_array,
    arma::mat observed_edges,
    List token_topic_assignment_list,
    List token_word_type_list,
    arma::vec document_authors,
    double beta,
    arma::vec alpha_m,
    arma::mat token_type_topic_counts,
    arma::vec topic_token_sums,
    int number_of_word_types,
    int itterations_before_cluster_assingment_update,
    double metropolis_target_accpet_rate,
    double step_size,
    int slice_sample_alpha,
    double MH_prior_standard_deviation,
    int seed
    ){
    int list_offset = 19;
    int list_length = (1 + list_offset);
    List to_return(list_length);
    int Gibbs_Counter = 0;
    int MH_Counter = 0;
    int slice_sample_counter = 0;
    
    arma::vec cluster_accept(number_of_clusters);
    arma::vec Proposed_MH_Likelihoods(number_of_clusters);
    arma::vec Current_MH_Likelihoods(number_of_clusters);
    arma::vec Topic_Model_Likelihoods(number_of_outer_itterations);
    arma::mat MH_acceptances(number_of_MH_itterations, number_of_clusters);
    arma::mat cur_proposal_variances(number_of_outer_itterations,number_of_clusters);
    arma::mat cur_accept_rates(number_of_outer_itterations,number_of_clusters);
    arma::mat all_topic_cluster_assignments(number_of_outer_itterations,number_of_topics);
    arma::mat store_intercepts(number_of_MH_itterations,number_of_clusters);
    arma::cube store_betas(number_of_clusters,number_of_betas,number_of_MH_itterations);
    arma::cube store_latent_positions(number_of_latent_dimensions*number_of_MH_itterations,number_of_clusters,number_of_actors);
    arma::mat store_cluster_whether_accepted(number_of_MH_itterations,number_of_clusters);
    arma::mat store_cluster_proposed_likelihoods(number_of_MH_itterations,number_of_clusters);
    arma::mat store_cluster_current_likelihoods(number_of_MH_itterations,number_of_clusters);

    // Set RNG and define uniform distribution 
    //boost::mt19937_64 generator(seed);
    boost::mt19937 generator(seed);
    boost::uniform_01<double> uniform_distribution;

    arma::cube topic_present_edge_counts = arma::zeros(number_of_actors,number_of_actors,number_of_topics);
    arma::cube topic_absent_edge_counts = arma::zeros(number_of_actors,number_of_actors,number_of_topics);
    
    //read in latent positions array [num dimensions x topics x actors]
    IntegerVector arrayDims3 = clp.attr("dim");
    arma::cube current_latent_positions(clp.begin(), arrayDims3[0], arrayDims3[1], arrayDims3[2], false);
    
    //read in beta indicator array (0,1) [number of topics x number of actors x number of betas]
    IntegerVector arrayDims5 = indicator_array.attr("dim"); 
    arma::cube beta_indicator_array(indicator_array.begin(), arrayDims5[0], arrayDims5[1], arrayDims5[2], false);
    
    //outer loop over the number of itterations (default 1000)
    for(int n = 0; n < number_of_outer_itterations; ++n){
        Rcpp::Rcout << "Current Gibbs Iteration: " << n << std::endl;
        
        // ===================================================== //
        // ===================== Gibbs Step ==================== //
        // ===================================================== //
        for(int i = 0; i < number_of_Gibbs_itterations; ++i){

            //initialize variables that will be used across iterations
            double beta_val = 0;
            arma::vec current_author_position = arma::zeros(number_of_latent_dimensions);
            arma::vec recipient_position = arma::zeros(number_of_latent_dimensions);
            arma::vec current_topic_betas = arma::zeros(number_of_betas);
            
            // ========== token topic assignment step =============
            for(int d = 0; d < number_of_documents; ++d){
                //set all document specific parameters 
                int document_author = document_authors[d] - 1;
                arma::vec token_topic_assignments1 = token_topic_assignment_list[d];
                int number_of_tokens = token_topic_assignments1.n_elem;
                arma::vec token_word_types = token_word_type_list[d];
                
                //loop over tokens
                for(int w = 0; w < number_of_tokens; ++w){
                    
                    //initialize token topic distribution vector
                    arma::vec token_topic_distribution(number_of_topics);
                    //loop over topics
                    for(int t = 0; t < number_of_topics; ++t){
                        int topic = t + 1;
                        
                        //get the current topic's cluster assignment
                        double cluster = topic_cluster_assignments[t] - 1;
                        for(int c = 0; c < number_of_latent_dimensions; ++c){
                          current_author_position[c] = current_latent_positions(c,cluster,document_author);
                        }
                        arma::rowvec current_cluster_betas = betas.row(cluster); 
                        
                        //this calculates the addition to the probability that the token was sampled from the topic by
                        //adding together edge likelihoods associated with that topic in the document
                        double additional_edge_probability = 0;
                        
                        //add to the log prob for every author because all edges are assigned to every token and visa versa
                        for(int a = 0; a < number_of_actors; ++a){
                            //no self loops
                            if(document_author != a){
                                //get whether or not there was an observed edge
                                int actual_edge = observed_edges(d,a);  
                                //get current recipient position
                                for(int c = 0; c < number_of_latent_dimensions; ++c){
                                  recipient_position[c] = current_latent_positions(c,cluster,a);
                                }
                                
                                //initialize distance
                                double distance = 0;
                                //calculate distance
                                for(int k = 0; k < number_of_latent_dimensions; ++k){
                                    distance += pow((current_author_position[k] - recipient_position[k]),2);
                                }
                        
                                beta_val = 0; 
                                for(int c = 1; c < number_of_betas; ++c){
                                    beta_val += current_cluster_betas[c]*beta_indicator_array(document_author,a,c);
                                }
                                
                                //calculate linear predictor
                                double eta = 0;
                                eta = current_intercepts[cluster] - pow(distance,.5) + beta_val;
                                
                                //double eta = current_cluster_intercept - pow(distance,.5) + beta_val;
                        
                                double log_prob = 0;
                                if(eta < 0){
                                    if(actual_edge == 1){
                                        log_prob = eta -log(1 + exp(eta));
                                    }
                                    else{
                                        log_prob = 0 -log(1 + exp(eta));
                                    }
                                }
                                else{
                                    if(actual_edge == 1){
                                        log_prob = 0 -log(1 + exp(-eta));
                                    }
                                    else{
                                        log_prob = 0 -eta -log(1 + exp(-eta));
                                    }
                                }
                                additional_edge_probability += log_prob;  
                            } 
                        }
                        
                        
                        double temp = 0;
                        if(token_word_types[0] != -1){
                          //now we calculate the first and second terms in the likelihood of of the token being from the current topic
                          //calculate the number of times a token in the current document has been assigned to the current topic
                          int ntd = 0;
                          for(int b = 0; b < number_of_tokens; ++b){
                            if(b != w){
                              if(token_topic_assignments1[b] == topic){
                                ntd +=1;
                              }
                            }
                          }
                          
                          int current_word = token_word_types[w] -1;
                          //get the number of times this word type has been assigned in the current topic
                          int wttac = token_type_topic_counts(current_word,t);
                          //number of tokens assigned to the topic
                          int ntt = topic_token_sums[t];
                          
                          //subtract one from these counts if the current token was assigned to this topic
                          if(topic == token_topic_assignments1[w]){
                            ntt -= 1;
                            wttac -=1;
                          }
                          
                          //helps deal with wierd initializations by no allowing negative counts
                          if(wttac < 0){
                            wttac = 0;
                          }
                          double first_term = double(ntd) + alpha_m[t];
                          double second_term = (wttac + (beta/double(number_of_word_types)))/(double(ntt) + beta);
                          //combine terms and add to conditional posterior distribution
                          temp = log(first_term) + log(second_term);
                        }
                        //this adds on the normalize edge contribution after weighting
                        double temp2 = temp + additional_edge_probability;
                        token_topic_distribution[t] = exp(temp2);
                    }   
        
                    //save old topic asignemnt
                    int old_topic = token_topic_assignments1[w];
                   
                    mjd::random::discrete_distribution<int> distribution (token_topic_distribution.begin(),token_topic_distribution.end());
                    int temp = distribution(generator) +1;
                    token_topic_assignments1[w] = temp ; 
                    int new_topic = token_topic_assignments1[w];
                    
                    //now we need to update all of the internal counts for the same words as the current token if we sampled a new assignment
                    if(old_topic != new_topic){
                        topic_token_sums[(old_topic-1)] -=1;
                        topic_token_sums[(new_topic-1)] += 1;
                        
                        int current_word_type = token_word_types[w] -1;
                        //now for all tokens that are the same
                        token_type_topic_counts(current_word_type,(old_topic-1)) -=1;
                        token_type_topic_counts(current_word_type,(new_topic-1)) +=1;
                    }

                }//end of loop over tokens 
            
                //now assign the new token topic assignments to the appropriate place in the list.
                token_topic_assignment_list[d] = token_topic_assignments1;
            }//end of token topic assignemnt sampler document loop
            
            
            // ============= create topic present and absent edge counts ============== // 
            //zero everything out
            topic_present_edge_counts = arma::zeros(number_of_actors,number_of_actors,number_of_topics);
            topic_absent_edge_counts = arma::zeros(number_of_actors,number_of_actors,number_of_topics);
            
            for(int d = 0; d < number_of_documents; ++d){
                
                //take care of document specific assignment tasks
                int document_author = document_authors[d] - 1;
                arma::vec token_topic_assignments2 = token_topic_assignment_list[d];
                int number_of_tokens2 = token_topic_assignments2.n_elem;
                for(int a = 0; a < number_of_actors; ++a){
                    if(document_author != a){
                        int actual_edge = observed_edges(d,a);
                        arma::vec edge_log_probabilities(number_of_tokens2);
                        for(int w = 0; w < number_of_tokens2; ++w){
                            //int token = w + 1;
                            int topic_assignment = token_topic_assignments2[w] -1;
                            // do some updating
                            if(actual_edge == 1){
                                topic_present_edge_counts(document_author,a,topic_assignment) += (1/double(number_of_tokens2));
                            }else{
                                topic_absent_edge_counts(document_author,a,topic_assignment) += (1/double(number_of_tokens2));
                            }
                        }
                    }//end recipeints loop
                }//end of loop over edges for for current document
            }// end of loop over docuemnts for edge-topic assignemnt step
            //Rcpp::Rcout << "Edges Assigned to Each Topic: " << std::endl << topic_present_edge_counts << std::endl;
            // ===================== Hard Cluster Topics ====================== //
            for(int t = 0; t < number_of_topics; ++t){
                
                    arma::vec topic_cluster_distribution = arma::zeros(number_of_clusters);
                    arma::vec cluster_edges_assigned = arma::zeros(number_of_clusters);
                    
                    for(int k = 0; k < number_of_clusters; ++k){
                        
                        arma::vec current_author_position = arma::zeros(number_of_latent_dimensions);
                        arma::vec recipient_position = arma::zeros(number_of_latent_dimensions);
                        arma::rowvec current_cluster_betas = betas.row(k);
                        double sum_log_probability_of_current_positions = 0;
                        
                        //for(int a = 0; a < 0; ++a){
                        for(int a = 0; a < number_of_actors; ++a){
                            
                            //get current position
                            for(int c = 0; c < number_of_latent_dimensions; ++c){
                              current_author_position[c] = current_latent_positions(c,k,a);
                            }
                            
                            for(int b = 0; b < number_of_actors; ++b){
                                if(b!= a){
                                    
                                    double num_actual_edge = topic_present_edge_counts(a,b,t); 
                                    double num_non_edge = topic_absent_edge_counts(a,b,t);
                                    for(int c = 0; c < number_of_latent_dimensions; ++c){
                                      recipient_position[c] = current_latent_positions(c,k,b);
                                    }
                                    
                                    //initialize distance
                                    double distance = 0;
                                    //calculate distance
                                    for(int j = 0; j < number_of_latent_dimensions; ++j){
                                        distance += pow((current_author_position[j] - recipient_position[j]),2);
                                    }
                                    
                                    double beta_val = 0;
                                    for(int c = 1; c < number_of_betas; ++c){
                                        beta_val += current_cluster_betas[c]*beta_indicator_array(a,b,c);
                                    }
                                    
                                    //calculate linear predictor
                                    double eta = 0;
                                    eta = current_intercepts[k] - pow(distance,.5) + beta_val;

                                    //calculate likelihoods for both
                                    double log_prob_edge = 0;
                                    double log_prob_no_edge = 0;
                                    if (eta != 0){
                                        if(eta < 0){
                                            log_prob_edge = eta -log(1 + exp(eta));
                                            log_prob_no_edge = 0 -log(1 + exp(eta));
                                        }
                                        else{
                                            log_prob_edge = 0 -log(1 + exp(-eta));
                                            log_prob_no_edge = 0 -eta -log(1 + exp(-eta));
                                        }
                                    }
                                    
                                    //multiply and add to sum
                                    sum_log_probability_of_current_positions += num_actual_edge*log_prob_edge;
                                    sum_log_probability_of_current_positions += num_non_edge*log_prob_no_edge;
                                     
                                }
                            }
                        }
                        topic_cluster_distribution[k] = sum_log_probability_of_current_positions;
                        
                    } // end loop over clusters
                    arma::vec cluster_probabilities = arma::zeros(number_of_clusters);
                    for(int x = 0; x < number_of_clusters; ++x){
                        cluster_probabilities[x] = exp(topic_cluster_distribution[x]);
                    }
                    mjd::random::discrete_distribution<int> distribution_clusters (cluster_probabilities.begin(),cluster_probabilities.end());
                    int cluster_assignment = distribution_clusters(generator) + 1;
                    topic_cluster_assignments[t] = double(cluster_assignment);
                }//end loop over topics
        }//end of big number of itterations loop for entire  gibbs sampling step.
          
        //store the current topic-cluster assignment vector to return.
        for(int b = 0; b < number_of_topics; ++b){
            all_topic_cluster_assignments(Gibbs_Counter,b) = topic_cluster_assignments[b];
        }
        Gibbs_Counter += 1;
        
        
        // ===================================================================== //
        // ===================== Slice Sampling Step =========================== //
        // ===================================================================== //
        
        //take the log of alpha so we can slice sample
        arma::vec log_alpha_m(number_of_topics);
        for(int t = 0; t < number_of_topics; ++t){
            log_alpha_m[t] = log(alpha_m[t]);
        }
        
        // ========================== Current Probability ====================//
        //initialize variables that will be used across iterations
        double Current_Alpha_Corpus_Likelihood = 0;

        for(int d = 0; d < number_of_documents; ++d){
            //set all document specific parameters 
            arma::vec token_topic_assignments1 = token_topic_assignment_list[d];
            int number_of_tokens = token_topic_assignments1.n_elem;
            double current_document_contribution = 0;
            double current_document_contribution2 = 0;
                
            //loop over topics
            for(int t = 0; t < number_of_topics; ++t){
                int topic = t + 1; 
                //now we calculate the first and second terms in the likelihood of of the token being from the current topic
                //calculate the number of times a token in the current document has been assigned to the current topic
                int ntd = 0;
                for(int b = 0; b < number_of_tokens; ++b){
                    if(token_topic_assignments1[b] == topic){
                        ntd +=1;
                    }
                }
                //int ntt = as<int>(get_number_of_tokens_assigned_to_topic(document,token,topic));
                double first_term = double(ntd) + exp(log_alpha_m[t]);
                current_document_contribution += lgamma(first_term);
                double first_term2 = exp(log_alpha_m[t]);
                current_document_contribution2 -= lgamma(first_term2);
            }//loop over topics 
            
            double second_term = number_of_tokens + exp(double(number_of_topics)*log_alpha_m[0]);
            current_document_contribution -= lgamma(second_term);
            
            double second_term2 = exp(double(number_of_topics)*log_alpha_m[0]);
            current_document_contribution2 += second_term2;
        
            Current_Alpha_Corpus_Likelihood += current_document_contribution + current_document_contribution2;

        }//end of document loop
        
        //we have to add on an alpha at the end because we are doing the log transform
        Current_Alpha_Corpus_Likelihood += (double(number_of_topics)*log_alpha_m[1]);
        
        Rcpp::Rcout << "Corpus Log Likelihood: " << Current_Alpha_Corpus_Likelihood << std::endl;
        
        //store so we can take a look later
        Topic_Model_Likelihoods[n] = Current_Alpha_Corpus_Likelihood;
            
        slice_sample_counter +=1;
        if(slice_sample_counter == 5){
            slice_sample_counter = 0;
            // ======================== Form New Slice ======================== //
            
            //take a log uniform draw and add it to the probability of the current sample to get a floor on probabilites of new slice samples we can accept
            double rand_num = uniform_distribution(generator);
            double lud = log(rand_num);
            double slice_probability_floor = Current_Alpha_Corpus_Likelihood + lud;
            
            arma::vec proposed_alpha_m(number_of_topics);
            arma::vec left_proposed_alpha_m(number_of_topics);
            arma::vec right_proposed_alpha_m(number_of_topics);
            //get the left and right bounds on the slice for intercepts,latent positions, betas  
            double rand_num1 = uniform_distribution(generator);
            for(int t = 0; t < number_of_topics; ++t){
                left_proposed_alpha_m[t] = log_alpha_m[t] - rand_num1*step_size;
                right_proposed_alpha_m[t] = left_proposed_alpha_m[t] + step_size;
            }

            
            //set equal to one when new sample accepted
            int in_slice = 0;
            
            while(in_slice < 1){
                //report(proposed_alpha_m[0]);
                //get new values for the slice for alpha 
                double rand_num2 = uniform_distribution(generator);
                for(int t = 0; t < number_of_topics; ++t){
                    proposed_alpha_m[t] = left_proposed_alpha_m[t] + rand_num2*(right_proposed_alpha_m[t] - left_proposed_alpha_m[t]);
                } 
            
                double Proposed_Alpha_Corpus_Likelihood = 0;
        
                for(int d = 0; d < number_of_documents; ++d){
                    //set all document specific parameters 
                    arma::vec token_topic_assignments1 = token_topic_assignment_list[d];
                    int number_of_tokens = token_topic_assignments1.n_elem;
                    double current_document_contribution = 0;
                    double current_document_contribution2 = 0;
                        
                    //loop over topics
                    for(int t = 0; t < number_of_topics; ++t){
                        int topic = t + 1; 
                        //now we calculate the first and second terms in the likelihood of of the token being from the current topic
                        //calculate the number of times a token in the current document has been assigned to the current topic
                        int ntd = 0;
                        for(int b = 0; b < number_of_tokens; ++b){
                                if(token_topic_assignments1[b] == topic){
                                    ntd +=1;
                                }
                        }
                        //int ntt = as<int>(get_number_of_tokens_assigned_to_topic(document,token,topic));
                        double first_term = ntd + exp(proposed_alpha_m[t]);
                        current_document_contribution += lgamma(first_term);
                        double first_term2 = exp(proposed_alpha_m[t]);
                        current_document_contribution2 -= lgamma(first_term2);
                    }//loop over topics 
                    
                    double second_term = number_of_tokens + exp(number_of_topics*proposed_alpha_m[0]);
                    current_document_contribution -= lgamma(second_term);
                    
                    double second_term2 = exp(number_of_topics*proposed_alpha_m[0]);
                    current_document_contribution2 += second_term2;
                    Proposed_Alpha_Corpus_Likelihood += current_document_contribution + current_document_contribution2;
        
                }//end of document loop

                //add on because we are working in log space
                Proposed_Alpha_Corpus_Likelihood += (number_of_topics*proposed_alpha_m[1]);
                //report(Proposed_Alpha_Corpus_Likelihood);
                
                // ========== check to see if it is under the curve ======== // 
                if(Proposed_Alpha_Corpus_Likelihood > slice_probability_floor){
                    in_slice = 1;
                }
                else{
                    //if the positions we tried were outside of the slice, set them as the new boundary
                    //get the left and right bounds on the slice for alpha 
                    for(int t = 0; t < number_of_topics; ++t){
                            if(proposed_alpha_m[t] < log_alpha_m[t]){
                                left_proposed_alpha_m[t] = proposed_alpha_m[t];
                            }
                            else{
                                right_proposed_alpha_m[t] = proposed_alpha_m[t];
                            }
                    }
                }   
            }// end of while checking to see if we are in slice loop
            
            //now update if the control paramter is one update, otherwise just keep equal to original 
            if(slice_sample_alpha == 1){
                for(int t = 0; t < number_of_topics; ++t){
                    alpha_m[t] = exp(proposed_alpha_m[t]); 
                }
            }
            
            //report(exp(proposed_alpha_m[1]));
        }//end of slice sample every 5 conditional statement
        
        // ===================================================================== //
        // ===================== Adaptive Metropolis Step ====================== //
        // ===================================================================== //
        if(n > 0){ // do not update on the first round 
            for(int k = 0; k < number_of_clusters; ++k){
                double num_accepted = 0;
                double iters = (double)number_of_MH_itterations;
                for(int i = 0; i < number_of_MH_itterations; ++i){
                    num_accepted += MH_acceptances(i,k); 
                }
                double accept_proportion = num_accepted/iters;

                cur_accept_rates(n,k) = accept_proportion;
                double temp = proposal_variance[k];
                
                if(accept_proportion > metropolis_target_accpet_rate + 0.05){
                    proposal_variance[k] =  temp + 0.05;
                }
                if((accept_proportion < metropolis_target_accpet_rate - 0.05) &(proposal_variance[k] > 0.09 )){
                    proposal_variance[k] = temp - 0.05;
                }
                
                //if we did not accept anything then make a jump.
                if(accept_proportion == 0){
                    proposal_variance[k] = 0.5;
                }
            }
            for(int k = 0; k < number_of_clusters; ++k){
                cur_proposal_variances(n,k) = proposal_variance[k];
            }
            
        }
        Rcpp::Rcout << "Cluster Proposal Variances: " << std::endl << proposal_variance << std::endl;

        // ===================================================== //
        // ===================== LSM Step ====================== //
        // ===================================================== //
        for(int i = 0; i < number_of_MH_itterations; ++i){
            
            double beta_val = 0;
            arma::vec current_author_position = arma::zeros(number_of_latent_dimensions);
            arma::vec proposed_author_position = arma::zeros(number_of_latent_dimensions);
            arma::vec recipient_position = arma::zeros(number_of_latent_dimensions);
            arma::vec proposed_intercepts = arma::zeros(number_of_clusters);
            arma::cube proposed_latent_positions = arma::zeros(number_of_latent_dimensions,number_of_clusters,number_of_actors);
            arma::mat proposed_betas = arma::zeros(number_of_clusters,number_of_betas);
            arma::vec cluster_distances = arma::zeros(number_of_clusters);
            
            for(int k = 0; k < number_of_clusters; ++k){

                //for intercepts   
                mjd::normal_distribution<double> distribution1(current_intercepts[k],proposal_variance[k]);
                proposed_intercepts[k] = distribution1(generator);

                //for latent positions
                for(int a = 0; a < number_of_actors; ++a){
                    for(int l = 0; l < number_of_latent_dimensions; ++l){
                        mjd::normal_distribution<double> distribution2(current_latent_positions(l,k,a),proposal_variance[k]);
                        proposed_latent_positions(l,k,a) = distribution2(generator);
                    }
                }
                //for mixing parameters
                proposed_betas(k,0) = 0;
                for(int b = 1; b < number_of_betas; ++b){
                    mjd::normal_distribution<double> distribution3(betas(k,b),proposal_variance[k]);
                    proposed_betas(k,b) = distribution3(generator);
                }
            }//end of loop over generating new potenttial LS positions
            
            
            //main loop
            for(int k = 0; k < number_of_clusters; ++k){
                double lsm_prior_current_positions = 0;
                double lsm_prior_proposed_positions = 0;
                double standard_deviation = MH_prior_standard_deviation;
                double dist_center = 0;
                double lsm_sum_log_probability_of_current_positions = 0;
                double lsm_sum_log_probability_of_proposed_positions = 0;
                
                double current_cluster_intercept = current_intercepts[k];
                double proposed_cluster_intercept = proposed_intercepts[k];
                lsm_prior_current_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (current_cluster_intercept-dist_center)/standard_deviation, 2.0 ) ));
                lsm_prior_proposed_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (proposed_cluster_intercept-dist_center)/standard_deviation, 2.0 ) ));
             
                arma::rowvec current_cluster_betas = betas.row(k);
                arma::rowvec proposed_cluster_betas = proposed_betas.row(k);
                for(int c = 1; c < number_of_betas; ++c){
                    lsm_prior_current_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (current_cluster_betas[c]-dist_center)/standard_deviation, 2.0 ) ));
                    lsm_prior_proposed_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (proposed_cluster_betas[c]-dist_center)/standard_deviation, 2.0 ) ));
                }

                for(int a = 0; a < number_of_actors; ++a){
                    for(int c = 0; c < number_of_latent_dimensions; ++c){
                      current_author_position[c] = current_latent_positions(c,k,a);
                      proposed_author_position[c] = proposed_latent_positions(c,k,a);
                      lsm_prior_current_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (current_author_position[c]-dist_center)/standard_deviation, 2.0 ) ));
                      lsm_prior_proposed_positions += log(( 1 / ( standard_deviation * sqrt(2*M_PI) ) ) * exp( -0.5 * pow( (proposed_author_position[c]-dist_center)/standard_deviation, 2.0 ) ));
                    }
                    
                    for(int b = 0; b < number_of_actors; ++b){
                        if(b!= a){
                            
                            double num_actual_edge = 0;
                            double num_non_edge = 0;
                            //get number of actual edge present and absent for this cluster sender reciever combo
                            for(int t = 0; t < number_of_topics; ++t){
                                double topic_cluster = topic_cluster_assignments[t] -1;
                                if(topic_cluster == k){
                                    num_actual_edge += topic_present_edge_counts(a,b,t);
                                    num_non_edge += topic_absent_edge_counts(a,b,t); 
                                }
                            }

                            for(int c = 0; c < number_of_latent_dimensions; ++c){
                              recipient_position[c] = current_latent_positions(c,k,b);
                            }
                            
                            //initialize distance
                            double distance = 0;
                            //calculate distance
                            for(int j = 0; j < number_of_latent_dimensions; ++j){
                                distance += pow((current_author_position[j] - recipient_position[j]),2);
                            }

                            beta_val = 0;
                            for(int c = 1; c < number_of_betas; ++c){
                                beta_val += current_cluster_betas[c]*beta_indicator_array(a,b,c);
                            }
        
                            //calculate linear predictor
                            double eta = 0;
                            eta = current_cluster_intercept - pow(distance,.5) + beta_val;

                            //calculate likelihoods for both
                            double log_prob_edge = 0;
                            double log_prob_no_edge = 0;
                            if (eta != 0){
                                if(eta < 0){
                                    log_prob_edge = eta -log(1 + exp(eta));
                                    log_prob_no_edge = 0 -log(1 + exp(eta));
                                }
                                else{
                                    log_prob_edge = 0 -log(1 + exp(-eta));
                                    log_prob_no_edge = 0 -eta -log(1 + exp(-eta));
                                }
                            }
                            
                            //multiply and add to sum
                            lsm_sum_log_probability_of_current_positions += num_actual_edge*log_prob_edge;
                            lsm_sum_log_probability_of_current_positions += num_non_edge*log_prob_no_edge;
                            
                            
                            // ======== Now calculate for new positions ==========//
                            //get current recipient position
                            for(int c = 0; c < number_of_latent_dimensions; ++c){
                              recipient_position[c] = proposed_latent_positions(c,k,b);
                            }  
                            
                            //initialize distance
                            distance = 0;
                            //calculate distance
                            for(int j = 0; j < number_of_latent_dimensions; ++j){
                                distance += pow((proposed_author_position[j] - recipient_position[j]),2);
                            }
                            
                            beta_val = 0;
                            for(int c = 1; c < number_of_betas; ++c){
                                beta_val += proposed_cluster_betas[c]*beta_indicator_array(a,b,c);
                            }
                            
                            //calculate linear predictor
                            eta = proposed_cluster_intercept - pow(distance,.5) + beta_val;
                            
                            log_prob_edge = 0;
                            log_prob_no_edge = 0;
                            if (eta != 0){
                                if(eta < 0){
                                    log_prob_edge = eta -log(1 + exp(eta));
                                    log_prob_no_edge = 0 -log(1 + exp(eta));
                                }
                                else{
                                    log_prob_edge = 0 -log(1 + exp(-eta));
                                    log_prob_no_edge = 0 -eta -log(1 + exp(-eta));
                                }
                            }
                            
                            //multiply and add to sum
                            lsm_sum_log_probability_of_proposed_positions += num_actual_edge*log_prob_edge;
                            lsm_sum_log_probability_of_proposed_positions += num_non_edge*log_prob_no_edge;
                        }
                    }  
                }
                
                
                lsm_sum_log_probability_of_proposed_positions += lsm_prior_proposed_positions;
                lsm_sum_log_probability_of_current_positions += lsm_prior_current_positions;
                //now calculate log ratio between two
                
                Proposed_MH_Likelihoods[k] = lsm_sum_log_probability_of_proposed_positions;
                Current_MH_Likelihoods[k] = lsm_sum_log_probability_of_current_positions;
                double log_ratio =  lsm_sum_log_probability_of_proposed_positions - lsm_sum_log_probability_of_current_positions;
                double rand_num = uniform_distribution(generator);

                double lud = log(rand_num);

                if(log_ratio < lud){
                    MH_acceptances(i,k) = 0 ;
                }
                else{
                    MH_acceptances(i,k) = 1; 
                    //for intercepts
                    cluster_accept[k] = 1;
                    double tempint = proposed_intercepts[k];
                    current_intercepts[k] = tempint;
                    
                    //for latent positions
                    for(int a = 0; a < number_of_actors; ++a){
                        for(int l = 0; l < number_of_latent_dimensions; ++l){
                            double templat = proposed_latent_positions(l,k,a);
                            current_latent_positions(l,k,a) = templat;
                        }
                    }
                    //for mixing parameters
                    for(int b = 0; b < number_of_betas; ++b){
                        double tempbet = proposed_betas(k,b);
                        betas(k,b) = tempbet;
                    }
                }
            }//end of loop over clusters for which we are doing separate proposals and acceptances
            
            //if we are in the last outter iteration save everything
            
            if(n == (number_of_outer_itterations -1)){
                //report("storing");
                arma::rowvec ints(number_of_clusters);
                for(int b = 0; b < number_of_clusters; ++b){
                    ints[b] = current_intercepts[b];
                }
                
                arma::mat bets(number_of_clusters,number_of_betas);
                for(int a = 0; a < number_of_clusters; ++a){
                    for(int b = 0; b < number_of_betas; ++b){
                        bets(a,b) = betas(a,b);
                    }
                }
                    
                arma::cube lat_pos = current_latent_positions;
                
                arma::rowvec cluster_accepted(number_of_clusters);
                arma::rowvec Cur_Proposed_MH_Likelihoods(number_of_clusters);
                arma::rowvec Cur_Current_MH_Likelihoods(number_of_clusters);
                for(int a = 0; a < number_of_clusters; ++a){
                    cluster_accepted[a] = cluster_accept[a];
                    cluster_accept[a] = (double)  0;
                    Cur_Proposed_MH_Likelihoods[a] = Proposed_MH_Likelihoods[a];
                    Proposed_MH_Likelihoods[a] = (double)  0;
                    Cur_Current_MH_Likelihoods[a] = Current_MH_Likelihoods[a];
                    Current_MH_Likelihoods[a] = (double)  0;
                }
                
                for(int b = 0; b < number_of_clusters; ++b){
                    //store intercepts from last round of metropolis hastings
                    store_intercepts(MH_Counter,b) = ints[b];
                    
                    for(int c = 0; c < number_of_betas; ++c){
                        //store betas from last round of metropolis hastings
                        store_betas(b,c,MH_Counter) = bets(b,c);
                    }
                    
                    //store whether or not the new MH proposal was accepted in the last round of MH
                    store_cluster_whether_accepted(MH_Counter,b) = cluster_accepted[b];
                    
                    //store proposed position likelihods in the last round of MH
                    store_cluster_proposed_likelihoods(MH_Counter,b) = Cur_Proposed_MH_Likelihoods[b];
                    
                    //store current position likelihoods in the last round of MH
                    store_cluster_current_likelihoods(MH_Counter,b) = Cur_Current_MH_Likelihoods[b];
                }
                
                //store latent positions from last round of metropolis hastings (will take up multiple slices per iteration depending on the number of latent dimensions)
                int startslice = number_of_latent_dimensions*MH_Counter;
                
                for(int a = 0; a < number_of_latent_dimensions; ++a){
                    for(int b = 0; b < number_of_clusters; ++b){
                        for(int c = 0; c < number_of_actors; ++c){
                            int store_position = startslice + a;
                            store_latent_positions(store_position,b,c) = lat_pos(a,b,c);
                        }
                    }
                }

                MH_Counter += 1;
            }
        }// end of MH loop
    }// end of main outer itteration loop 
    
    //return 
    to_return[0] = token_topic_assignment_list;
    to_return[1] = topic_present_edge_counts;
    to_return[2] = topic_absent_edge_counts;
    to_return[3] = token_type_topic_counts;
    to_return[5] = number_of_documents;
    to_return[6] = number_of_outer_itterations;
    to_return[7] = number_of_Gibbs_itterations;
    to_return[8] = number_of_MH_itterations;
    to_return[9] = number_of_clusters;
    to_return[10] = cur_proposal_variances;
    to_return[11] = cur_accept_rates;
    to_return[12] = Topic_Model_Likelihoods;
    to_return[13] = all_topic_cluster_assignments;
    to_return[14] = store_intercepts;
    to_return[15] = store_betas;
    to_return[16] = store_latent_positions;
    to_return[17] = store_cluster_whether_accepted;
    to_return[18] = store_cluster_proposed_likelihoods;
    to_return[19] = store_cluster_current_likelihoods;

    return to_return;
        
}


            


