// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
#include <boost/random.hpp>
#include <boost/random/uniform_01.hpp>
#include <math.h>
#include <cmath>

using std::pow;
using std::exp;
using std::sqrt;
using std::log;
using namespace Rcpp;

// [[Rcpp::export]]
List MH_Sampler(
    int number_of_MH_itterations,
    int number_of_actors, 
    int number_of_topics,
    int number_of_latent_dimensions,
    arma::vec proposal_variance,
    arma::vec topic_cluster_assignments,
    NumericVector tpec,
    NumericVector taec,
    NumericVector clp,
    NumericVector plp, 
    arma::vec current_intercepts,
    arma::mat betas,
    int number_of_betas,
    NumericVector indicator_array,
    int burnin,
    int number_of_clusters,
    int store_every_x_rounds,
    int adaptive_metropolis_update_every,
    int use_adaptive_metropolis,
    double MH_prior_standard_deviation,
    int seed
    ){
    
    //count up to a storage round
    int storage_counter = 0;

    //one less than the number of unique objects I want to store at the beginning of the list
    int list_length = 7;
    List to_return(list_length);
    
    int MH_Counter = 0;
    
    arma::mat store_intercepts = arma::zeros(number_of_MH_itterations/store_every_x_rounds,number_of_clusters);
    arma::cube store_betas = arma::zeros(number_of_clusters,number_of_betas,(number_of_MH_itterations/store_every_x_rounds));
    arma::cube store_latent_positions = arma::zeros(number_of_latent_dimensions*(number_of_MH_itterations/store_every_x_rounds),number_of_clusters,number_of_actors);
    arma::mat store_cluster_whether_accepted = arma::zeros(number_of_MH_itterations/store_every_x_rounds,number_of_clusters);
    arma::mat store_cluster_proposed_likelihoods = arma::zeros(number_of_MH_itterations/store_every_x_rounds,number_of_clusters);
    arma::mat store_cluster_current_likelihoods = arma::zeros(number_of_MH_itterations/store_every_x_rounds,number_of_clusters);
    arma::vec cluster_accept= arma::zeros(number_of_clusters);
    arma::vec Proposed_MH_Likelihoods= arma::zeros(number_of_clusters);
    arma::vec Current_MH_Likelihoods= arma::zeros(number_of_clusters);
    //set up varaibles for adaptive metropolis
    int number_to_store = ceil(double(burnin)/double(adaptive_metropolis_update_every)) + 1;
    int adaptive_metropolis_update_counter = 0;
    arma::mat MH_acceptances = arma::zeros(adaptive_metropolis_update_every,number_of_clusters);
    arma::mat cur_accept_rates = arma::zeros(number_to_store,number_of_clusters);
    int reached_burnin = 0;
    
    double metropolis_target_accpet_rate = 0.20;
    
    // Set RNG and define uniform distribution 
    //boost::mt19937_64 generator(seed);
    boost::mt19937 generator(seed);
    boost::uniform_01<double> uniform_distribution;
    //read in topic present edge counts array [num actors x num actors x topics]
    IntegerVector arrayDims1 = tpec.attr("dim");
    arma::cube topic_present_edge_counts(tpec.begin(), arrayDims1[0], arrayDims1[1], arrayDims1[2], false);
    
    //read in topic absent edge counts array [num actors x num actors x topics]
    IntegerVector arrayDims2 = taec.attr("dim");
    arma::cube topic_absent_edge_counts(taec.begin(), arrayDims2[0], arrayDims2[1], arrayDims2[2], false);
    
    //read in latent positions array [num dimensions x clusters x actors]
    IntegerVector arrayDims3 = clp.attr("dim");
    arma::cube current_latent_positions(clp.begin(), arrayDims3[0], arrayDims3[1], arrayDims3[2], false);
    
    //read in beta indicator array (0,1) [number of topics x number of actors x number of betas]
    IntegerVector arrayDims5 = indicator_array.attr("dim"); 
    arma::cube beta_indicator_array(indicator_array.begin(), arrayDims5[0], arrayDims5[1], arrayDims5[2], false);

    arma::vec current_author_position(number_of_latent_dimensions);
    arma::vec proposed_author_position(number_of_latent_dimensions);
    arma::vec recipient_position(number_of_latent_dimensions);

    IntegerVector arrayDims4 = plp.attr("dim");
    arma::cube proposed_latent_positions(plp.begin(), arrayDims4[0], arrayDims4[1], arrayDims4[2], false);
    
    arma::mat proposed_betas(number_of_clusters,number_of_betas);    
    arma::vec current_cluster_betas(number_of_betas);
    arma::vec proposed_cluster_betas(number_of_betas);

    for(int i = 0; i < number_of_MH_itterations; ++i){
            
        //Adaptive metropolis step
        if(use_adaptive_metropolis == 1){
            
            if((adaptive_metropolis_update_counter == (adaptive_metropolis_update_every -1)) & (reached_burnin == 0)){  
                
                //reset counter 
                adaptive_metropolis_update_counter =0;
                
                for(int k = 0; k < number_of_clusters; ++k){
                    
                    double num_accepted = 0;
                
                    for(int n = 0; n < adaptive_metropolis_update_every; ++n){
                        num_accepted += MH_acceptances(n,k); 
                        MH_acceptances(n,k) = 0;
                    }
                    
                    double accept_proportion = double(num_accepted)/double(adaptive_metropolis_update_every);
                    //update record of accept rates
                    cur_accept_rates[adaptive_metropolis_update_counter] = accept_proportion;
                    double temp = proposal_variance[k];
                    //ifthe accept proportion is zero then we should not do anything
                    
                    if(accept_proportion > metropolis_target_accpet_rate + 0.05){
                        proposal_variance[k] =  temp + 0.05;
                    }
                    if((accept_proportion < metropolis_target_accpet_rate - 0.05) &(proposal_variance[k] > 0.09 )){
                        proposal_variance[k] = temp - 0.05;
                    }
                    
                }
                Rcpp::Rcout << "Cluster Proposal Variacnes: " << std::endl << proposal_variance << std::endl;
            }// end of if statement to see if we are actually in an update iteration
            adaptive_metropolis_update_counter += 1;
        }//end of if statement for using adaptive metropolis
        
        
        if(i == burnin ){
            //set variable that tells us we have reached burnin
            reached_burnin =1;
        }
        
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
            boost::normal_distribution<double> distribution1(current_intercepts[k],proposal_variance[k]);
            proposed_intercepts[k] = distribution1(generator);

            //for latent positions
            for(int a = 0; a < number_of_actors; ++a){
                for(int l = 0; l < number_of_latent_dimensions; ++l){
                    boost::normal_distribution<double> distribution2(current_latent_positions(l,k,a),proposal_variance[k]);
                    proposed_latent_positions(l,k,a) = distribution2(generator);
                }
            }
            //for mixing parameters
            proposed_betas(k,0) = 0;
            for(int b = 1; b < number_of_betas; ++b){
                boost::normal_distribution<double> distribution3(betas(k,b),proposal_variance[k]);
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
                
                current_author_position[0] = current_latent_positions(0,k,a);
                current_author_position[1] = current_latent_positions(1,k,a);
                proposed_author_position[0] = proposed_latent_positions(0,k,a);
                proposed_author_position[1] = proposed_latent_positions(1,k,a);
                
                for(int c = 0; c < number_of_latent_dimensions; ++c){
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

                        recipient_position[0] = current_latent_positions(0,k,b);
                        recipient_position[1] = current_latent_positions(1,k,b);
                        
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
                        recipient_position[0] = proposed_latent_positions(0,k,b);
                        recipient_position[1] = proposed_latent_positions(1,k,b);
                        
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
            double accepted = 0;
            double log_ratio =  lsm_sum_log_probability_of_proposed_positions - lsm_sum_log_probability_of_current_positions;
            double rand_num = uniform_distribution(generator);
            double lud = log(rand_num);
            
            if(log_ratio < lud){
                accepted = 0;
                cluster_accept[k] = 0;
            }else{
                accepted = 1;
                cluster_accept[k] = 1;
                double tempint = proposed_intercepts[k];
                current_intercepts[k] = tempint;
                for(int a = 0; a < number_of_actors; ++a){
                    for(int l = 0; l < number_of_latent_dimensions; ++l){
                        current_latent_positions(l,k,a) = proposed_latent_positions(l,k,a);
                    }
                }
                for(int b = 0; b < number_of_betas; ++b){
                    betas(k,b) = proposed_betas(k,b);
                }
            }//end of update 
            if((use_adaptive_metropolis == 1) & (reached_burnin == 0)){
                MH_acceptances((adaptive_metropolis_update_counter -1),k) = accepted;
            }
        }//end of clusters loop 
            
            storage_counter +=1;
            if(store_every_x_rounds == storage_counter){
                storage_counter =0;
                
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
            }//end of save 
            
    }// end of MH loop

    to_return[0] = number_of_MH_itterations/store_every_x_rounds;
    to_return[1] = store_cluster_proposed_likelihoods;
    to_return[2] = store_cluster_current_likelihoods;
    to_return[3] = store_cluster_whether_accepted;
    to_return[4] = store_intercepts;
    to_return[5] = store_latent_positions;
    to_return[6] = store_betas;

 return to_return;
}
