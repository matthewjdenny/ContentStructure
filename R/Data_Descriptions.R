#' Department Manager Email dataset -- Author Attributes
#'
#' A dataset containing 121 emails sent between department managers in a North 
#' Carolina County Government. This dataset is very small and not terrbily interesting 
#' and is meant primarily to serve as an illustration of what the model can do and the 
#' output it will create. The data objects included represent the minimal set necessary
#' to make themodel work and should serve as a template for formatting your own data 
#' and are as follows:
#'   
#' \itemize{
#'   \item Department -- The name of the department the individual is a manager of.
#'   \item Gender -- The gender of the department manager.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name author_attributes
#' @usage data(author_attributes)
#' @format A dataframe with 20 rows and 2 columns.
NULL

#' Department Manager Email dataset -- Document Edge Matrix
#'
#' A dataset containing 121 emails sent between department managers in a North 
#' Carolina County Government. This dataset is very small and not terrbily interesting 
#' and is meant primarily to serve as an illustration of what the model can do and the 
#' output it will create. The data objects included represent the minimal set necessary
#' to make themodel work and should serve as a template for formatting your own data 
#' and are as follows:
#'   
#' \itemize{
#'   \item The first column records the index (starting from 1) of the sender of the email.
#'   \item The remaining 20 columns  (1 for each manager in the same order they appear in the author_attributes dataset) are equal to 1 if the manager recieved (or was Cc'd on) the email and zero otherwise.
#'   \item There is one row for each email.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name document_edge_matrix
#' @usage data(document_edge_matrix)
#' @format A a number of emails (121) by number of authors plus one (21), dataframe where the first column records the index (starting from 1) of the sender of the email and the rest of the columns record a 1 if the manager recieved the email and zero otherwise.
NULL

#' Department Manager Email dataset -- Document Word Matrix
#'
#' A dataset containing 121 emails sent between department managers in a North 
#' Carolina County Government. This dataset is very small and not terrbily interesting 
#' and is meant primarily to serve as an illustration of what the model can do and the 
#' output it will create. The data objects included represent the minimal set necessary
#' to make themodel work and should serve as a template for formatting your own data 
#' and are as follows:
#'   
#' \itemize{ 
#'   \item document_word_matrix -- a number of emails (121) by number of unique terms (204) matrix the records the number of times each term was used in each email. Note that stopwords 
#'   have been removed as have names.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name document_word_matrix
#' @usage data(document_word_matrix)
#' @format A a number of emails (121) by number of unique terms (204) dataframe that records the number of times each term was used in each email.
NULL

#' Department Manager Email dataset -- Vocabulary
#'
#' A dataset containing 121 emails sent between department managers in a North 
#' Carolina County Government. This dataset is very small and not terrbily interesting 
#' and is meant primarily to serve as an illustration of what the model can do and the 
#' output it will create. The data objects included represent the minimal set necessary
#' to make themodel work and should serve as a template for formatting your own data 
#' and are as follows:
#'   
#' \itemize{
#'   \item a vector containing all 204 unique terms. Note that stopwords 
#'   have been removed as have names.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name vocabulary
#' @usage data(vocabulary)
#' @format A dataframe with 204 rows and one word per row.
NULL