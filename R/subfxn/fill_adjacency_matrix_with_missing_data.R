#' Fill an adjacency matrix that includes missing data
#'
#' Fill an 'i' by 'j' *undirected* adjacency matrix,
#' the foundation of network graphs, from an 
#' edgelist. 
#' In the input edgelist, there can be actors
#' without alters, and alters not in the survey data.
#' Matrix elements between alters not in the survey 
#' data will be assigned "NA" instead of 0.
#' IMPORTANT: THE WEIGHTED VERSION OF THIS RUNS, BUT THE WEIGHTS
#' ARE INCORRECT BECAUSE OF THE INPUT DATA FRAME FROM SCRIPT
#' 'SOCIAL_NETWORK_FOR_SEN.R'
#' 
#'
#' @param el an edgelist with at least two columns: actor from, actor to. if weighted, there should be a third column which gives the weight.
#' @param missing_data a vector that contains the names of alters that are not represented in the survey data. 
#' @param self true/false. should the function retain self-loops? If false, any self-loops in the edgelist are filtered out.
#' @param multiple_entries how should the 
#' @return an adjacency matrix
#' @examples
#' 
#' @export

fill_adjacency_matrix_with_missing_data <- function(el, missing_data, self=FALSE, el_directed=FALSE){
  source(here('R','subfxn','make_square_matrix.R'))
  
  ###----------------- OPTION 1: WEIGHTED -----------------###
  if(dim(el)[2] == 3){
    
    ## PART 1: prep to fill adjacency matrix ##
    
    # rename columns to standardize
    colnames(el) <- c('from','to','r')
    
    # vector of egos / alters
    all_a <- unique(c(el$from,el$to))
    all_a <- all_a[which(!is.na(all_a))]
    
    # create a matrix; fill empty elements with zeros
    el2 <- el %>%
      #zeros v. missing data: start by making everyone that is blank NA
      tidyr::pivot_wider(id_cols=from,names_from=to,values_from=r, values_fill=0) %>%
      column_to_rownames('from')
    
    # make the matrix square; fill empty elements with zeros
    el2 <- make_square(a=el2, values_fill=0, make_symmetric=FALSE, verbose=FALSE)
    
    # pivot back to edgelist. replace zeros with NAs as needed.
    el2 %<>% as.data.frame() %>%
      rownames_to_column('from') %>%
      pivot_longer(cols=2:(dim(el2)[1]+1),names_to='to',values_to='r') %>%
      #if either actor in the pair was in our dat_out data frame as an ego, switch 0 to NA
      mutate(r=ifelse(r==0 & (from %in% missing_data & to %in% missing_data),NA,r))
  

    ## PART 2: fill adjacency matrix ##
    
    # create an empty matrix #
    A <- matrix(data=NA, nrow=length(all_a), ncol=length(all_a))
    row.names(A) <- all_a
    colnames(A) <- all_a
    
    # fill A #
    if(el_directed){
    for(i in 1:length(all_a)){
      for(j in 1:length(all_a)){
        if(i!=j | (i==j & self)){
          d <- filter(el2, from==all_a[i] & to==all_a[j] | from==all_a[j] & to==all_a[i])
          r_tmp <- sum(d$r, na.rm=TRUE)
          A[all_a[i],all_a[j]] <- r_tmp
          A[all_a[j],all_a[i]] <- r_tmp
          if(length(d$r[which(d$r!=0)]) > 1){
            message('There were two entries for the actor pair ', all_a[i],':', all_a[j], ', weights ',paste0(d$r,collapse=','),' summed for matrix.')
          } } # end if(i!=j)
          
      } } # for i & j in all_a
      
      
    } else if(!el_directed){
      for(i in 1:length(all_a)){
        for(j in 1:length(all_a)){
          if(i!=j | (i==j & self)){
            d <- filter(el2, from==all_a[i] & to==all_a[j] | from==all_a[j] & to==all_a[i])
            r_tmp <- unique(d$r)
            if(length(r_tmp)==1){
              A[all_a[i],all_a[j]] <- r_tmp
              A[all_a[j],all_a[i]] <- r_tmp
              
            } else{
              if(0 %in% r_tmp){
                r_tmp <- max(r_tmp, na.rm=TRUE)
                A[all_a[i],all_a[j]] <- r_tmp
                A[all_a[j],all_a[i]] <- r_tmp
              } else if(!(0 %in% r_tmp)){
                stop(paste0('ERROR: actor pair ', all_a[i],':', all_a[j], ' is represented by different weights.'))
              }
            } # end else
            
          } # if(i!=j)
        } } # for i & j in all_a
    } # end !directed
    
    return(A)
    
  } else if(dim(el)[2] == 2){
    ###----------------- OPTION 2: UNWEIGHTED -----------------###
    
    ## PART 1: prep to fill adjacency matrix ##
    
    # rename columns to standardize
    colnames(el) <- c('from','to')
    el %<>% mutate(r=1)
    
    # vector of egos / alters
    all_a <- unique(c(el$from,el$to))
    all_a <- all_a[which(!is.na(all_a))]
    
    # create a matrix; fill empty elements with zeros
    el2 <- el %>%
      #zeros v. missing data: start by making everyone that is blank NA
      tidyr::pivot_wider(id_cols=from,names_from=to,values_from=r, values_fill=0) %>%
      column_to_rownames('from')
    
    # make the matrix square; fill empty elements with zeros
    el2 <- make_square(a=el2, values_fill=0, make_symmetric=FALSE, verbose=FALSE)
    
    # pivot back to edgelist. replace zeros with NAs as needed.
    el2 %<>% as.data.frame() %>%
      rownames_to_column('from') %>%
      pivot_longer(cols=2:(dim(el2)[1]+1),names_to='to',values_to='r') %>%
      #if either actor in the pair was in our dat_out data frame as an ego, switch 0 to NA
      mutate(r=ifelse(r==0 & (from %in% missing_data & to %in% missing_data),NA,r))
    
    
    ## PART 2: fill adjacency matrix ##
    
    # create an empty matrix #
    A <- matrix(data=NA, nrow=length(all_a), ncol=length(all_a))
    row.names(A) <- all_a
    colnames(A) <- all_a
    
    # fill A #
    for(i in 1:length(all_a)){
      for(j in 1:length(all_a)){
        if(i!=j | (i==j & self)){
          d <- filter(el2, from==all_a[i] & to==all_a[j] | from==all_a[j] & to==all_a[i])
          r_tmp <- sum(d$r, na.rm=TRUE)
          ## if we at least one of the edgelist entries for this pair is 1:
          if(!is.na(r_tmp) & r_tmp > 0){
            A[all_a[i],all_a[j]] <- 1
            A[all_a[j],all_a[i]] <- 1
            ## otherwise (all entries are 0 or NA): 
          } else{
            A[all_a[i],all_a[j]] <- r_tmp
            A[all_a[j],all_a[i]] <- r_tmp
          }
        } # if(i!=j)
      } } # for i & j in all_a
    
    return(A)
    
    
  } else{stop('ERROR: Please check whether you have the correct combination of edgelist columns / weight argument.')}
  
  
}
