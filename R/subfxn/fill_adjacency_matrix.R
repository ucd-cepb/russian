#' Fill an adjacency matrix
#'
#' Fill an 'i' by 'j' adjacency matrix, the 
#' foundation of network graphs. 
#'
#' @param network_data a data frame with the columns: id, v.  First columns should contain 
#' @param empty_A an empty matrix to fill from the network_data data frame. default is NULL, the function builds the empty matrix for you
#' @return a adjacency matrix
#' @examples
#' 
#' @export
fill_adjacency_matrix <- function(network_data, empty_A = NULL, self=TRUE, weight=TRUE){
  
  # create an empty matrix if needed #
  if(is.null(empty_A)){
  A <- matrix(data=0, nrow=length(unique(unlist(network_data[,2]))), ncol=length(unique(unlist(network_data[,2]))))
  row.names(A) <- unique(unlist(network_data[,2]))
  colnames(A) <- unique(unlist(network_data[,2]))
  } else{A <- empty_A}
  
  # fill A #
  for(i in 1:dim(A)[1]){
    idat <- network_data %>% filter(v==row.names(A)[i])
    for(j in 1:dim(A)[2]){
      if(i==j){
        if(self){
          n <- length(unique(network_data %>% 
                               filter(id %in% idat$id) %>%
                               pull(id)) )
          if(weight==FALSE){if(n > 0){n <- 1}}
          A[i,j] <- n
        }
      }
      if(i!=j){
        n <- length(unique(network_data %>% 
                             filter(id %in% idat$id) %>%
                             filter(v==colnames(A)[j]) %>%
                             pull(id)) )
        if(weight==FALSE){if(n > 0){n <- 1}}
        A[i,j] <- n
        
        
      } # end if(i!=j)
    }
  }

  return(A)
}
