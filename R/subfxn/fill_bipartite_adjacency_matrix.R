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
fill_bipartite_adjacency_matrix <- function(network_data, empty_A = NULL){
  
  # create an empty matrix if needed #
  if(is.null(empty_A)){
  A <- matrix(data=0, nrow=length(unique(unlist(network_data[,1]))), ncol=length(unique(unlist(network_data[,2]))))
  row.names(A) <- unique(unlist(network_data[,1]))
  colnames(A) <- unique(unlist(network_data[,2]))
  } else{A <- empty_A}
  
  # fill A #
  for(i in 1:dim(A)[1]){
    idat <- network_data %>% filter(v1==row.names(A)[i])
    for(j in 1:dim(A)[2]){
        n <- length(unique(network_data %>% 
                             filter(v1 %in% idat$v1) %>%
                             filter(v2==colnames(A)[j]) %>%
                             pull(v2)) )
        A[i,j] <- n
        
    }
  }

  return(A)
}
