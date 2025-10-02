make_square <- function(a, values_fill=0, make_symmetric=FALSE, verbose=TRUE){
  to_add_cols <- rownames(a)[which(!(rownames(a) %in% colnames(a)))]
  to_add_rows <- colnames(a)[which(!(colnames(a) %in% rownames(a)))]
  
  if(verbose){message('adding ', length(to_add_cols), ' columns and ', length(to_add_rows), ' rows.')}
  
  a <- as.matrix(a, dimnames=list(rownames(a), colnames(a)))
  
  if(length(to_add_rows) > 0){
  a <- rbind(a, new=matrix(data=rep(values_fill, ncol(a)*length(to_add_rows)), ncol=ncol(a), nrow=length(to_add_rows), dimnames=list(to_add_rows, colnames(a))))
  }
  
  if(length(to_add_cols) > 0){
  a <- cbind(a, new=matrix(data=rep(values_fill, nrow(a)*length(to_add_cols)), nrow=nrow(a), ncol=length(to_add_cols), dimnames=list(rownames(a), to_add_cols)))
  }
  
  if(make_symmetric){
    new_order <- rownames(a)
    a <- a[new_order,new_order,drop=FALSE]
  }
  
  return(a)
}
