
#' brute_merge
#'
#' merge spdfs by adding unique columns to the dataframe
#'
#'
#' @param so1 is a SpatialPolygonsDataFrame
#' @param so2 is a SpatialPolygonsDataFrame
#' @return a merged spdf
#' @export
brute_merge <- function(so1, so2) {
  n1 <- names(so1)
  n2 <- names(so2)
  dupes <- n2[n2 %in% n1]
  uniq <- n2[!(n2 %in% n1)]
  for (i in seq_along(uniq)) {
    eval(parse(text = paste0('so1$', uniq[i], ' <- 0')))
  }
  names(so1) <- c(n1, uniq)
  nondupes <- n1[!(n1 %in% dupes)]
  for (i in seq_along(nondupes)) {
    eval(parse(text = paste0('so2$', nondupes[i], ' <- 0')))
  }
  names(so2) <- c(n2, nondupes)
  so1 <- so1[ , order(names(so1))]
  so2 <- so2[ , order(names(so2))]
  return(rbind(so1, so2))
}


#' month_no
#'
#' converts month names to numbers
#'
#' @param vec is a character vector of month names
#' @return a vector of number 1-12 representing months
#' @export

month_no <- function(vec) {
  nos <- vector(length(vec), mode = 'numeric')
  for(i in seq_along(vec)) {
    if(!is.na(vec[i])){
      if(vec[i] == 'January') nos[i] <- 1
      if(vec[i] == 'February') nos[i] <- 2
      if(vec[i] == 'March') nos[i] <- 3
      if(vec[i] == 'April') nos[i] <- 4
      if(vec[i] == 'May') nos[i] <- 5
      if(vec[i] == 'June') nos[i] <- 6
      if(vec[i] == 'July') nos[i] <- 7
      if(vec[i] == 'August') nos[i] <- 8
      if(vec[i] == 'September') nos[i] <- 9
      if(vec[i] == 'October') nos[i] <- 10
      if(vec[i] == 'November') nos[i] <- 11
      if(vec[i] == 'December') nos[i] <- 12
    }
  }
  nos
}





















