query_fun[['match_all']] <- function(boost= 1.0)  list(match_all= list(boost= boost)) 

query_fun[['match_none']] <- function() list()
