
occurs <- c("must", "must_not", "filter", "should"  )
names(occurs) <- occurs
 
bool_macro <- function(occur__){

	occ__ <- ensym(occur__)

	function(...){
		list2( !! occ__ := list2(...))
	}

}
 
query_fun <- list()

query_fun[['bool']] <- function(...) list(bool = c(...))
query_fun[['query']]<- function(...) list(query= c(...))

