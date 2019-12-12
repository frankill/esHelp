bool_macro <- function(occur__){

	occ__ <- ensym(occur__)

	function(...){
		list2( !! occ__ := list2(...))
	}

}
 
env_bind(query_fun, bool = function(...) list(bool = c(...)))
env_bind(query_fun, query = function(...) list(query = c(...)))

lapply(c("must", "must_not", "filter", "should" ), function(x){
	 env_bind(query_fun, !! ensym(x) := bool_macro(x) )
}) 
 