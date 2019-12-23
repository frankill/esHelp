bool_macro <- function(occur__){

	occ__ <- ensym(occur__)

	function(...){
		list2( !! occ__ := list2(...))
	}

}
 
env_bind(elastic_dsl, bool = function(...) list(bool = c(...)))


lapply(c("must", "must_not", "filter", "should" ), function(x){
	 env_bind(elastic_dsl, !! ensym(x) := bool_macro(!! ensym(x)) )
}) 
 