bool_macro <- function(occur__){

	occ__ <- ensym(occur__)

	function(...){
		dots <- cheak_dots(...)
		if (length(dots)){
			abort("At least one named parameter needs to be provided")
		}
		list2( !! occ__ := dots)
	}

}
 
env_bind(elastic_dsl, bool = function(...) list(bool = c(...)))
env_bind(elastic_dsl, query = function(...) list(query = c(...)))
env_bind(elastic_dsl, dsl = function(...) c(...))

lapply(c("must", "must_not", "filter", "should" ), function(x){
	 env_bind(elastic_dsl, !! ensym(x) := bool_macro(!! ensym(x)) )
}) 
 