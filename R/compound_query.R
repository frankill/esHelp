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
 
env_bind(elastic_dsl, boosting = function(pos= list(), neg= list() , boost= 1.0) {
	
	if (missing(pos))
		abort("pos is required query object")
	if (missing(neg))
		abort("neg is Required, query object")
	if (missing(boost))
		abort("boost is Required, query object")

	list(boosting= list(
		positive= pos,
		negative= neg, 
		negative_boost= boost
		))
})

env_bind(elastic_dsl, constant_score = function(filter= list(), boost= 1.0) {
	
	if (missing(filter))
		abort("pos is required query object")

	list(constant_score= list(
		filter= filter,
		boost= boost
		))
})

env_bind(elastic_dsl, dis_max = function(..., boost= 1.0) {
 
	list(dis_max= list(
		queries= list2(...),
		boost= boost
		))
})

env_bind(elastic_dsl, function_score = function(
	query= list(), 
	boost=1.0,
	max_boost= 1.0,
	min_score=1.0,
	score_mode= 'max',
	boost_mode='multiply',
	... ) {
 
	list2(function_score= list2(
		query= query,
		boost= boost,
		max_boost= max_boost, 
		min_score= min_score,
		score_mode= score_mode,
		boost_mode= boost_mode,
		...
		))
})

env_bind(elastic_dsl, func = function(...) {
 
	list(functions=  list2(...))
})
