env_bind(elastic_dsl, script = function(lang="painless",source="", id="", ...){

	type__ <- NA_character_
	if(!missing(source) && source != ""){
		type__ <- sym('source')
	}

	if(!missing(id) && id != ""){
		type__ <- sym('id')
	}

	if (!is_symbol(type__)){
		abort("source | id there must be one")
	}

	dots <- cheak_dots(...)

	res <- list2(script = list2(lang= lang, !!type__ := eval(type__) ) )

	if (length(dots)) {
		res[["params"]] <- dots
	}

	return(res)

})

env_bind(elastic_dsl, script_fields = function(...){

	dots <- cheak_dots(...)

	if (!length(dots)) {
		abort("Script is required, please use script func")
	}

	list2(script_fields= list2(...) )

})

env_bind(elastic_dsl, script_score = function(...){

	list2(script_fields= c(...) )

})

env_bind(elastic_dsl, wrapper = function(value){

	list2(wrapper= list2(query= value) )

})

env_bind(elastic_dsl, pinned = function(ids= c(), query= list()){

	list2(pinned= list2(ids= ids, organic= query) )

})

other_macro <- function(type__){

	type__ <- ensym(type__)

	function(field, ...){

		list2(!! type__= list2( field = field, ...) )

	}

}

lapply(c('rank_feature','percolate','distance_feature'), other_macro ) 

env_bind(elastic_dsl, more_like_this = function(fields= c(), ... ){

	list2(more_like_this= list2(fields= fields, ...) )

})
