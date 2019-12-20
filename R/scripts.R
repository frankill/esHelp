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

	res <- list2(script = list2(lang= lang, !!type__ := eval(type__), ))

	if (!length(dots)) {
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