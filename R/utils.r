es_print <- function(x,y) {
	cat(x, "  param => (", paste0(formalArgs(y), ','), ")\n")
}

############# $$$$$$$$$$$$$$$$$$$$##################

elastic_dsl <- .Internal(env2list(elastic_dsl, FALSE, FALSE))

### @export
funs_list <- function() {

	for(i in  seq_along(elastic_dsl)) {
		es_print(names(elastic_dsl)[i], elastic_dsl[[i]])
	} 

}
### @export
with_query <- function(code__, pretty=T){

	exps__ <- enexpr(code__)
	res <- eval_tidy(exps__, elastic_dsl)
	if (pretty)
		toJSON(x = res, pretty = T, auto_unbox = T)
	else 
		res 
}


############# $$$$$$$$$$$$$$$$$$$$##################

elastic_mappings <- .Internal(env2list(elastic_mappings, FALSE, FALSE))

### @export
type_list <- function(){
	for(i in  seq_along(elastic_mappings)) {
		es_print(names(elastic_mappings)[i], elastic_mappings[[i]])
	} 
}
### @export
#@param code__ 
#@param prettry 
with_mapping <- function(code__, prettry= T) { 

	exps__ <- enexpr(code__) 
	res <- eval_tidy(exps__, elastic_mappings)

	if(prettry)
		toJSON(x= res, pretty = T, auto_unbox = T)
	else 
		res
	
}

### @export
bool_query <- function(...){
	exp_ <- enexprs(...)
	exp_ <- expr(query(bool(filter( !!! exp_ ))))
	
	res <- eval_tidy(exp_, elastic_dsl)

	toJSON(x= res, pretty = T, auto_unbox = T)
}

### @export

es_template <- function( index_patterns= '*', 
						settings = list(),
						aliases= list(), 
						mappings= list(), 
						version= 1L, ... ) {
	NULL
}

