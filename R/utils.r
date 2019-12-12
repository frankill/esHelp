es_print <- function(x,y) {
	cat(x, "  param => (", paste0(formalArgs(y), ','), ")\n")
}

############# $$$$$$$$$$$$$$$$$$$$##################

query_fun <- .Internal(env2list(query_fun, FALSE, FALSE))

### @export
funs_list <- function() {

	for(i in  seq_along(query_fun)) {
		es_print(names(query_fun)[i], query_fun[[i]])
	} 

}
### @export
with_query <- function(code__, pretty=T){

	exps__ <- enexpr(code__)
	res <- eval_tidy(exps__, query_fun)
	if (pretty)
		toJSON(x = res, pretty = T, auto_unbox = T)
	else 
		res 
}


############# $$$$$$$$$$$$$$$$$$$$##################

es_mapping_type <- .Internal(env2list(es_mapping_type, FALSE, FALSE))

### @export
type_list <- function(){
	for(i in  seq_along(es_mapping_type)) {
		es_print(names(es_mapping_type)[i], es_mapping_type[[i]])
	} 
}
### @export
with_mapping <- function(code__, prettry= T) { 

	exps__ <- enexpr(code__) 
	res <- eval_tidy(exps__, es_mapping_type)

	if(prettry)
		toJSON(x= res, pretty = T, auto_unbox = T)
	else 
		res
	
}

### @export
bool_query <- function(...){
	exp_ <- enexprs(...)
	exp_ <- expr(query(bool(filter( !!! exp_ ))))
	
	res <- eval_tidy(exp_, query_fun)

	toJSON(x= res, pretty = T, auto_unbox = T)
}