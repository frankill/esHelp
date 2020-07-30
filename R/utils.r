cheak_dots <- function(...) {
	value <- dots_values(...)
	ids <- which(names(value)=="")
	if(!length(ids)){
		return(value)
	}
	value[[ids]] <- NULL
	return(value)
}


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
elastic_q <- function(code__){

	env <- parent.frame()
	exps__ <- enexpr(code__)
	eval_tidy(exps__, data=elastic_dsl, env=env )

}

elastic_s <- function(...){

	env <- parent.frame()
	exps__ <- enexprs(...)

	res <- flatten(lapply(exps__, eval_tidy, data= elastic_dsl, env= env))
	check <- match(c(T,F), res)
	num <- check[!is.na(check)]

	if ( length(num)>1) abort("Can contain only one logical value")
	
	if (length(num) ==1 ) 
		res <- unlist(res)[num]

	list( `_source` =res)

}

elastic_a <- function(...){

	env <- parent.frame()
	exps__ <- enexprs(...)
	res <- lapply(exps__, eval_tidy, data= elastic_dsl, env= env) 
	flatten(res)

}

'%+%' <- function(left, right) {

	if(!is.list(left)) abort("left must user the list type ")
	if(!is.list(left)) abort("right must user the list type ")
	
	c(left, right)
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
elastic_m <- function(code__, prettry= T) { 

	env <- parent.frame()
	exps__ <- enexpr(code__) 
	eval_tidy(exps__, data= elastic_mappings, env= env)

}


### @export
bool_q <- function(...){
	env <- parent.frame()
	exp_ <- enexprs(...)
	exp_ <- expr(query(bool(filter( !!! exp_ ))))
	eval_tidy(exp_, data=elastic_dsl, env=env)
}

#' @export
#' @param ... for details of contents https://www.elastic.co/guide/en/elasticsearch/reference/7.5/indices-templates.html
#' @example
#' alias2 <-  with_query(c(filter(user=='kimchy'), routing='kimchy') , pretty = F)
#' es_template(c('te*') , 
#' 				settings(2), 
#' 				aliases(
#' 					alias1= c(), 
#' 					alias2= alias2, 
#' 					`{index}-alias`= c()))
#' 
#' es_template(c('te*','bar*'), 
#' 			settings(1), 
#' 			mappings(
#' 				meta_field = list(`_source`= list(enabled= F)), 
#' 				keyword(host_name),
#' 				 date(created_at)))


elastic_t <- function( patterns= '*' ,  ... ) {
	
	exp__ <- enexprs(...)

	res <- flatten(lapply(exp__, eval_tidy, data= elastic_mappings))
	list2(index_patterns= patterns, !!! res )

}

