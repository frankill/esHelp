es_print <- function(x,y) {
	cat(x, "  param => (", paste0(formalArgs(y), ','), ")\n")
}


query_fun <- c( 
				query_fun, 
				lapply(full_text_methods, full_text_macro), 
				lapply(full_text_methods2, full_text_macro2),
				lapply(occurs, bool_macro),
				 mapply(join_query_macro, join_query_methods, join_query_params)
			   )
### @export
funs_list <- function() {

	for(i in  seq_along(query_fun)) {
		es_print(names(query_fun)[i], query_fun[i])
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

es_mapping_type <- c( 
					relations= relations, 
					mappings= mappings, 
					temps= temps, 
					dynamic=dynamic ,
					settings= settings,
					lapply(TYPEFUNC, es_type_create_macro)
			    )

### @export
type_list <- function(){
	for(i in  seq_along(es_mapping_type)) {
		es_print(names(es_mapping_type)[i], es_mapping_type[i])
	} 
}
### @export
with_mapping <- function(code__, prettry= T) { 

	exps__ <- enquo(code__) 
	res <- eval_tidy(exps__, es_mapping_type)

	if(prettry)
		toJSON(x= res, pretty = T, auto_unbox = T)
	else 
		res
	
}