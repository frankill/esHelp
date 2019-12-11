
query_fun <- c( 
				query_fun, 
				lapply(full_text_methods, full_text_macro), 
				lapply(full_text_methods2, full_text_macro2),
				lapply(occurs, bool_macro),
				 mapply(join_query_macro, join_query_methods, join_query_params)
			   )

funs_list <- function() cat(args(query_fun), "\n")

# esHelp::with_query(query(bool(filter( a == 5, b %in% 1:4 , between(local_time, 1,3, time_zone= "+01:00"), dd > 4 , ? 'test'))))

with_query <- function(code__, pretty=T){
	exps__ <- enexpr(code__)
	res <- eval_tidy(exps__, query_fun)
	if (pretty)
		toJSON(x = res, pretty = T, auto_unbox = T)
	else 
		res 
}

 