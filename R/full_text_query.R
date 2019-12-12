full_text_methods <- c('match', 'match_bool_prefix','match_phrase', 'match_phrase_prefix','common' )
full_text_methods2 <- c('multi_match','query_string','simple_query_string')
 

full_text_macro <- function(methods){

	methods_ <- ensym(methods)

	function(key__, value__, ...){
		list2( !! methods_ := 
			list2( !! ensym(key__) := 
				list2("query" = value__, ...) ) )
	}

}

full_text_macro2 <- function(methods){

	methods_ <- ensyms(methods)

	function(value__, ...){
		list2( !! methods_ :=  list2("query" = value__, ...) )		  
	}

}

lapply(full_text_methods ,function(x){
	env_bind(query_fun, !! ensym(x) := full_text_macro(!! ensym(x))) 
}) 

lapply(full_text_methods2 ,function(x){
	env_bind(query_fun, !! ensym(x) := full_text_macro2( !! ensym(x))) 
}) 
