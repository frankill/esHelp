full_text_methods <- c('match', 'match_bool_prefix','match_phrase',
						'match_phrase_prefix','common' )
names(full_text_methods) <- full_text_methods

full_text_methods2 <- c('multi_match','query_string','simple_query_string')
names(full_text_methods2) <- full_text_methods2

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


