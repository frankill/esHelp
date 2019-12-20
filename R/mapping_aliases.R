#' Adds or removes index aliases.
#' An index alias is a secondary name used to refer to one or more existing indices.
#' Most Elasticsearch APIs accept an index alias in place of an index name
 
#' Macros that generate alias functions
aliases_macro <- function(action) {

	action__ <- ensym(action)

	function(...){
		dots <- cheak_dots(...)
		list2(!! action__ := dots)
	}

} 


#' https://www.elastic.co/guide/en/elasticsearch/reference/7.5/indices-aliases.html
#' Include function  aliases ，add, remove, remove_index 
#' @param ... See for details of contents  https://www.elastic.co/guide/en/elasticsearch/reference/7.5/indices-aliases.html
#' @examples 
#' Note that with_mapping must be used
#' with_mapping(aliases(add(index= 'twitter')))
#' with_mapping(aliases(add(index= 'twitter', alias= 'alias111')))
#' with_mapping(aliases(remove(index= 'twitter', alias= 'alias111')))
#' with_mapping(aliases(remove(index= 'twitter', alias= 'alias111'), add(index= 'twitter', alias= 'alias222')))
#' with_mapping(aliases(add(indices=  c('test1','test2'), alias= 'alias111')))
#' with_mapping(aliases(add(indices=  c('test1','test2'), alias= 'alias111'), remove_index(index = 'test')))
#' with_mapping(aliases(add(indices=  c('test1','test2'), alias= 'alias111', filter = with_query(user == 'kimchy', pretty = F))))
#' with_mapping(aliases(add(index= 'twitter', alias= 'alias111', routing =1 )))
#' with_mapping(aliases(add(index= 'twitter', alias= 'alias111', routing =1 ), add(index= 'test1', alias= "tt1", is_write_index= T)))

env_bind(elastic_mappings, aliases   = function(...) {
	dots <- cheak_dots(...)
	if (length(dots)){
		abort("At least one named parameter needs to be provided")
	}
	list2(actions= dots )
} )

lapply(c('add','remove','remove_index'), function(x){
	env_bind(elastic_mappings, !! ensym(x) := aliases_macro(!! ensym(x)) )
}) 