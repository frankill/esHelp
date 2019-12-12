#' Adds or removes index aliases.
#' An index alias is a secondary name used to refer to one or more existing indices.
#' Most Elasticsearch APIs accept an index alias in place of an index name
 
#' Macros that generate alias functions
aliases_macro <- function(action) {

	action__ <- ensym(action)

	function(...){
		list2(!! action__ = list2(...))
	}

} 


#' https://www.elastic.co/guide/en/elasticsearch/reference/7.5/indices-aliases.html
#' Include function  aliases ，add, remove, remove_index 
#' @param ... See for details of contents  https://www.elastic.co/guide/en/elasticsearch/reference/7.5/indices-aliases.html
#' @examples
#' 
#' 

env_bind(elastic_mappings, aliases   = function(...) list2(actions= list2(...)) )

lapply(c('add','remove','remove_index'), function(x){
	env_bind(elastic_mappings, !! ensym(x) := aliases_macro(x) )
}) 