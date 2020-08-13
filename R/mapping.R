typefun <- c("long", "integer", "float", "date", "short", "byte", "double",
			 "half_float","scaled_float","nested", "keyword", "text", 
			 "join", "ip", "point", "linestring", "polygon", "multipoint",
			 "multipolygon", "circle","geo_point","flattened","dense_vector",
			 "date_nanos", "boolean","binary", "alias", "percolator",
			 "integer_range", "float_range", "long_range", "double_range",
			 "date_range", "ip_range", "rank_feature", "rank_features", 
			 "search_as_you_type","sparse_vector","token_count")

relations <- function(code__){
	exp <-  enexpr(code__) 
	list2(relations= list2( !! exp[[2]] := eval(exp[[3]], envir = caller_env())))
}

settings <- function( shards= 1, replicas= 0, interval= '1m',...) {

	dots <- cheak_dots(...)

	res <- list2(
		 		index.number_of_shards= shards, 
		 		index.number_of_replicas= replicas, 
		 		index.refresh_interval= interval,
		 		!!! dots
 			)

 	list2(settings =  res)
}

mappings <- function(
				...,
				meta_field= list(),
				templates= list(),
				date_detection=FALSE, 
				date_formats= c(), 
				numeric_detection= TRUE 
				) {
	res <- list(
			date_detection= date_detection, 
			numeric_detection= numeric_detection, 
			properties= c(...) 
 			)

	if (!missing(meta_field))
		res <- c(meta_field, res )

	if(!missing(templates)) {
		res[['dynamic_templates']] <- templates
	}

	if (!missing(date_formats)){
		res[['dynamic_date_formats']] <- date_formats
	}

 	list(mappings =  res)
}

es_type_create_macro <- function(type__){
 
	function(name__ , ... ){
		dots <- cheak_dots(...)
		name_ <- ensym(name__)
		list2( !! name_  :=  list2( type= type__, !!! dots))

	}

}

env_bind(elastic_mappings, relations  = relations)
env_bind(elastic_mappings, mappings   = mappings)
env_bind(elastic_mappings, temps      = function(...) {
	dots <- cheak_dots(...)
	list(dynamic_templates= dots)
})
env_bind(elastic_mappings, dynamic    = function( name__,  ...) list2( !! ensym(name__) :=  c(...) ))
env_bind(elastic_mappings, settings   = settings)
 
lapply(typefun, function(x){
	env_bind(elastic_mappings, !! ensym(x) := es_type_create_macro(x) )
}) 
