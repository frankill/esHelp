TYPEFUNC <- c("long", "integer", "float", "date", "short", "byte", "double",
			 "half_float","scaled_float","nested", "keyword", "text", 
			 "join", "ip", "point", "linestring", "polygon", "multipoint",
			 "multipolygon", "circle","geo_point","flattened","dense_vector",
			 "date_nanos", "boolean","binary", "alias", "percolator",
			 "integer_range", "float_range", "long_range", "double_range",
			 "date_range", "ip_range", "rank_feature", "rank_features", 
			 "search_as_you_type","sparse_vector","token_count")

names(TYPEFUNC) <- TYPEFUNC 

relations <- function(code__){
	exp <-  enexpr(code__) 
	list2(relations= list2( !! exp[[2]] := eval(exp[[3]], envir = caller_env())))
}

dynamic  <- function( name__,  ...){

	 list2( !! ensym(name__) :=  c(...) )

}

temps <- function(...){
	list(dynamic_templates= list(...))
}

settings <- function( 
					number_of_shards= 1,
				 	number_of_replicas= 0,
					refresh_interval= '1m',
					...
					 ) {
	res <- list(
		 		number_of_shards= number_of_shards, 
		 		number_of_replicas= number_of_replicas, 
		 		refresh_interval= refresh_interval,
		 		...
 			)

 	list(settings =  res)
}

mappings <- function(...,
					meta_field= list(),
				 	dynamic_templates= list(),
					date_detection=FALSE, 
					dynamic_date_formats= c(), 
					numeric_detection= TRUE ) {
	res <- list(
		 		date_detection= date_detection, 
		 		numeric_detection= numeric_detection, 
		 		properties= c(...) 
 			)

	if (!missing(meta_field))
		res <- c(res, meta_field)

	if(!missing(dynamic_templates)) {
		res[['dynamic_templates']] <- dynamic_templates
	}

	if (!missing(dynamic_date_formats)){
		res[['dynamic_date_formats']] <- dynamic_date_formats
	}

 	list(mappings =  res)
}

es_type_create_macro <- function(type__=''){
 
	function(name__ , ... ){

		name_ <- ensym(name__)
		list2( !! name_  :=  list2( type= type__, !!! list2(...)))

	}

}


es_mapping_type <- c( relations= relations, mappings= mappings, 
						temps= temps, dynamic=dynamic ,settings= settings,
						lapply(TYPEFUNC, es_type_create_macro))

type_list <- function(){
	cat(args(es_mapping_type),sep='\n')
}

with_mapping <- function(code__, prettry= T) { 

	exps__ <- enquo(code__) 
	res <- eval_tidy(exps__, es_mapping_type)

	if(prettry)
		toJSON(x= res, pretty = T, auto_unbox = T)
	else 
		res
	
}


