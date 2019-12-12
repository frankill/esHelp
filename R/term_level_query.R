rege_macro <- function(expr__) {

	exp <- ensym(expr__)

	function(key__, value__, ...){

		res <- list2( !! ensym(key__) :=  list2(value =value__, !!! list2(...)) ) 
		list2( !! exp := res )

	}

}

compare_macro <- function(expr__){
	exp__ <- ensym(expr__)
	function(key__,value__,...) {
		tmp__ <- list2( !! exp__ := value__)
		list( range=  list2( !! ensym(key__) :=  c(tmp__, ...)) )
	}
} 

eq_macro <- function(expr__){
	exp__ <- ensym(expr__)
	function(key__, value__,  boost= 1.0) {
		list2(!!exp__ := list2( !! ensym(key__) := list2(value = value__, boost= boost))) 
	}
}

elastic_dsl[['ids']]  <- function(value__) list(ids = list(values= value__))

elastic_dsl[['==']]   <- function(key__,value__) {
	list(term= list2( !! ensym(key__) := value__))	
}  
elastic_dsl[['%in%']] <- function(match__,set__,   ...) {
	list(terms= list2( !! ensym(match__) := set__,  !!!list2(...))) 
}

elastic_dsl[['terms_set']] <- function(key__, value__, ...){
	list(terms_set= list2(!! ensym(key__) := list2(terms= value__, !!!list2(...))))
}

elastic_dsl[['?']]    <- function(field__) list(exists= list(field = as_string(ensym(field__)))) 

elastic_dsl[['between']]  <- function(key__,start__,end__,...) {
		
	res <- list2( !! ensym(key__) :=  list2( gte = start__, lte= end__, ...)  )
	list(range = res)

} 

env_bind(elastic_dsl, equals = eq_macro(term) )
env_bind(elastic_dsl, middle = eq_macro(terms) )
 
env_bind(elastic_dsl, `>` = compare_macro(gt) )
env_bind(elastic_dsl, `<` = compare_macro(lt) )
env_bind(elastic_dsl, `>=` = compare_macro(gte) )
env_bind(elastic_dsl, `<=` = compare_macro(lte) )
 
env_bind(query_fun, fuzzy = rege_macro(fuzzy) )
env_bind(query_fun, prefix = rege_macro(prefix) )
env_bind(query_fun, regexp = rege_macro(regexp) )
env_bind(query_fun, wildcard = rege_macro(wildcard) )
