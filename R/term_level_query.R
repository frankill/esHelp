rege_macro <- function(expr__) {

	exp <- ensym(expr__)

	function(key__, value__, ...){

		dots <- cheak_dots(...)
 
		res <- list2( !! ensym(key__) :=  list2(value =value__, !!! dots) ) 
		list2( !! exp := res )

	}

}

compare_macro <- function(expr__){
	exp__ <- ensym(expr__)
	function(key__,value__,...) {
		dots <- cheak_dots(...)
		tmp__ <- list2( !! exp__ := value__)
		list( range=  list2( !! ensym(key__) :=  c(tmp__, !!! dots)) )
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
	dots <- cheak_dots(...)
	list(terms= list2( !! ensym(match__) := set__,  !!!dots)) 
}

elastic_dsl[['terms_set']] <- function(key__, value__, ...){
	dots <- cheak_dots(...)
	list(terms_set= list2(!! ensym(key__) := list2(terms= value__, !!!dots)))
}

elastic_dsl[['?']]    <- function(field__) list(exists= list(field = as_string(ensym(field__)))) 

elastic_dsl[['between']]  <- function(key__,start__,end__,...) {
	dots <- cheak_dots(...)
		
	res <- list2( !! ensym(key__) :=  list2( gte = start__, lte= end__, !!!dots)  )
	list(range = res)

} 

env_bind(elastic_dsl, equals = eq_macro(term) )
env_bind(elastic_dsl, middle = eq_macro(terms) )
 
env_bind(elastic_dsl, `>` = compare_macro(gt) )
env_bind(elastic_dsl, `<` = compare_macro(lt) )
env_bind(elastic_dsl, `>=` = compare_macro(gte) )
env_bind(elastic_dsl, `<=` = compare_macro(lte) )
 
env_bind(elastic_dsl, fuzzy = rege_macro(fuzzy) )
env_bind(elastic_dsl, prefix = rege_macro(prefix) )
env_bind(elastic_dsl, regexp = rege_macro(regexp) )
env_bind(elastic_dsl, wildcard = rege_macro(wildcard) )
