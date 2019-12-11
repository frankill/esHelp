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
		list2(!!exp__ = list2( !! ensym(key__) := list2(value = value__, boost= boost))) 
	}
}

query_fun[['ids']]  <- function(value__) list(ids = list(values= value__))

query_fun[['==']]   <- function(key__,value__) {
	list(term= list2( !! ensym(key__) := value__))	
}  
query_fun[['%in%']] <- function(match__,set__,   ...) {
	list(terms= list2( !! ensym(match__) := set__,  !!!list2(...))) 
}

query_fun[[['terms_set']]] <- function(key__, value__, ...){
	list(terms_set= list2(!! ensym(key__) := list2(terms= value__, !!!list2(...))))
}

query_fun[['?']]    <- function(field__) list(exists= list(field = as_string(ensym(field__)))) 

query_fun[['between']]  <- function(key__,start__,end__,...) {
		
	res <- list2( !! ensym(key__) :=  list2( gte = start__, lte= end__, ...)  )
	list(range = res)

} 


query_fun[['equals']]   <-  eq_macro(term)
query_fun[['middle']] <-  eq_macro(terms)

query_fun[['>']]    <- compare_macro(gt)
query_fun[['<']]    <- compare_macro(lt)
query_fun[['>=']]   <- compare_macro(gte) 
query_fun[['<=']]   <- compare_macro(lte)

query_fun[['fuzzy']]  <-  rege_macro('fuzzy')
query_fun[['prefix']] <-  rege_macro('prefix')
query_fun[['regexp']] <- rege_macro('regexp')
query_fun[['wildcard']] <- rege_macro('wildcard')


