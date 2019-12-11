library(esHelp)
library(purrr)
library(rlang)

field_name <- letters 

time_ <- c("z","b", "d") 
 
format <-  "yyyy-MM-dd HH:mm:ss||yyyy-MM-dd HH:mm:ss.SSSZ||strict_date_optional_time||epoch_millis"

f  <-  function(b){
	if(some(time_, ~ .x == b))   
		  es_mapping_type$date(!!b, format =  format ) 
	else  
		  es_mapping_type$keyword(!!b) 
}
 
with_mapping(eval(expr(  mappings(!!! map(field_name, f)) ) )) 



 