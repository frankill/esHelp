library(esHelp)
library(rlang)

field_name <- letters 

time_ <- c("z","b", "d") 
 
format <-  "yyyy-MM-dd HH:mm:ss||yyyy-MM-dd HH:mm:ss.SSSZ||strict_date_optional_time||epoch_millis"

f  <-  function(b){
	if(purrr::some(time_, ~ .x == b))   
		  elastic_m(date(!!b, format =  format )) 
	else  
		  elastic_m(keyword(!!b)) 
}
 
elastic_m(eval(expr(  mappings(!!! purrr::map(field_name, f)) ) )) 



 