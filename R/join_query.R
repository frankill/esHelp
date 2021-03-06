join_query_macro <- function(method_, type_){

    methods_ <- ensym(method_) 
    type_    <- ensym(type_)

    function(type_value, query= list(), ...){

        dots <- cheak_dots(...)

        param_ <- list2( !! type_ :=  as_string(ensym(type_value)) , query= query,  !!!dots )

        if (missing(query))
            param_[['query']] <- NULL

        list2(!! methods_ := param_)

    }

}

mapply(function(x,y){

		env_bind(elastic_dsl, !! ensym(x) := join_query_macro(!! ensym(x),!! ensym(y)))

	}, 
	c('has_parent','has_child','parent_id','nested'), 
	c('parent_type','type','type','path')
)
