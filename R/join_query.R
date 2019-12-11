join_query_methods <- c('has_parent','has_child','parent_id','nested')
names(join_query_methods) <- join_query_methods
join_query_params <- c('parent_type','type','type','path')

join_query_macro <- function(method_, type_){

    methods_ <- ensym(method_) 
    type_    <- ensym(type_)

    function(type_value, query= list(), ...){

        param_ <- list2( !! type_ :=  as_string(ensym(type_value)) , query= query,  ... )

        if (missing(query))
            param_[['query']] <- NULL

        list2(!! methods_ := param_)

    }

}