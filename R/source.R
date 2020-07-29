env_bind(elastic_dsl, inc = function(fields){ 
    
    list(includes= fields) 
    
})

env_bind(elastic_dsl, uni = function(fields){ 

    list(excludes= fields)
    
 })