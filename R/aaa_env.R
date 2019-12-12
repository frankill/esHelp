#' In order to avoid naming conflicts with other R packages, 
#' ES related namespaces are established

elastic_dsl <- new_environment()
elastic_mappings <- new_environment()


env_bind(elastic_dsl, '`' = function(...)  list(...))
env_bind(elastic_mappings, '`' = function(...)  list(...))
