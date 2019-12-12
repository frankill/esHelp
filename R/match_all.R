env_bind(elastic_dsl, match_all = function(boost= 1.0)  list(match_all= list(boost= boost)))
env_bind(elastic_dsl, match_none = function() list())
 