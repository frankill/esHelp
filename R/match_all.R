env_bind(query_fun, match_all = function(boost= 1.0)  list(match_all= list(boost= boost)))
env_bind(query_fun, match_none = function() list())
 