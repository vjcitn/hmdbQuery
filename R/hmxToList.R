.hmxPath = function(prefix="http://www.hmdb.ca/metabolites/",
                   id="HMDB00001", ...) {
  sub("__PRE__", prefix, sub("%%ID%%", id, "__PRE__%%ID%%.xml") )
}

hmxToList = function (prefix = "http://www.hmdb.ca/metabolites/", id = "HMDB00001", 
    ...) 
{
    requireNamespace("XML")
    stopifnot(is.atomic(prefix), length(prefix)==1, is.atomic(id), length(id)==1)
    xmlToList(.hmxPath(prefix = prefix, id = id, ...))
}
