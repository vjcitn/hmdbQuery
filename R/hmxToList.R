.hmxPath = function(prefix="http://www.hmdb.ca/metabolites/",
                   id="HMDB0000001", ...) {
  sub("__PRE__", prefix, sub("%%ID%%", id, "__PRE__%%ID%%.xml") )
}

hmxToList = function (prefix = "http://www.hmdb.ca/metabolites/", id = "HMDB0000001", 
    ...) 
{
    requireNamespace("XML")
    stopifnot(is.atomic(prefix), length(prefix)==1, is.atomic(id), length(id)==1)
    txt = readLines(.hmxPath(prefix=prefix, id=id, ...))
    prs = xmlTreeParse(txt, asText=TRUE)
    xmlToList(prs)
}
