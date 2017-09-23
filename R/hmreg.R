# this will not be exported in first release as a full
# registry was made only in June of 2016 and data base has
# since tripled in size
##
##makeHmreg = function() {
## reg = get(load(system.file("hmreg/hmmeta-files/registry.RData", package="metabExplore")))
## reg$file.dir = system.file("hmreg/hmmeta-files", package="metabExplore")
## reg
##}
##
##hmdbByDisease = function( disease = "Asthma", reg = makeHmreg(), index =
##   get(load(system.file("data/hmregIndex.rda", package="metabExplore"))),
##   hitsOnly=TRUE, ...) {
### implement a funnel on the registry to isolate metabolites annotated to a disease
##  dinds = grep(disease, index$disease, ignore.case=TRUE)
##  if (length(dinds)==0) stop(paste("no entries found for", disease))
##  jobs = unique(index[dinds, "job"])
##  pulls = lapply(jobs, function(x) { 
##       ans = loadResult(reg, x)
##       attr(ans, "job") = x
##       ans
##       })
### these are the jobs that have some reference to asthma
##  nonNullAndMatchDisease = function(z) {  # disease is bound
##    if (length(z@diseases$disease)==0) return(FALSE)
##    if (sum(z@diseases$disease==disease)==0) return(FALSE)
##    return(TRUE)
##    }
##  relevant = lapply(pulls, function(x) sapply(x, nonNullAndMatchDisease)) # relevant jobs
##  for (i in 1:length(pulls)) {
##     thej = attr(pulls[[i]], "job")
##     pulls[[i]] = pulls[[i]][relevant[[i]]]
##     attr(pulls[[i]], "job") = thej
##     }
##  pint = lapply(pulls, function(x) {
##       ji = attr(x, "job")
##       cbind( do.call(rbind,  lapply(x, function(z) z@diseases)), DataFrame(job=ji))
##       })
##  ans = do.call( rbind, pint )
##  if (hitsOnly) ans = ans[ ans$disease==disease, ]
##  ans
##  }
##
##jobkeys = function(reg, id) {
## stopifnot(length(id)==1 && is.atomic(id))
## ents = loadResult(reg, id)
## ans = sapply(ents, function(x) c(id=x@id, metabolite=x@metabolite,
##           job=id))
## DataFrame(t(ans))
##}
