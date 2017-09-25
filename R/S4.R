#' basic container for an HMDB metabolite entry
#' @import methods
#' @import XML
#' @importFrom S4Vectors DataFrame List
#' @importClassesFrom S4Vectors character_OR_NULL
#' @slot metabolite character(1) institutional name of metabolite
#' @slot id HMDB identifier
#' @slot diseases S4Vectors DataFrame instance listing associated diseases
#' @slot biofluids S4Vectors DataFrame instance listing associated biofluids
#' @slot tissues S4Vectors DataFrame instance listing associated tissues
#' @slot store contains parsed XML
#' @note Ontological tagging of diseases and other associated elements should be considered.
#' @exportClass HmdbEntry
setClass("HmdbEntry", representation(metabolite="character", id="character",
   diseases="DataFrame", biofluids="character",
   tissues="character_OR_NULL", store="ANY"))
setMethod("show", "HmdbEntry", function(object) {
cat(paste("HMDB metabolite metadata for ", object@metabolite, ":\n", sep=""))
cat(paste("There are ", nrow(object@diseases), " diseases annotated.\n", sep=""))
cat(paste("Direct association reported for ", length(object@biofluids), " biofluids and ", length(object@tissues), " tissues.\n",
    sep=""))
cat("Use diseases(), biofluids(), tissues() for more information.\n")
})
.hmlistDiseaseDF = function(hmlist) {
  dis = hmlist$diseases
  if (is.character(dis) && length(dis)==1 && dis == "\n  ") return(DataFrame())
  dnames = as.character(sapply(dis, "[[", "name"))
  refl = lapply(dis, function(x) x$references)
  reflc = sapply(refl,class)
  badrefl = which(reflc == "character") # known failure
  if (length(badrefl)>0) {
     for (i in badrefl) refl[[i]] = list(reference=list(reference_text=NA_character_, pubmed_id=NA_character_))
     }
  pms = lapply( lapply(refl, lapply, function(x) unname(x$pubmed_id)), unlist )
  DataFrame(metabolite=hmlist$name, disease=dnames, pmids=List(unname(pms)), accession=hmlist$accession)
}
#' Constructor for HmdbEntry instance
#' @param prefix character(1) URL of HMDB source accepting queries for XML documents
#' @param id character(1) HMDB identifier tag
#' @param keepFull logical(1) indicating that entire parsed XML will be retained
#' @return instance of HmdbEntry
#' @examples
#' HmdbEntry()
#' @export
HmdbEntry = function(prefix = "http://www.hmdb.ca/metabolites/",
    id = "HMDB00001", keepFull=TRUE) {
  imp = hmxToList(prefix=prefix, id=id)
  tissues=unname(unlist(imp$tissue_locations))
  if (tissues == "\n  ") tissues=NULL
  ans = new("HmdbEntry", metabolite=imp$name, id=id,
        diseases=.hmlistDiseaseDF(imp), tissues=tissues,
        biofluids=unname(unlist(imp$biofluid_locations)))
  if (keepFull) ans@store = imp
  ans
}

setGeneric("tissues", function(x) standardGeneric("tissues"))
setGeneric("biofluids", function(x) standardGeneric("biofluids"))
setGeneric("diseases", function(x) standardGeneric("diseases"))
setGeneric("store", function(x) standardGeneric("store"))

.tissues = function(x) slot(x, "tissues")
.biofluids = function(x) slot(x, "biofluids")
.diseases = function(x) slot(x, "diseases")
.store = function(x) slot(x, "store")

#' extract tissue associations
#' @param x HmdbEntry instance
#' @return character vector
#' @examples
#' data(hmdb1)
#' tissues(hmdb1)
#' @aliases tissues
#' @export
setMethod("tissues", "HmdbEntry", function(x) .tissues(x))

#' extract disease associations
#' @param x HmdbEntry instance
#' @return DataFrame
#' @examples
#' data(hmdb1)
#' diseases(hmdb1)
#' @aliases diseases
#' @export
setMethod("diseases", "HmdbEntry", function(x) .diseases(x))

#' extract biofluid associations
#' @param x HmdbEntry instance
#' @aliases biofluids
#' @examples
#' data(hmdb1)
#' biofluids(hmdb1)
#' @return character vector
#' @export
setMethod("biofluids", "HmdbEntry", function(x) .biofluids(x))

#' extract general association metadata in store slot
#' @param x HmdbEntry instance
#' @aliases store
#' @examples
#' data(hmdb1)
#' names(store(hmdb1))
#' @return list
#' @export
setMethod("store", "HmdbEntry", function(x) .store(x))

