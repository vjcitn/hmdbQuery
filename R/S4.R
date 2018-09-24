#' basic container for an HMDB metabolite entry
#' @import methods
#' @import XML
#' @importFrom S4Vectors DataFrame List
#' @importClassesFrom S4Vectors character_OR_NULL
#' @slot metabolite character(1) institutional name of metabolite
#' @slot id HMDB identifier
#' @slot diseases S4Vectors DataFrame instance listing associated diseases
#' @slot biospecimens S4Vectors DataFrame instance listing associated biospecimens
#' @slot tissues S4Vectors DataFrame instance listing associated tissues
#' @slot store contains parsed XML
#' @note Ontological tagging of diseases and other associated elements should be considered.
#' @exportClass HmdbEntry
setClass("HmdbEntry", representation(metabolite = "character", id = "character", 
    diseases = "DataFrame", biospecimens = "character", tissues = "character_OR_NULL", 
    store = "ANY"))
setMethod("show", "HmdbEntry", function(object) {
    cat("HMDB metabolite metadata for ", object@metabolite, ":\n", sep = "")
    cat("There are ", nrow(object@diseases), " diseases annotated.\n", sep = "")
    cat("Direct association reported for ", length(object@biospecimens), " biospecimens and ", 
        length(object@tissues), " tissues.\n", sep = "")
    cat("Use diseases(), biospecimens(), tissues() for more information.\n")
})
.hmlistDiseaseDF = function(hmlist) {
    dis = hmlist$diseases
    if (is.character(dis) && length(dis) == 1 && dis == "\n  ") 
        return(DataFrame())
    dnames = as.character(vapply(dis, "[[", "character", "name"))
    refl = lapply(dis, function(x) x$references)
    reflc = vapply(refl, class, "character")
    badrefl = which(reflc == "character")  # known failure
    if (length(badrefl) > 0) {
        for (i in badrefl) refl[[i]] = list(reference = list(reference_text = NA_character_, 
            pubmed_id = NA_character_))
    }
    pms = lapply(lapply(refl, lapply, function(x) unname(x$pubmed_id)), unlist)
    DataFrame(metabolite = hmlist$name, disease = dnames, pmids = List(unname(pms)), 
        accession = hmlist$accession)
}
HmdbEntryOLD = function(prefix = "http://www.hmdb.ca/metabolites/", id = "HMDB0000001", 
    keepFull = TRUE) {
    imp = hmxToList(prefix = prefix, id = id)
    tissues = unname(unlist(imp$tissue_locations))
    if (tissues[1] == "\n  ") 
        tissues = NULL
    ans = new("HmdbEntry", metabolite = imp$name, id = id, diseases = .hmlistDiseaseDF(imp), 
        tissues = tissues, biospecimens = unname(unlist(imp$biospecimen_locations)))
    if (keepFull) 
        ans@store = imp
    ans
}

setGeneric("tissues", function(x) standardGeneric("tissues"))
setGeneric("biospecimens", function(x) standardGeneric("biospecimens"))
setGeneric("diseases", function(x) standardGeneric("diseases"))
setGeneric("store", function(x) standardGeneric("store"))

.tissues = function(x) slot(x, "tissues")
.biospecimens = function(x) slot(x, "biospecimens")
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

#' extract biospecimen associations
#' @param x HmdbEntry instance
#' @aliases biospecimens
#' @examples
#' data(hmdb1)
#' biospecimens(hmdb1)
#' @return character vector
#' @export
setMethod("biospecimens", "HmdbEntry", function(x) .biospecimens(x))

#' extract general association metadata in store slot
#' @param x HmdbEntry instance
#' @aliases store
#' @examples
#' data(hmdb1)
#' names(store(hmdb1))
#' @return list
#' @export
setMethod("store", "HmdbEntry", function(x) .store(x))



#' Constructor for HmdbEntry instance
#' @param prefix character(1) URL of HMDB source accepting queries for XML documents
#' @param id character(1) HMDB identifier tag
#' @param keepFull logical(1) indicating that entire parsed XML will be retained
#' @return instance of HmdbEntry, or a list
#' @note The XML returned by hmdb.ca can have different structures
#' for different metabolites.  If the mapping form XML to list
#' is not as anticipated for a given metabolite, the xmlToList
#' result is returned with a warning.  Such entries should be
#' reported to the hmdbQuery maintainer for map revision.
#' @examples
#' HmdbEntry()
#' @export
HmdbEntry = function (prefix = "http://www.hmdb.ca/metabolites/", id = "HMDB0000001", 
    keepFull = TRUE) 
{
    imp = hmxToList(prefix = prefix, id = id)
    nimp = names(imp)
    if ("tissue_locations" %in% nimp) {
      #update the list, old data model in use -- XML doesn't care!
      imp$biological_properties = list()
      imp$biological_properties$tissue_locations = imp$tissue_locations
      imp$biological_properties$biospecimen_locations = imp$biospecimen_locations
      }
    tissues = unname(unlist(imp$biological_properties$tissue_locations))
    if (tissues[1] == "\n  ") 
        tissues = NULL
    ans = try(new("HmdbEntry", metabolite = imp$name, id = id, diseases = .hmlistDiseaseDF(imp), 
        tissues = tissues, biospecimens = unname(unlist(imp$biological_properties$biospecimen_locations))))
    if (inherits(ans, "try-error")) {
      warning("The HMDB XML has an unexpected structure.  Please 
report the HMDB ID to stvjc@channing.harvard.edu.  Returning a list of all 
retrieved data.")
      return(imp)
      }
    if (keepFull) 
        ans@store = imp
    ans
}
