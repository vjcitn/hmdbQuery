library(S4Vectors)
library(xml2)
getdis = function(file="hmdb_metabolites.xml") {
 rr = read_xml(file)
 rrl = as_list(rr)
 acc = unlist(sapply(rrl, "[[", "accession"))
 nm = unlist(sapply(rrl, "[[", "name"))
 dl = sapply(rrl, "[[", "diseases")
 diss = sapply(dl, function(x) try(unlist(sapply(x, "[[", "name"))))
 omim = sapply(dl, function(x) try(unlist(sapply(x, "[[", "omim_id"))))
 diss = sapply(diss, function(x) {
    if (inherits(x, "try-error")) x = NA
    x 
    })
 omim = sapply(omim, function(x) {
    if (inherits(x, "try-error")) x = NA
    x 
    })
 ns = unlist(sapply(diss, length))
 ons = unlist(sapply(omim, length))
 accs = rep(acc, ns)
 nms = rep(nm, ns)
 oaccs = rep(acc, ons)
 disframe = DataFrame(accession=accs, name=nms, disease=unlist(diss))
 omimframe = DataFrame(accession=oaccs, omim=unlist(omim))
 plist = function (pp, prop="uniprot_id") {
    #g = sapply(pp, "[[", "gene_name")
    ps = try(unlist(sapply(pp[[2]], "[[", prop)))
    if (inherits(ps, "try-error")) {
          df = DataFrame(accession=pp[[1]], val=NA)
          names(df)[2] = prop
          return(df)
          }
    n = length(ps)
    df = DataFrame(accession = rep(pp[[1]], n), val=ps)
    names(df)[2] = prop
    df
  }
 ppl = lapply(rrl, function(x) list(x$accession[[1]], x$protein))
 aa = lapply(ppl, plist)
 gg = lapply(ppl, plist, prop="gene_name")
 list(disframe=disframe, omimframe=omimframe,
       prots=do.call(rbind, aa), genes=do.call(rbind, gg))
#
}
mm = getdis()
bar_disease = mm[[1]]
save(bar_disease, file="bar_disease.rda")
bar_omim = mm[[2]]
save(bar_omim, file="bar_omim.rda")
bar_protein = mm[[3]]
save(bar_protein, file="bar_protein.rda")
bar_gene = mm[[4]]
save(bar_gene, file="bar_gene.rda")
