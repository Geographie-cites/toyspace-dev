

# pol <- communes
# pol$fingerplan <- sample(x = c(0, 1), size = nrow(pol), replace = TRUE, prob = c(0.8, 0.2))
# id <- "CODGEO"
# cand <- "fingerplan"
# tabflows <- tabFlows
# idori <- "ORI"
# iddes <- "DES"
# idflow <- "FLOW"



# relocate (low level function) ----

relocate_one <- function(pol, id, cand){
  if(st_crs(pol)[[1]] != 2154) stop("Check the CRS (2154) and read the fucking manual")
  pol$ID <- pol[[id]]
  pol$KEY <- pol[[cand]]
  centroPol <- st_centroid(pol)
  oriRelocate <- centroPol
  desRelocate <- centroPol %>% filter(KEY == 1)
  matDist <- st_distance(oriRelocate, desRelocate)
  idMin <- apply(matDist, 1, which.min)
  dictioTransfer <- tibble(OLD = pol$ID, NEW = pol$ID[idMin])
  return(dictioTransfer)
}

# finger plan configuration (high level function) ----

finger_plan <- function(pol, id, cand, tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, id = id, cand = cand)
  tabflows$ORI <- plyr::mapvalues(x = tabflows$ORI, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabflows$DES <- plyr::mapvalues(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  return(tabflows)
}

