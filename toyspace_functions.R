

library(sf)
library(reshape2)
library(tidyverse)

pol <- readRDS("data/pomacom.Rds")
pol$fingerplan <- sample(x = c(0, 1), size = nrow(pol), replace = TRUE, prob = c(0.8, 0.2))
pol$cbd <- ifelse(substr(pol$CODGEO, 1, 2) == "75", 1, 0)
id <- "CODGEO"
cand <- "cbd"
tabflows <- readRDS("data/tabflows.Rds")
idori <- "ORI"
iddes <- "DES"
idflow <- "FLOW"




##### RELOCATE THE STOCKS (matrix margins) #####

# finger plan configuration ----

finger_plan <- function(pol, id, cand, tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, id = id, cand = cand)
  tabflows$ORI <- plyr::mapvalues(x = tabflows$ORI, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabflows$DES <- plyr::mapvalues(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabFlows <- tabflows %>% select(ORI, DES, FLOW)
  return(tabflows)
}

# Polycentrisation ---- 

polycentric_city <- function(pol, id, cand, tabflows, iddes, idflow){
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, id = id, cand = cand)
  tabflows$DES <- plyr::mapvalues(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  return(tabflows)
}

# TOD city ---- 

tod_city <- function(pol, id, cand, tabflows, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, id = id, cand = cand)
  tabflows$ORI <- plyr::mapvalues(x = tabflows$ORI, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  tabflows$DES <- plyr::mapvalues(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  return(tabflows)
}

# CBDsation ---- 

cbd_city <- function(pol, id, cand, tabflows, idori, iddes, idflow){
  tabflows$ORI <- tabflows[[idori]]
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  pol <- pol %>% st_set_geometry(NULL)
  pol$ID <- pol[[id]]
  pol$CAND <- pol[[cand]]
  
  # compute proportion of jobs and proportion of labor force
  totDesIn <- tabflows %>% 
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>% 
    filter(CAND == 1) %>% 
    group_by(DES) %>% 
    summarise(FLOW = sum(FLOW)) %>% 
    mutate(PCTFLOW = FLOW / sum(FLOW)) %>% 
    ungroup()
  
  totOriOut <- tabflows %>% 
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>% 
    filter(CAND != 1) %>% 
    group_by(ORI) %>% 
    summarise(FLOW = sum(FLOW)) %>% 
    mutate(PCTFLOW = FLOW / sum(FLOW)) %>% 
    ungroup()
  
  # re-affect jobs
  tabFlowsSub <- tabflows %>% 
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>% 
    filter(CAND != 1)
  matPctIn <- sapply(tabFlowsSub$FLOW, function(x) x * totDesIn$PCTFLOW) %>% t()
  row.names(matPctIn) <- paste(tabFlowsSub$ORI, tabFlowsSub$MODE, sep = "_")
  colnames(matPctIn) <- totDesIn$DES
  tabFlowsIn <- melt(matPctIn, varnames = c("ORIMODE", "DES"), value.name = "FLOW", as.is = TRUE) %>% 
    mutate(ORI = substr(ORIMODE, 1, 5), MODE = substr(ORIMODE, 7, 8)) %>% 
    group_by(ORI, DES, MODE) %>% 
    summarise(FLOW = sum(FLOW)) %>% 
    ungroup()
  
  tabFlowsCand <- tabflows %>% 
    left_join(pol[, c("ID", "CAND")], by = c("DES" = "ID")) %>% 
    filter(CAND == 1) %>% 
    transmute(ORI = ORI, DES = DES, MODE = substr(MODE, 1, 2), FLOW = FLOW)
  
  jobsRelocated <- rbind(tabFlowsIn, tabFlowsCand)
  
  # re-affect labor force
  tabFlowsCbd <- jobsRelocated %>% 
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>% 
    filter(CAND == 1)
  matPctOut <- sapply(tabFlowsCbd$FLOW, function(x) x * totOriOut$PCTFLOW) %>% t()

  row.names(matPctOut) <- paste(tabFlowsCbd$DES, tabFlowsCbd$MODE, sep = "_")
  colnames(matPctOut) <- totOriOut$ORI
  tabFlowsOut <- melt(matPctOut, varnames = c("DESMODE", "ORI"), value.name = "FLOW", as.is = TRUE) %>% 
    mutate(DES = substr(DESMODE, 1, 5), MODE = substr(DESMODE, 7, 8)) %>% 
    group_by(ORI, DES, MODE) %>% 
    summarise(FLOW = sum(FLOW)) %>% 
    ungroup()

  tabFlowsNocbd <- jobsRelocated %>% 
    left_join(pol[, c("ID", "CAND")], by = c("ORI" = "ID")) %>% 
    filter(CAND != 1) %>% 
    transmute(ORI = ORI, DES = DES, MODE = substr(MODE, 1, 2), FLOW = FLOW)
  
  allRelocated <- rbind(tabFlowsOut, tabFlowsNocbd)

  return(allRelocated)
}



##### REWIRE THE FLOWS (matrix cells) #####

# Excess commuting ----

excess_commuting <- function(matflows, matcost){
  if(nrow(matflows) == ncol(matflows) & nrow(matcost) == ncol(matcost) & nrow(matflows) == nrow(matcost)){
    n = nrow(matflows)
  } else {
    stop("Check the matrix size (square matrices are required)")
  }
  
  lpResult <- transport(a = apply(matflows, 1, sum), b = apply(matflows, 2, sum), costm = matcost) 
  lpResult$from <- factor(x = lpResult$from, levels = 1:nrow(matflows), labels = 1:nrow(matflows))
  lpResult$to <- factor(x = lpResult$to, levels = 1:nrow(matflows), labels = 1:nrow(matflows))
  lpWide <- dcast(data = lpResult, formula = from ~ to, fill = 0, drop = FALSE, value.var = "mass")
  matMin <- as.matrix(lpWide[, -1])
  
  return(matMin)
}


##### COMPUTE AND MAP INDICATORS #####


# dominant flows (Nystuen-Dacey) ----

nystuen_dacey <- function(
  tabflows,   # data.frame with commuting flows, long format (origin, destination, flow)
  poptab,     # table with population, flows summary (total at ori, des, intra) and core id
  idfield,    # CHAR, name of the id field in the population table (poptab)
  targetfield,# CHAR, name of the variable used for weighting
  threspct    # threshold for defining max flow
)
{
  # prepare data
  colnames(tabflows) <- c("ORI", "DES", "FLOW")
  tabflows <- tabflows %>%
    filter(ORI != DES)
  
  poptab <- poptab %>%
    transmute(ORI = poptab[, idfield],
              DES = poptab[, idfield],
              WGT = poptab[, targetfield])
  
  tabFlowsSum <- tabflows %>%
    group_by(ORI) %>% 
    summarise(SUMFLOW = sum(FLOW, na.rm = TRUE))
  
  tabFlowsMax <- tabflows %>%
    group_by(ORI) %>% 
    arrange(desc(FLOW)) %>% 
    slice(1) %>% 
    left_join(y = tabFlowsSum, by = "ORI") %>% 
    mutate(PCTMAX = FLOW / SUMFLOW) %>% 
    filter(PCTMAX > threspct)
  
  tabFlowsAggr <- tabFlowsMax %>%   
    left_join(x = ., y = poptab[, c("ORI", "WGT")], by = "ORI") %>% 
    left_join(x = ., y = poptab[, c("DES", "WGT")], by = "DES") 
  
  colnames(tabFlowsAggr)[6:7] <- c("WGTORI", "WGTDES")
  
  tabFlowsAggr <- tabFlowsAggr %>% filter(WGTORI < WGTDES)
  
  graphFlows <- graph.data.frame(d = tabFlowsAggr[, c("ORI", "DES")], directed = TRUE)
  V(graphFlows)$DEGIN <- degree(graphFlows, mode = "in")
  graphTab <- get.data.frame(x = graphFlows, what = "vertices")
  
  degSorted <- sort(V(graphFlows)$DEGIN, decreasing = TRUE)
  degSecond <- degSorted[2] + 1
  
  tabflows <- tabflows %>% 
    left_join(y = graphTab, by = c("ORI" = "name")) %>% 
    left_join(y = graphTab, by = c("DES" = "name")) %>% 
    mutate(STATUSORI = ifelse(is.na(DEGIN.x) | DEGIN.x == 0, 0, ifelse(DEGIN.x == 1 | DEGIN.x == 2, 1, ifelse(DEGIN.x < degSecond, 2, 3))),
           STATUSDES = ifelse(is.na(DEGIN.y) | DEGIN.y == 0, 0, ifelse(DEGIN.y == 1 | DEGIN.y == 2, 1, ifelse(DEGIN.y < degSecond, 2, 3))),
           STATUS = paste(STATUSORI, STATUSDES, sep = "_")) %>% 
    select(ORI, DES, STATUS)
  
  graphTab <- graphTab %>% 
    mutate(STATUS = ifelse(is.na(DEGIN) | DEGIN == 0, 0, 
                           ifelse(DEGIN == 1 | DEGIN == 2, 1, 
                                  ifelse(DEGIN < degSecond, 2, 3))))
  
  return(list(FLOWS = tabflows, PTS = graphTab))
}



##### LOW LEVEL FUNCTIONS #####


# relocate (used to relocate people and jobs) ----

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





