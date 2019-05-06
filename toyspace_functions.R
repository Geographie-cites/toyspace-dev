
##### RELOCATE THE STOCKS (matrix margins) #####

# finger plan configuration ----

#' DF transform to finger plan urban model
#'
#' this function allows you to change the origins and destination of flows 
#' stored into a dataframe to simulate a finger-plan urban model. Cities containing 
#' railroad station are designed as candidate and flows are moved from non-candidate 
#' cities to the nearest candidate cities (using osm network)
#'
#' @param pol An sf object of the cities
#' @param id A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' 
#' @return A data.frame of flows as tabflows with origins and destinations changed
#' 
#' @examples 
#' # Import data
#' 
#' id <- "CODGEO"
#' cand <- "fingerplan"
#' data(pol)
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' 
#' fg_flows <- finger_plan (
#' pol = pol, 
#' id = id, 
#' cand = cand, 
#' tabflows = tabFlows,
#' idori = idori,
#' iddes = iddes,
#' idflow = idflow)
#' 
#' fg_flows[1:10,]
#'
#' @export
#'


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

#' DF transform to polycentric urban model
#'
#' this function allows you to change the origins and destination of flows 
#' stored into a dataframe to simulate a polycentric urban model. Cities considered as employment 
#' pole and containing railroad station are designed as candidate and flows are moved 
#' from non-candidate cities to the nearest candidate cities (using osm network)
#'
#' @param pol An sf object of the cities
#' @param id A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' 
#' @return A data.frame of flows as tabflows with origins and destinations changed
#' 
#' @examples 
#' # Import data
#' 
#' id <- "CODGEO"
#' cand <- "polycentric"
#' data(pol)
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' 
#' poly_flows <- polycentric_city (
#' pol = pol, 
#' id = id, 
#' cand = cand, 
#' tabflows = tabFlows,
#' idori = idori,
#' iddes = iddes,
#' idflow = idflow)
#' 
#' poly_flows[1:10,]
#'
#' @export
#'

polycentric_city <- function(pol, id, cand, tabflows, iddes, idflow){
  tabflows$DES <- tabflows[[iddes]]
  tabflows$FLOW <- tabflows[[idflow]]
  dictionary <- relocate_one(pol = pol, id = id, cand = cand)
  tabflows$DES <- plyr::mapvalues(x = tabflows$DES, from = dictionary$OLD, to = dictionary$NEW, warn_missing = FALSE)
  return(tabflows)
}

# TOD city ---- 

#' DF transform to TOD urban model
#'
#' this function allows you to change the origins and destination of flows 
#' stored into a dataframe to simulate a transport oriented developpement urban model. Cities considered as employment 
#' pole and containing railroad station are designed as candidate and flows are moved 
#' from non-candidate cities to the nearest candidate cities (using osm network)
#'
#' @param pol An sf object of the cities
#' @param id A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' 
#' @return A data.frame of flows as tabflows with origins and destinations changed
#' 
#' @examples 
#' # Import data
#' 
#' id <- "CODGEO"
#' cand <- "tod"
#' data(pol)
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' 
#' tod_flows <- tod_city (
#' pol = pol, 
#' id = id, 
#' cand = cand, 
#' tabflows = tabFlows,
#' idori = idori,
#' iddes = iddes,
#' idflow = idflow)
#' 
#' tod_flows[1:10,]
#'
#' @export
#'

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

#' DF transform to CBD urban model
#'
#' this function allows you to change the origins and destination of flows 
#' stored into a dataframe to simulate a Central Business District urban model. City considered as
#' the main city of the region is designed as candidate and flows are moved 
#' from non-candidate cities to the candidate city
#'
#' @param pol An sf object of the cities
#' @param id A character string of the column containing the id of the pol object
#' @param cand A character string of the column containing binary (1, 0) candidate value of the pol object
#' @param tabflows A data.frame of flows between origins and destinations (long format matrix containing, at least, origins, destinations, flows)
#' @param idori A character string giving the origin field name in tabflows
#' @param iddes A character string giving the destination field name in tabflows
#' @param idflow A character string giving the flow field name in tabflows
#' 
#' @return A data.frame of flows as tabflows with origins and destinations changed
#' 
#' @examples 
#' # Import data
#' 
#' id <- "CODGEO"
#' cand <- "cbd"
#' data(pol)
#' data(tabflows)
#' idori <- "ORI"
#' iddes <- "DES"
#' idflow <- "FLOW"
#' 
#' cbd_flows <- cbd_city (
#' pol = pol, 
#' id = id, 
#' cand = cand, 
#' tabflows = tabFlows,
#' idori = idori,
#' iddes = iddes,
#' idflow = idflow)
#' 
#' cbd_flows[1:10,]
#'
#' @export
#'

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


# Bind partial minimal matrices ----

bind_excess <- function(tabindiv, matcost, idspat, varori, vardes, varwgt, variable, modal){
  matToFill <- matrix(data = rep(0, times = length(matcost)), nrow = nrow(matcost), ncol = ncol(matcost))
  for(i in 1:length(modal)){
    matFlowsPart <- prepare_matflows(tabindiv = tabindiv, 
                                  idspat = idspat, 
                                  varori = varori, 
                                  vardes = vardes, 
                                  varwgt = varwgt,
                                  variable = variable,
                                  label = modal[i])
    matFlowsPartMin <- excess_commuting(matflows = matFlowsPart, matcost = matcost)
    matToFill <- matToFill + matFlowsPartMin
  }
  row.names(matToFill) <- colnames(matToFill) <- colnames(matFlowsPartMin)
  return(matToFill)
}



##### COMPUTE STEWART POTENTIALS #####


# Compute raw stewart raster ----

stewart_raw <- function(tabflows, ref, selexpr = NULL, spatunits, res, span, mask){
  stocks <- stock_flows(tabflows = tabflows, ref = ref, selexpr = selexpr)
  # spatUnits <- AttribJoin(df = as.data.frame(stocks), spdf = spatunits, df.field = "ID", spdf.field = "CODGEO") 
  resGrid <- CreateGrid(w = spatUnits, resolution = res)
  matDist <- CreateDistMatrix(knownpts = spatUnits, unknownpts = resGrid, bypassctrl = TRUE)
  vecStewart <- stewart(knownpts = spatUnits, unknownpts = resGrid, matdist = matDist, 
                        varname = "N", span = span, mask = mask, resolution = res, 
                        typefct = "exponential", beta = 3)
  rasStewart <- rasterStewart(x = vecStewart, mask = mask)
  return(rasStewart)
}



# Compute difference between 2 stewart rasters (DES - ORI) ----

StewartDif <- function(tabflows, selexpr = NULL, spatunits, res, span, mask){
  stocksOri <- stock_flows(tabflows = tabflows, ref = "ORI", selexpr = selexpr)
  stocksDes <- stock_flows(tabflows = tabflows, ref = "DES", selexpr = selexpr)
  stocksOriDes <- full_join(stocksOri, stocksDes, by = "ID") %>% rename(NORI = N.x, NDES = N.y)
  # spatUnits <- AttribJoin(df = as.data.frame(stocksOriDes), spdf = spatunits, df.field = "ID", spdf.field = "CODGEO") 
  resGrid <- CreateGrid(w = spatUnits, resolution = res)
  matDist <- CreateDistMatrix(knownpts = spatUnits, unknownpts = resGrid, bypassctrl = TRUE)
  vecStewartOri <- stewart(knownpts = spatUnits, unknownpts = resGrid, matdist = matDist, 
                           varname = "NORI", span = span, mask = mask, resolution = res, 
                           typefct = "exponential", beta = 3)
  vecStewartDes <- stewart(knownpts = spatUnits, unknownpts = resGrid, matdist = matDist, 
                           varname = "NDES", span = span, mask = mask, resolution = res, 
                           typefct = "exponential", beta = 3)
  rasStewartOri <- rasterStewart(x = vecStewartOri, mask = mask)
  rasStewartDes <- rasterStewart(x = vecStewartDes, mask = mask)
  rasDif <- rasStewartDes - rasStewartOri
  return(rasDif)
}



##### COMPUTE AND MAP INDICATORS #####


# Map indicators ----

mobIndic <- function (tabflow,                 # a df containing the flows 
                      shapeSf,                 # an sf df containing the cities
                      id                       # the cities id
){
  
  #Store Origins to Origins Flow Value into a df name "tabflowOriOri"
  tabflowOriOri <- tabflow %>% filter_( "ORI == DES")
  colnames(tabflowOriOri) <- c("ORI", "DES","OriOriFlow")
  
  #Store Origins Flow Value into a df name "tabflowOri"
  tabflowOri <-  tabflow %>% filter_( "ORI != DES") %>% group_by(ORI) %>% summarise(OriFlow = sum(FLOW))
  
  #Store Destination Flow Value into a df name "tabflowDes"
  tabflowDes <-  tabflow %>% filter_( "ORI != DES") %>% group_by(DES) %>% summarise(DesFlow = sum(FLOW))
  tabflow <- left_join(x = tabflowOriOri, y = tabflowOri, by = c("ORI","ORI"))
  tabflow <- left_join(x = tabflow, y = tabflowDes, by = c("DES","DES"))
  tabflow$DES <- NULL
  colnames(tabflow) <- c("idflow", "OriOriFlow","OriFlow", "DesFlow")
  
  #Building indicators
  tabflow$Dependency <- tabflow$OriOriFlow / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$AutoSuff <- tabflow$OriOriFlow / (tabflow$DesFlow + tabflow$OriOriFlow)
  tabflow$Mobility <- (tabflow$DesFlow+tabflow$OriFlow) / (tabflow$OriFlow + tabflow$OriOriFlow)
  tabflow$RelBal <- (tabflow$DesFlow-tabflow$OriFlow) / (tabflow$OriFlow + tabflow$DesFlow)
  
  shapeSf$idshp <- shapeSf[[id]]
  shapeflow <- merge(x = shapeSf,y = tabflow, by.x="idshp", by.y = "idflow")
  
  return(shapeflow)
}



# dominant flows (Nystuen-Dacey) ----

nystuen_dacey <- function(
  tabflows,   # data.frame with commuting flows, long format (origin, destination, flow)
  poptab,     # table with population, flows summary (total at ori, des, intra) and core id
  idfield,    # CHAR, name of the id field in the population table (poptab)
  targetfield,# CHAR, name of the variable used for weighting
  threspct,    # threshold for defining max flow
  shape,
  shapeId
)
{
  
  #prepare data
  colnames(tabflows) <- c("ORI", "DES", "FLOW")
  tabflows <- tabflows %>%    #on élimine les flux intra
    filter(ORI != DES)
  
  poptab <- poptab %>%  #on crée un tableau propre ne comportant que l'origine, destination et la variable choisie pour pondérer (WGT)
    transmute(ORI = poptab[, idfield],
              DES = poptab[, idfield],
              WGT = poptab[, targetfield])
  
  tabFlowsSum <- tabflows %>% #Tableau des sommes de flux à l'origine
    group_by(ORI) %>% 
    summarise(SUMFLOW = sum(FLOW, na.rm = TRUE))
  
  tabFlowsMax <- tabflows %>% #tableau des flux maximums à l'origine et du pourcentage que représente ce flux par rapport au flux total de la commune
    group_by(ORI) %>% 
    arrange(desc(FLOW)) %>% 
    slice(1) %>% 
    left_join(y = tabFlowsSum, by = "ORI") %>% 
    mutate(PCTMAX = FLOW / SUMFLOW) %>% 
    filter(PCTMAX > threspct)  #ne conserve que les communes dont le flux est superieur au seuil rentré au préalable
  
  tabFlowsAggr <- tabFlowsMax %>%   #Jointure des tableaux des flux maximum et de la variable de pondération (à l'origine et à la destination)
    left_join(x = ., y = poptab[, c("ORI", "WGT")], by = "ORI") %>% 
    left_join(x = ., y = poptab[, c("DES", "WGT")], by = "DES") 
  
  colnames(tabFlowsAggr)[6:7] <- c("WGTORI", "WGTDES")
  
  tabFlowsAggr <- tabFlowsAggr %>% filter(WGTORI < WGTDES) # ne garder que les flux dont la valeur de pondération à la destination est plus grande qu'a l'origine
  
  graphFlows <- graph.data.frame(d = tabFlowsAggr[, c("ORI", "DES")], directed = TRUE) #Pour chaque commune on représente le lien du flux le plus élevé
  V(graphFlows)$DEGIN <- degree(graphFlows, mode = "in")  #nombre de flux entrant par communes
  graphTab <- get.data.frame(x = graphFlows, what = "vertices")
  
  degSorted <- sort(V(graphFlows)$DEGIN, decreasing = TRUE) #on trie du plus grand au plus petit
  degSecond <- degSorted[2] + 1 #on dégage la seconde valeur la plus grande
  
  tabflows <- tabflows %>% # Obtenir le statut des flux d'une commune à l'autre
    left_join(y = graphTab, by = c("ORI" = "name")) %>%
    left_join(y = graphTab, by = c("DES" = "name")) %>%
    mutate(STATUSORI = ifelse(is.na(DEGIN.x) | DEGIN.x == 0, 0, ifelse(DEGIN.x == 1 | DEGIN.x == 2, 1, ifelse(DEGIN.x < degSecond, 2, 3))),
           STATUSDES = ifelse(is.na(DEGIN.y) | DEGIN.y == 0, 0, ifelse(DEGIN.y == 1 | DEGIN.y == 2, 1, ifelse(DEGIN.y < degSecond, 2, 3))),
           STATUS = paste(STATUSORI, STATUSDES, sep = "_")) %>%
    select(ORI, DES, STATUS)
  
  graphTab <- graphTab %>% #Obtenir le statut de pole d'emploi des communes (petit 1, moyen 2, grand 3)
    mutate(STATUS = ifelse(is.na(DEGIN) | DEGIN == 0, 0,
                           ifelse(DEGIN == 1 | DEGIN == 2, 1,
                                  ifelse(DEGIN < degSecond, 2, 3))))
  
  #Get geometry for tabflows
  spLinks <- getLinkLayer(x = shape, xid = shapeId, df = tabFlowsAggr[, c("ORI", "DES")], dfid = c("ORI", "DES"))
  spLinks$KEY <- paste(spLinks$ORI, spLinks$DES, sep = "_")
  tabflows$KEY <- paste(tabflows$ORI, tabflows$DES, sep = "_")
  tabflows <- left_join(spLinks, tabflows[, c("KEY", "STATUS")], by = "KEY")
  # tabflows <- left_join(spLinks, tabflows[,"KEY"], by = "KEY")
  tabflows$KEY <- NULL
  
  #Get geometry for graphTab
  shapeSf <- st_as_sf(shape)
  shapeSfCent <- st_centroid(shapeSf)
  proj4string <- as.character(shape@proj4string)
  xy <- do.call(rbind, st_geometry(shapeSfCent))
  shapeSfCent$lon <- project(xy=xy, proj4string, inv = TRUE)[,1]
  shapeSfCent$lat <- project(xy=xy, proj4string, inv = TRUE)[,2]
  graphTab <- transform(graphTab, name = as.numeric(name))
  graphTab <- left_join(graphTab, shapeSfCent, by = c("name"= shapeId))
  graphTab <- transform(graphTab, name = as.character(name))
  graphTab <- left_join(graphTab, poptab, by = c("name"= "ORI"))
  
  return(list( PTS = graphTab, FLOWS = tabflows))
}

# ROUTING (COMPUTE NETWORK DISTANCE BETWEEN CITIES) ----

routing_machine <- function(road, #A street network represented as sf LINESTRING objects 
                            pol,  #Polygons of cities
                            idpol #Polygons identifier of cities
){
  #Set weight to the same
  road$wgt <- 0
  
  #Création du graph réseau
  roadgraph <- weight_streetnet(x = road, wt_profile = 0, type_col = road$wgt)
  
  #Chopper les centroïdes
  shapesfCent <- st_centroid(pol)
  xy <- do.call(rbind, st_geometry(shapesfCent))
  xy <- data.frame (lon = xy [, 1], lat = xy [, 2])
  
  #Création de la matrice de distance
  matDist <- dodgr_dists(graph = roadgraph, 
                         from = xy, 
                         to = xy)
  
  row.names(matDist) <- shapesfCent[[idpol]]
  colnames(matDist) <- shapesfCent[[idpol]]
  
  return(matDist)
}




##### LOW LEVEL FUNCTIONS #####



# Compute totals by origin or destination ----

stock_flows <- function(tabflows, ref, selexpr){
  if(is.null(selexpr)){
    tabFlows <- tabflows %>% select_(ref, "IPONDI")
    colnames(tabFlows) <- c("ID", "WGT")
  } else {
    tabFlows <- tabflows %>% filter_(selexpr) %>% select_(ref, "IPONDI")
    colnames(tabFlows) <- c("ID", "WGT")
  }
  
  aggrTab <- tabFlows %>% group_by(ID) %>% summarise(N = sum(WGT))
  return(aggrTab)
}


# Prepare OD matrix wide matrix from the table of individuals -----

prepare_matflows <- function(tabindiv, idspat, varori, vardes, varwgt = NULL, variable = NULL, label = NULL){
  tabflows <- create_tabflows(tabindiv = tabindiv, varori = varori, vardes = vardes, varwgt = varwgt, variable = variable, label = label)
  matFlows <- cast_tabflows(tabflows = tabflows, idspat = idspat)
  matFlows <- round(matFlows, digits = 0)
  mode(matFlows) <- "integer"
  return(matFlows)
}


# Create OD matrix (long table) ----

create_tabflows <- function(tabindiv, varori, vardes, varwgt = NULL, variable = NULL, label = NULL){
  # rename variables
  tabindiv$ORI <- tabindiv[[varori]]
  tabindiv$DES <- tabindiv[[vardes]]
  
  # get weights
  if(!is.null(varwgt)){
    tabindiv$WGT <- tabindiv[[varwgt]]
  } else {
    tabindiv$WGT <- 1
  }
  
  # extract selection
  if(!is.null(variable) & !is.null(label)){
    tabindiv <- tabindiv[tabindiv[, variable] == label, ]
  }
  
  # group by origin and destination
  tabFlows <- tabindiv %>% 
    group_by(ORI, DES) %>% 
    summarise(VALUE = sum(WGT)) %>% 
    ungroup()
  
  return(tabFlows)
}

# Cast OD long table into square matrix ----

cast_tabflows <- function(tabflows, idspat){
  tabIndex <- expand.grid(ORI = idspat,
                          DES = idspat,
                          stringsAsFactors = FALSE)
  
  print("Warning! Columns interpretation: 1-ORI, 2-DES, 3-VALUE")
  colnames(tabflows)[1:3] <- c("ORI", "DES", "VALUE")
  tabIndex <- left_join(x = tabIndex, y = tabflows, by = c("ORI", "DES"))
  infoFlowsWide <- dcast(tabIndex, formula = ORI ~ DES, value.var = "VALUE", fill = 0, drop = FALSE)
  matFlows <- as.matrix(infoFlowsWide[, -1])
  row.names(matFlows) <- colnames(matFlows)
  return(matFlows)
}


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
