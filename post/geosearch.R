#https://www.geonames.org/login
#http://www.geonames.org/enablefreewebservice
## Install package
#install.packages("geonames")
library(geonames)
options(geonamesUsername="user")
drc <- read.delim("drc.tsv")
results <- data.frame( adminCode1= "",
                       lng= "",
                       geonameId= "",
                       toponymName= "",
                       countryId= "",
                       fcl= "",
                       population= "",
                       countryCode= "",
                       name= "",
                       fclName= "",
                       adminCodes1.ISO3166_2= "",
                       countryName= "",
                       fcodeName= "",
                       adminName1= "",
                       lat= "",
                       fcode= "",
                       Village= "",
                       Departement= "",
                       District= "",
                       stringsAsFactors = FALSE)

for (i in 1:nrow(drc)) {
  #i <- 1
  Village <- as.character(drc[ i, c("VILLAGES")])
  Departement <- as.character(drc[ i, c("Departement")])
  District <- as.character(drc[ i, c("District")])
  cat(paste0("searching for ", Village, "\n"))

  resulti <- GNsearch(q = Village ,
                      country = "CD",
                      featureClass = "P",
                      continentCode = "AF",
                      fuzzy = "0.6",
                      maxRows = 20)
  cat(paste0("Results potential ", nrow(resulti), "\n"))
  if (nrow(resulti) > 0 ) {
    resulti$Village  <- Village
    resulti$Departement  <- Departement
    resulti$District  <- District
    results <- rbind(results, resulti) } else { results <- results}
}

#names(resulti)

write.csv(results, "results.csv", row.names = FALSE)
