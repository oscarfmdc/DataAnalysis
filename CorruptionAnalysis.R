# Load the libraries for performing SPARQL queries and for invoking the Google Maps API.
library(SPARQL)
library(ggmap)

# Let’s query the 270a SPARQL endpoint for all the country scores of the Corruption Perceptions Index in 2011.

# First we define the required namespaces.
prefixT = c("sdmx-dimension","http://purl.org/linked-data/sdmx/2009/dimension#",
            "property","http://transparency.270a.info/property/",
            "indicator","http://transparency.270a.info/classification/indicator/",
            "year","http://reference.data.gov.uk/id/year/",
            "skos","http://www.w3.org/2004/02/skos/core#")

sparql_prefixT = "
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX sdmx-dimension: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX property: <http://transparency.270a.info/property/>
PREFIX indicator: <http://transparency.270a.info/classification/indicator/>
PREFIX year: <http://reference.data.gov.uk/id/year/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
"

# Then we define a query to get all the country labels with their scores.
qT = paste(sparql_prefixT,"
SELECT ?countryLabel ?score 
WHERE {
  ?s sdmx-dimension:refPeriod year:2011 ;
    property:indicator indicator:corruption-perceptions-index ;
    property:score ?score ;
    sdmx-dimension:refArea ?countryURI .
  ?countryURI skos:prefLabel ?countryLabel .
}
ORDER BY ?score
")

# And submit it to the SPARQL endpoint.
endpointT = "http://transparency.270a.info/sparql"
optionsT=""
countryScores = SPARQL(endpointT,qT,ns=prefixT,extra=optionsT)$results

# Let’s take a look at the output of the query.
head(countryScores)
tail(countryScores)

# We have a lot of countries, let’s visualize the country scores through a histogram.
hist(countryScores$score, main="Corruption Perceptions Index 2011", xlab = "CPI")

# If we take a look to the labels that we have retreived, we can see that we need to normalize them.
head(countryScores$countryLabel,20)

# Let’s clean the country labels to remove the language tag (@en) and quotes (“).
countryScores$countryLabel = gsub("@en","",countryScores$countryLabel)
countryScores$countryLabel = gsub("\"","",countryScores$countryLabel)

head(countryScores$countryLabel,20)

# Which was Spain’s Corruption Perceptions Index in 2011?
SpainCPI2011 = countryScores[countryScores$countryLabel=="Spain",]

points(SpainCPI2011$score,20,pch=19, col="red")
text(SpainCPI2011$score,23,labels=SpainCPI2011$countryLabel, col="red")

# Now let’s plot the CPI of another country.
OtherCPI2011 = countryScores[countryScores$countryLabel=="Greece",]

points(OtherCPI2011$score,30,pch=19, col="red")
text(OtherCPI2011$score,33,labels=OtherCPI2011$countryLabel, col="red")

# Let’s query DBpedia for all the countries in Europe.

#First we define the namespaces to be used.
prefixD = c("yago","http://dbpedia.org/class/yago/",
            "dbpedia-owl","http://dbpedia.org/ontology/")

sparql_prefixD = "
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX yago: <http://dbpedia.org/class/yago/>
PREFIX dbpedia-owl: <http://dbpedia.org/ontology/>
PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
"

# And we define the query to retrieve every country in Europe.
qD = paste(sparql_prefixD,"
SELECT ?country WHERE {
    ?place rdf:type yago:EuropeanCountries .
    ?place rdf:type dbpedia-owl:Country .
    ?place rdfs:label ?country .
FILTER(LANGMATCHES(LANG(?country), \"en\"))
}
")

# We submit the query to the DBpedia SPARQL endpoint.
endpointD = "http://dbpedia.org/sparql"
optionsD=""

europeanCountries = SPARQL(endpointD,qD,ns=prefixD,extra=optionsD)$results

# And take a look at the output of the query.
europeanCountries

# We obtained the data in a row.
dim(europeanCountries)

# Let’s transpose it to a column.
europeanCountries=as.data.frame(t(europeanCountries))
colnames(europeanCountries)[1] = "countryLabel"
head(europeanCountries,10)

# Now we clean the country label to remove the language tag (@en) and quotes (“).
europeanCountries$countryLabel = gsub("@en","",europeanCountries$countryLabel)
europeanCountries$countryLabel = gsub("\"","",europeanCountries$countryLabel)
head(europeanCountries,10)

# We also need to clean the Georgia and Ireland labels.
europeanCountries$countryLabel = gsub(" \\(country\\)","",europeanCountries$countryLabel)
europeanCountries$countryLabel = gsub("Republic of ","",europeanCountries$countryLabel)
head(europeanCountries)

# We have two data frames to merge: the one with the country scores and the one with the European countries.
head(countryScores)
head(europeanCountries)

# Let’s merge the data frames using the values of their countryLabel columns.
europeanCountriesWithCPI = merge(countryScores, europeanCountries, 
                                  by.x = "countryLabel", by.y = "countryLabel")
head(europeanCountriesWithCPI,10)

# Do we have CPIs for all European countries?
europeanCountriesWithCPI.allY = merge(countryScores, europeanCountries, 
                                      by.x = "countryLabel", by.y = "countryLabel", 
                                      all.y=TRUE)
europeanCountriesWithCPI.allY[is.na(europeanCountriesWithCPI.allY$score),]

# Let’s visualize the corruption scores along Europe.
hist(europeanCountriesWithCPI$score, main="Corruption Perceptions Index 2011 in Europe", xlab = "CPI")

# And now we plot Spain’s CPI and the CPI of another country.
points(SpainCPI2011$score,5,pch=19, col="red")
text(SpainCPI2011$score,5.5,labels=SpainCPI2011$countryLabel, col="red")

points(OtherCPI2011$score,5,pch=19, col="red")
text(OtherCPI2011$score,5.5,labels=OtherCPI2011$countryLabel, col="red")

# Draw the two graphs (World and Europe) at once.

# With par we define a new graph configuration to include two plots in a graph. At the end we restore it to include only one graph.
par(mfrow=c(1, 2))

hist(countryScores$score, main="World Corruption Perceptions Index 2011", xlab = "CPI")
points(SpainCPI2011$score,20,pch=19, col="red")
text(SpainCPI2011$score,23,labels=SpainCPI2011$countryLabel, col="red")
points(OtherCPI2011$score,30,pch=19, col="red")
text(OtherCPI2011$score,33,labels=OtherCPI2011$countryLabel, col="red")

hist(europeanCountriesWithCPI$score, main="Europe Corruption Perceptions Index 2011", xlab = "CPI")
points(SpainCPI2011$score,5,pch=19, col="red")
text(SpainCPI2011$score,5.5,labels=SpainCPI2011$countryLabel, col="red")
points(OtherCPI2011$score,5,pch=19, col="red")
text(OtherCPI2011$score,5.5,labels=OtherCPI2011$countryLabel, col="red")

par(mfrow=c(1, 1))

# Let’s query again the 720a data set for all the country scores of the Corruption Perceptions Index in 2011, 
# but now we also retrieve the links to DBpedia.

# We can reuse the same prefixes as before.
qT2 = paste(sparql_prefixT,"
SELECT ?score ?countryLabel ?URI
WHERE {
  ?s sdmx-dimension:refPeriod year:2011 ;
    property:indicator indicator:corruption-perceptions-index ;
    property:score ?score ;
    sdmx-dimension:refArea ?countryURI .
  ?countryURI skos:prefLabel ?countryLabel ;
    owl:sameAs ?URI .
  FILTER (REGEX(STR(?URI),\"dbpedia\"))
}
ORDER BY ?score
")

# Let’s run the query and see the output.
countryScoresWLinks = SPARQL(endpointT,qT2,ns=prefixT,extra=optionsT)$results

head(countryScoresWLinks,10)

# And now we query DBpedia for all the URIs of the countries in Europe.
qD2 = paste(sparql_prefixD,"
SELECT ?place  WHERE {
    ?place rdf:type yago:EuropeanCountries .
    ?place rdf:type dbpedia-owl:Country .
}
")

# Let’s run the query and see the output.
europeanCountriesWURIs = SPARQL(endpointD,qD2,ns=prefixD,extra=optionsD)$results

europeanCountriesWURIs[,1:10]

# Let’s transpose it into a column.
europeanCountriesWURIs=as.data.frame(t(europeanCountriesWURIs))
colnames(europeanCountriesWURIs)[1] = "countryURI"
head(europeanCountriesWURIs)

# Now we merge the data using the URIs instead of the labels.

# On the one hand there is no need to clean the labels and search for potential problems.
# On the other hand, we now that if both URIs are the same, they refer to the same entity.
europeanCountriesWithCPIv2 = merge(countryScoresWLinks, europeanCountriesWURIs, by.x = "URI", by.y = "countryURI")
head(europeanCountriesWithCPIv2,10)

# With these data we obtain the same histogram.
hist(europeanCountriesWithCPIv2$score, main="Corruption Perceptions Index 2011 in Europe", xlab = "CPI")
points(SpainCPI2011$score,5,pch=19, col="red")
text(SpainCPI2011$score,5.5,labels=SpainCPI2011$countryLabel, col="red")
points(OtherCPI2011$score,5,pch=19, col="red")
text(OtherCPI2011$score,5.5,labels=OtherCPI2011$countryLabel, col="red")

# Let’s show the most corrupt countries in a map.

# Let’s query DBpedia for all the countries in Europe with their URI and coordinates.
qD3 = paste(sparql_prefixD,"
SELECT ?place ?lat ?long WHERE {
    ?place rdf:type yago:EuropeanCountries .
    ?place rdf:type dbpedia-owl:Country .
    ?place geo:lat ?lat .           
    ?place geo:long ?long . 
}
")

# Let’s run the query and see the output.
europeanCountriesWCoord = SPARQL(endpointD,qD3,ns=prefixD,extra=optionsD)$results

head(europeanCountriesWCoord,10)

# Now we merge the results with the previous data.
europeanCountriesWithCPIandCoord = merge(europeanCountriesWithCPIv2, europeanCountriesWCoord, 
                                          by.x = "URI", by.y = "place")
head(europeanCountriesWithCPIandCoord,10)

# We can put all these data in a map by calling the Google Maps API.
qmap('Brussels', zoom=4) +
     geom_point(aes(x=europeanCountriesWithCPIandCoord$long, 
                    y=europeanCountriesWithCPIandCoord$lat), 
     data=europeanCountriesWithCPIandCoord, size=europeanCountriesWithCPIandCoord$score, 
     colour="red") + 
     scale_color_manual(values = rainbow(10))
	 
# Let’s show the most corrupt ones (CPI<4).
europeanCountriesWithCPIandCoordWorst = europeanCountriesWithCPIandCoord[europeanCountriesWithCPIandCoord$score<4,]
europeanCountriesWithCPIandCoordWorst

# And plot them in the map.
qmap('Brussels', zoom=4) +
  geom_point(aes(x=europeanCountriesWithCPIandCoordWorst$long, y=europeanCountriesWithCPIandCoordWorst$lat), 
             data=europeanCountriesWithCPIandCoordWorst, size=europeanCountriesWithCPIandCoordWorst$score, colour="red") + 
  scale_color_manual(values = rainbow(10))
 
# Is there any correlation between the movies produced by a country and the Corruption Perceptions Index?

# Let’s query the Linked Movie Data Base for all the movies from European countries 
# Note: the “IN” approach doesn’t work for the endpoint, so we change the approach.

# Let’s first define the prefixes and the endpoint, and initialize a dataframe for storing the results.
prefixI = c("movie","http://data.linkedmdb.org/page/movie/")

sparql_prefixI = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX movie: <http://data.linkedmdb.org/resource/movie/>
"
endpointI = "http://data.linkedmdb.org/sparql"
optionsI=""

moviesPerCountry = data.frame(country= character(0), numMovies = integer(0))

# Then we search for the movies in every country.
for (i in 1:length(europeanCountriesWithCPIandCoord$URI)) {
  qI = paste(sparql_prefixI,"
  SELECT ?country (COUNT(*) AS ?numMovies) WHERE
  { 
    ?countryI rdf:type movie:country .
    ?countryI owl:sameAs ?country .
    ?film movie:country ?countryI .
  FILTER (?country=", europeanCountriesWithCPIandCoord$URI[i] ,")
  }
  GROUP BY ?country
  ")
  
  numberMovies  = SPARQL(endpointI,qI,ns=prefixI,extra=optionsI)$results

  moviesPerCountry = rbind(moviesPerCountry,numberMovies)

  # To avoid being rejected by the endpoint
  Sys.sleep(2)
}

# It may happen that the Linked Movie Data SPARQL endpoint is down. In that case, we can generate the number of movies randomly.
for (i in 1:length(europeanCountriesWithCPIandCoord$URI)) {
  numberMovies = data.frame(matrix(ncol = 2, nrow = 1))
  numberMovies[1,1] = europeanCountriesWithCPIandCoord$URI[i]
  numberMovies[1,2] = round(rnorm(1, mean = 500, sd = 200))
  colnames(numberMovies) = c("country","numMovies")

  moviesPerCountry = rbind(moviesPerCountry,numberMovies)
}

# We obtained the number of movies per country.
head(moviesPerCountry,10)

# We merge our results with the previous data.
europeanCountriesWithCPICoordMovies = merge(europeanCountriesWithCPIandCoord, moviesPerCountry, 
                                             by.x = "URI", by.y = "country")
head(europeanCountriesWithCPICoordMovies, 10)

# Is there any correlation between the number of movies produced by a country in IMDB and the Corruption Perceptions Index in 2011?
plot(europeanCountriesWithCPICoordMovies$score,europeanCountriesWithCPICoordMovies$numMovies,
     main="Number of movies per CPI", xlab = "CPI", ylab="Numer of movies")