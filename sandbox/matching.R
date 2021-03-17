### * Description

# Script to gather and tidy code from Matteo to perform sequence name matching

### * From Matteo's comments on the issue #6 in Gitlab repo on 2021-03-16

# [The corresponding sequences.xlsx is saved in sandbox/]

seqtable = data.frame(readxl::read_xlsx("sequences.xlsx")) #correspondence table output from concatipede_prepare()
V1 = as.vector(unlist(seqtable[,2])) # column 1 is the names so I don't use it for matching
V1 = V1[!is.na(V1)]
V2 = as.vector(unlist(seqtable[,3]))
V2 = V2[!is.na(V2)]
matches = data.frame(string_to_match = V1,
closest_match = V2[apply(adist(x = V1, y = V2,ignore.case = TRUE, partial = TRUE), 1, which.min)],
distance = apply(adist(x = V1, y = V2,ignore.case = TRUE, partial = TRUE), 1,min))
#find id treshold by calculatind a density distribution of the distances and giving the treshold as the end of the first density peak
index = pracma::findpeaks(density(matches$distance)$y)[1,4]
dist_treshold = ceiling(density(matches$distance)$x[index])+6 #6 is for genbank accession number
#get only "good" matches
good_matches = matches[matches$distance<=dist_treshold,]

# This is a way to extract the names from a row of matched sequences: (it still need to be cleaned)
matched_names=c()
for (i in 1:nrow(good_matches)){
to_match = unlist(good_matches[i,1:2])
matched_names[i] = GrpString::CommonPatt(unlist(to_match),low=100)[1,1]
}

### * E-mail from Matteo (2021-03-16)

## I made a function to match sequence names between two vectors.

## It does not report the names that does not have a good match in the other
## vector, however at the end they can be appended in the last rows of the
## correspondence table where it is clear they don´t have any match.

## The problem is how to expand it between columns. I think it would be better
## to match each couple of columns in a template correspondence table and then
## find a way to merge the results

seqtable = data.frame(readxl::read_xlsx("sequences.xlsx")) #correspondence table output from concatipede_prepare()
#seqtable = data.frame(readxl::read_xlsx("Macrobiotidae_seqnames.xlsx")) #correspondence table output from concatipede_prepare()

V1 = as.vector(unlist(seqtable[,2])) # column 1 is the names so I don't use it for matching
V1 = unique(V1[!is.na(V1)])
V2 = as.vector(unlist(seqtable[,3]))
V2 = unique(V2[!is.na(V2)])

match_seqnames = function(V1,V2){
# MATCHING IN THE FIRST DIRECTION
matches = data.frame(string_to_match = V1,
                     closest_match = V2[apply(adist(x = V1, y = V2,ignore.case = TRUE, partial = TRUE), 1, which.min)],
                     distance = apply(adist(x = V1, y = V2,ignore.case = TRUE, partial = TRUE), 1,min))
#find id treshold by calculatind a density distribution of the distances and giving the treshold as the end of the first density peak
index = pracma::findpeaks(density(matches$distance)$y)[1,4]
dist_treshold = ceiling(density(matches$distance)$x[index])+6 #6 is to allor for flxibility on genbank accession number
#get only "good" matches
good_matches_1 = matches[matches$distance<=dist_treshold,]
colnames(good_matches_1)[1:2] = c("V1","V2")


# MATCHING IN THE second DIRECTION
matches = data.frame(string_to_match = V2,
                     closest_match = V1[apply(adist(x = V2, y = V1,ignore.case = TRUE, partial = TRUE), 1, which.min)],
                     distance = apply(adist(x = V2, y = V1,ignore.case = TRUE, partial = TRUE), 1,min))
#find id treshold by calculatind a density distribution of the distances and giving the treshold as the end of the first density peak
index = pracma::findpeaks(density(matches$distance)$y)[1,4]
dist_treshold = ceiling(density(matches$distance)$x[index])+6 #6 is to allor for flxibility on genbank accession number
#get only "good" matches
good_matches = matches[matches$distance<=dist_treshold,]
good_matches = good_matches[order(good_matches$distance),]
good_matches_2 = good_matches[!duplicated(good_matches$closest_match),]
good_matches_2 = data.frame(V1 = good_matches_2$closest_match, V2 = good_matches_2$string_to_match, distance = good_matches_2$distance)


# Combine the two dataframes and filter them for duplicate entries
good_matches_comb = rbind(good_matches_1, good_matches_2)
good_matches_comb = good_matches_comb[!duplicated(good_matches_comb),]
good_matches_comb = good_matches_comb[order(good_matches_comb$distance),]
good_matches_comb = good_matches_comb[!duplicated(good_matches_comb$V1),]
good_matches_comb = good_matches_comb[!duplicated(good_matches_comb$V2),]

return(good_matches_comb)}

### * E-mail from Matteo (2021-03-16)

## I attach another snippet of code I make in how to put together all the
## different alignments, but I couldn´t manage to complete it


# Generate a datframe with the possible columns combinations (excluding the first column)
col_numbers = 2:ncol(seqtable)
col_numbers = data.frame(t(combn(col_numbers,m=2)))


# for each couple of columns, match the sequence names
correspondences = list()
for (i in 1:nrow(col_numbers)){
  # extract the columns ad vectors and remove duplicates and NAs
  V1 = unlist(seqtable[,col_numbers[i,1]])
  V1 = unique(V1[!is.na(V1)])
  
  V2 = unlist(seqtable[,col_numbers[i,2]])
  V2 = unique(V2[!is.na(V2)])
 
  matchtable_temp = match_seqnames(V1,V2)
  #colnames(matchtable_temp)[1:2] = colnames(seqtable)[unlist(col_numbers[i,])]
  
  correspondences[[i]] = matchtable_temp
   
}

# bind all the tables together
binded_tables = do.call(dplyr::bind_rows,correspondences)


# Create a network and identify all the separate clusters
gD <- igraph::simplify(igraph::graph.data.frame(binded_tables[,1:2], directed=FALSE))
lou <- cluster_louvain(gD)

# plot(lou, gD, vertex.label = NA, vertex.size=5, edge.arrow.size = .2) useless but makes a pretty plot


## identify and keep only clusters with numbers of components =<  number of alignments
Small = which(table(lou$membership) <= ncol(seqtable)-1 )

## Which nodes should be kept?
Keep = V(gD)[(lou$membership %in% Small)]

gD2  = induced_subgraph(gD, Keep)
lou2 = cluster_louvain(gD2)

# Ideally only the clusters with a cyclic structure should be kept and then their components mapped to the original columns
# But I have no idea on how to do it
