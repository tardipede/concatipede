.rename.seqs=function(dataset,table,align){
  #this function takes as input a list of alignments (dataset), a translation table (table) and the name of the alignment (align)
  #inside the list in which change the names

  col=match(align,colnames(table))


  newalign=list()

  for (i in 1:nrow(table)){
    newalign[i]=dataset[match(table[i,col],names(dataset))]
  }
  class(newalign)="DNAbin"
  names(newalign)=as.character(table[,1])

  if (any(sapply(newalign, is.null))){
    newalign=newalign[-which(sapply(newalign, is.null))]}

  return(newalign)
}

#this function is a wrapper to write alignments in 3 different formats
#save one alignment file for each format specified
write.alignment=function(align,name,format=c("fasta","nexus","phylip")){

  if("fasta" %in% format){ape::write.FASTA(align,file=paste0(name,".fasta"))}
  if("nexus" %in% format){ape::write.nexus.data(align,file=paste0(name,".nexus"),interleaved=F)}
  if("phylip" %in% format){
    ape::write.dna(align,file=paste0(name,".phy"),colsep="",format="sequential",colw=max(sapply(align, length)))}
}

#helper function to format the data frame loaded with xlsx - removes the X added at the beginning of the column name when it starts with a number
.colname.clean=function(x){
  if(substr(x, 1, 1)=="X"){x=substring(x,2)}
  return(x)
}

#helper function to format the data frame loaded with xlsx - remove NAs and replace them with an empty cell
.clean.NA=function(x){
  x=as.character(x)
  x[is.na(x)]=""
  x=as.factor(x)
  return(x)

}

#Checks if a text string containg a genbank accession number - helper for get_genbank_table
.contains_accnos=function(x){
  if(length(x)==1){y = length(grep("[A-Z]{2}[0-9]{6}",x,value=F))}
  if(length(x)>1){
    y=c()
    for (i in 1:length(x)){
      y[i] = length(grep("[A-Z]{2}[0-9]{6}",x[i],value=F))
    }
  }
  return(y)
}

# Extract accession number froma  single object - helper for get_genbank_table
.extract_accnos_single = function(x){
  x = as.character(x)
  if(.contains_accnos(x)==0){y = "NA"}
  if(.contains_accnos(x)==1){y = as.character(stringr::str_extract_all(x,"[A-Z]{2}[0-9]{6}",simplify=T))}
  return(y)
}

# Extract accession numbers from a vector - helper for get_genbank_table
.extract_accnos = function(x){
  if(length(x) == 1){y = .extract_accnos_single(x)}
  if(length(x) > 1){
    y=c()
    for (i in 1:length(x)){
      y[i] = .extract_accnos_single(x[i])
    }
  }
  return(y)
}

# This function cleans the new sequence names for the rename_sequences() function by removing eventual NA's at the beginning and _ at the beginning or end
.clean.names = function(x){
  if(substr(x,1,3)=="NA_"){x = substr(x,4,nchar(x))}
  if(substr(x,1,1)=="_"){x = substr(x,2,nchar(x))}
  if(substr(x,nchar(x),nchar(x))=="_"){x = substr(x,1,nchar(x)-1)}
  return(x)
}
