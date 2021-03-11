.rename.seqs=function(dataset,table,align){
  #this function takes as input a list of alignments (dataset), a translation table (table) and the name of the alignment (align)
  #inside the list in which change the names
  require("ape")

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
  require("ape")

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
