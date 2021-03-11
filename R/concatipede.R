#' Concatenate alignments
#'
#' This function concatenate sequences from alignments present in the working directory based on a correspondence table and saves the output in a new directory
#'
#' @param filename filename of correspondence table
#' @param format a string specifying in what formats you want the alignment
#' @param plotimg return a graphical representation of the alignment in pdf format
#' @param return.aln return the concatenate alignment inside R workspace
#' @param remove.gaps remove gap only columns. Useful if not using all sequences in the alignments
#' @param write.outputs save concatenated alignment, partitions position table and graphical representation. If FALSE it overrides plotimg
#' @param excel.sheet specify what sheet from the exce spreadsheet you wanna read. Either a string (the name of a sheet), or an integer (the position of the sheet).
#' @param exclude fasta files with this text in the working directory will be ingnored by the function
#' @param out specify outputs filename
#' @return Can return alignment in the workspace if return.aln is set to TRUE
#' @export

concatipede = function(filename="seqnames.txt",
                       format=c("fasta","nexus","phylip"),
                       plotimg=T,
                       return.aln=F,
                       out=NULL,
                       remove.gaps=TRUE,
                       write.outputs=TRUE,
                       excel.sheet=1,
                       exclude="concatenated"){
  require("ape")
  # check if the translation table is in text format or in excel
  if(grepl(".txt",filename)==TRUE){df=read.table(filename,header=T,sep="\t",check.names=F)}
  if(grepl(".xlsx",filename)==TRUE){
    df=readxl::read_xlsx(path=filename, sheet=excel.sheet, col_names=TRUE)
    colnames(df)=unlist(lapply(colnames(df),.colname.clean))
    df=as.data.frame(apply(df,2,.clean.NA))
  }


  #read files in the foldes and create a list
  files=list.files(pattern = "\\.fas")

  #exclude the files containing the "excluded" word in the filename
  if (!is.null(exclude)){
    toKeep=!grepl(exclude,files)
    files=files[toKeep]}

  # load the fasta alignments, do some quality check and rename them with the original file name
  l=list()
  maxlen=0
  for (i in 1:length(files)){
    dataset=ape::read.FASTA(files[i])
    a=sd(unlist(lapply(dataset,length)))
    if(a!=0){cat("ATTENTION! In file ",files[i]," not all sequences of same length \n")}
    l[[i]]=assign(files[i],dataset)
    if(length(names(dataset))>maxlen){maxlen=length(names(dataset))}
  }
  names(l)=files

  alignments = l

  # this allow to not to use all the genes in the concatenations
  lR=alignments[names(alignments) %in% colnames(df)[-1]]

  #rename sequences in each alignment with the final sequence name
  for (j in 1:length(lR)){
    align=names(lR[j])
    lR[[j]]=.rename.seqs(lR[[j]],table=df,align=align)
    lR[[j]]=lR[[j]][!is.na(names(lR[[j]]))]} #this delete all sequences not present in the translation table


  lR=lapply(lR,as.matrix)

  if(remove.gaps == TRUE){lR = lapply(lR,ape::del.colgapsonly)}

  if (length(lR) == 1){conc = lR}
  if (length(lR) != 1){
  #concatenate alignments by name
  conc=cbind(lR[[1]],lR[[2]],fill.with.gaps=T)
  if(length(lR)>2){
    for (i in 3:length(lR)){
      conc=cbind(conc,lR[[i]],fill.with.gaps=T)
    }}}

  #Create data frame with partitions lenghts and limits
  v=vector()
  for (i in 1:length(lR)){
    v[i]=max(unlist(lapply(lR[[i]],length)))}
  len.df=data.frame(alignment=names(lR),lenght=v)


  len.df$from=rep(0,nrow(len.df))
  len.df$to=rep(0,nrow(len.df))
  len.df$from[1]=1
  len.df$to[1]=len.df$lenght[1]

  if (length(lR) != 1){
  for (i in 2:nrow(len.df)){
    len.df$from[i]=len.df$to[i-1]+1
    len.df$to[i]=len.df$to[i-1]+len.df$lenght[i]}}

  # just to make sure it will be in the right format to be saved
  if (length(lR) == 1){conc = conc[[1]]}



  if (write.outputs == T){

  #Create directory where to save outputs file

  if(!is.null(out)){dir_name = out}
  if(is.null(out)){dir_name = "concatenated"}

  # this part will check if a directory already exists and to avoid overwrite it append
  #a progressive number to the name of the new folder
  base_dir_name = dir_name
  N = 0
  while (dir.exists(dir_name)) {
    N = N+1
    dir_name = paste0(base_dir_name,"_",N)
  }

  dir.create(dir_name)

  # save alignment
  if(is.null(out)){write.alignment(conc,name=paste0(dir_name,"/concatenated"),format=format)}
  if(!is.null(out)){write.alignment(conc,name=paste0(dir_name,"/",out),format=format)}


  #Save partition lenghts table
  if(is.null(out)){filename=paste0(dir_name,"/length_summary.txt")}
  if(!is.null(out)){filename=paste0(dir_name,"/",out,"_length_summary.txt")}
  write.table(len.df,file=filename,sep="\t",quote=FALSE,row.names=FALSE)

  #alignment plotting option
  if(is.null(out)){filename=paste0(dir_name,"/concatenated_alignment.pdf")}
  if(!is.null(out)){filename=paste0(dir_name,"/",out,"_concatenated_alignment.pdf")}

  if (plotimg==T){pdf(filename)
    img=image(conc,cex=0.3)
    dev.off()}
  }

  #return concatenated alignment in R option
  if(return.aln==T) {return(conc)}

}
