#' Load alignments and prepare template correspondence table for concatenate () function
#'
#' This function loads all the fasta files present in the working directory,
#' creates a template correspondence table that is saved in the working directory.
#' This function also output an object (list) that is the input for the concatenate () function.
#'
#' @param filename filename of the saved correspondence table template
#' @param writetable logic indicating if saving the template correspondence table file in the working directory
#' @param excel logic indicating if saving the correspondence table template in the working directory in excel format (otherwise it is saved as text file)
#' @param exclude fasta files with this text in the working directory will be ingnored by the function
#' @return List object containing alignments, this object is one of the input for concatipede() function
#' @export

concatipede_prepare = function(filename="seqnames",writetable=T,excel=F,exclude="concatenated"){
  #filename: specify the filename of the raw translation table to be created
  #writetable: do not write the translation table, only generate input object for concatenat.part2() function
  #excel: output table as excel file
  #exclude: exclude fasta files with a keyword


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

  #create dataframe with sequence names
  df <- as.data.frame(matrix(0, ncol = length(l)+1, nrow = maxlen))
  colnames(df)=c("name",names(l))

  for(i in 1:length(l)){
    column=names(l[i])
    seqnames=c(names(l[[i]]),rep("",maxlen-length(names(l[[i]]))))
    df[,i+1]=seqnames
  }

  # save the template for the translation table as txt or excel
  if (writetable==T) {
    if(excel==F){write.table(df,sep="\t",file=paste0(filename,".txt"),row.names=FALSE,quote=FALSE)}
    if(excel==T){xlsx::write.xlsx(df,file=paste0(filename,".xlsx"),col.names=T,row.names=F,append=F)}
  }
  return(l)
}
