#' Load alignments and prepare template correspondence table for concatenate () function
#'
#' This function loads all the fasta files present in the working directory,
#' creates a template correspondence table that is saved in the working directory.
#'
#' @importFrom stats sd
#' @importFrom utils write.table
#' 
#' @param filename filename of the saved correspondence table template
#' @param excel logic indicating if saving the correspondence table template in the working directory in excel format (otherwise it is saved as text file)
#' @param tibble Boolean, should the function return the correspondence table as a tibble instead of saving it as a file? Default is \code{FALSE}. If set to \code{TRUE}, the values of \code{filename} and \code{excel} are disregarded.
#' @param exclude fasta files with this text in the working directory will be ingnored by the function
#' @return If \code{tibble} is \code{TRUE}, a tibble with the correspondence table template.
#' @export

concatipede_prepare = function(filename="seqnames", excel = TRUE, tibble = FALSE, exclude="concatenated"){
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

    # Return df as a tibble if required
    if (tibble) {
        return(tibble::as_tibble(df))
    }

  # save the template for the translation table as txt or excel
    if(excel==F){write.table(df,sep="\t",file=paste0(filename,".txt"),row.names=FALSE,quote=FALSE)}
    if(excel==T){writexl::write_xlsx(df,path=paste0(filename,".xlsx"),col_names=T,format_headers=T)}
}
