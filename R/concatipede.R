#' Concatenate alignments
#'
#' This function concatenate sequences from alignments present in the working directory based on a correspondence table and saves the output in a new directory
#'
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics image
#' @importFrom utils read.table
#'
#' @param df The user-defined correspondence table, as a data frame or equivalent. This is used only if no \code{filename} argument is provided.
#' @param filename Filename of input correspondence table. Alternatively, if no filename is provided, the user can provide their own correspondence table as the \code{df} argument.
#' @param format a string specifying in what formats you want the alignment
#' @param dir Optional, path to the directory containing the fasta files. This argument has an effect only if fasta files names are taken from the columns of the \code{df} argument, and that \code{df} does not have an attribute \code{dir_name} itself. If no \code{dir} is provided and \code{df} does not have a \code{dir_name} attribute, the current working directory is ued with a warning.
#' @param plotimg return a graphical representation of the alignment in pdf format
#' @param remove.gaps remove gap only columns. Useful if not using all sequences in the alignments
#' @param write.outputs save concatenated alignment, partitions position table and graphical representation. If FALSE it overrides plotimg
#' @param save.partitions save in the concatenated alignmeent directory a text file with partitions limits for the concatenated alignment.
#' @param excel.sheet specify what sheet from the excel spreadsheet has to be read. Either a string (the name of a sheet), or an integer (the position of the sheet).
#' @param out specify outputs filename
#'
#' @return The concatenated alignment (invisibly if \code{out} is not NULL).
#'
#' @examples
#' dir <- system.file("extdata", package = "concatipede")
#' z <- concatipede(filename = paste0(dir,"/Macrobiotidae_seqnames.xlsx"), dir = dir)
#' z
#'
#' @export

concatipede <- function(df = NULL,
                        filename = NULL,
                        format = c("fasta","nexus","phylip"),
                        dir,
                        plotimg = FALSE,
                        out = NULL,
                        remove.gaps = TRUE,
                        write.outputs = TRUE,
                        save.partitions = TRUE,
                        excel.sheet = 1){

  # Check that exactly one of `filename` or `df` is provided
  if (is.null(df) & is.null(filename)) {
      stop("Either `filename` or `df` must be provided.")
  }
  if (!is.null(df) & !is.null(filename)) {
      stop("Only one of `filename` or `df` must be provided, not both.")
  }
  # If `filename`was provided
  if (!is.null(filename)) {
      # check if the translation table is in text format or in excel
      if(grepl(".txt$",filename)==TRUE){
          df=read.table(filename,header=T,sep="\t",check.names=F)
      } else if(grepl(".xlsx$",filename)==TRUE){
          df=readxl::read_xlsx(path=filename, sheet=excel.sheet, col_names=TRUE)
          colnames(df)=unlist(lapply(colnames(df),.colname.clean))
          df=as.data.frame(apply(df,2,.clean.NA))
      } else {
          stop("Input file format not recognized. `filename` must end with \".txt\" or \".xlsx\".")
      }
      message("Loading the fasta files from the current directory (", getwd(), ").")
      fasta_dir_name <- getwd()
  } else {
      # Check: was `df` provided by the user?
      stopifnot(!is.null(df))
      # Forcing df to be a data frame (things do not work properly in the rest
      # of the function is df was given as a tibble and is not converted to a
      # data frame)
      df <- as.data.frame(df)
      # Check the dir_name attribute
      fasta_dir_name <- attr(df, "dir_name")
      if (is.null(fasta_dir_name) & missing(dir)) {
          warning("The `df` input did not have a \"dir_name\" attribute and no `dir` argument was passed: loading the fasta files from the current directory by default (", getwd(), ").")
          fasta_dir_name <- getwd()
      } else if (!is.null(fasta_dir_name) & missing(dir)) {
          message("Loading the fasta files from the directory stored in the \"dir_name\" attribute of `df` (", fasta_dir_name, ").")
      } else if (!is.null(fasta_dir_name) & !missing(dir)) {
          warning("A `dir` argument was passed and overrides the \"dir_name\" attribute of `df`. The `dir` argument is ", dir, " and it will be used to load the fasta files, while the \"dir_name\" attribute of `df` was ", fasta_dir_name, " and it will be ignored.")
          fasta_dir_name <- dir
      } else {
          stopifnot(is.null(fasta_dir_name) & !missing(dir))
          message("Loading the fasta files from the directory provided by the `dir` argument (", dir, ").")
          fasta_dir_name <- dir
      }
  }

  # Remove all the dataframe columns before the "name" columns
  df = df[,which(colnames(df)=="name"):ncol(df)]

  # Take the file names for the column names of df
  files <- colnames(df)[2:ncol(df)]

  # load the fasta alignments, do some quality check and rename them with the original file name
  l=list()
  maxlen=0
  for (i in 1:length(files)){
    dataset=ape::read.FASTA(file.path(fasta_dir_name, files[i]))
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
    lR[[j]]=lR[[j]][!is.na(names(lR[[j]]))]} #this delete all sequences not present in the correspondence table


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

      # Save alignment
      if (is.null(out)) {
          write.alignment(conc, name = paste0(dir_name, "/concatenated"), format = format)
      } else {
          write.alignment(conc, name = paste0(dir_name, "/", out), format = format)
      }


  #Save partition lenghts table
  if (save.partitions == TRUE){
  if(is.null(out)){filename=paste0(dir_name,"/length_summary.txt")}
  if(!is.null(out)){filename=paste0(dir_name,"/",out,"_length_summary.txt")}
  write.table(len.df,file=filename,sep="\t",quote=FALSE,row.names=FALSE)}

  #alignment plotting option
  if(is.null(out)){filename=paste0(dir_name,"/concatenated_alignment.pdf")}
  if(!is.null(out)){filename=paste0(dir_name,"/",out,"_concatenated_alignment.pdf")}

  if (plotimg==T){pdf(filename)
    img=image(conc,cex=0.3)
    dev.off()}
  }

    # Return concatenated alignment
    if (!is.null(out)) {
        return(invisible(conc))
    }
    conc
}
