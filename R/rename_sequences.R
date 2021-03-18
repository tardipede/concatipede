#' Rename sequences
#'
#' This function rename sequences in fasta files based on a correspondence table
#'
#'
#' @param filename Filename of correspondence table. Alternatively, if no filename is provided, the user can provide their own correspondence table as the \code{df} argument.
#' @param df The user-defined correspondence table, as a data frame or equivalent. This is used only if no \code{filename} argument is provided.
#' @param marker_names the name of the marker for each alignment to be appended at the end of the sequences names, in the same order as in the correspondence table
#' @param out specify outputs filename
#' @param format a string specifying in what formats you want the alignment. Can be "fasta", "phylip" and "nexus"
#' @param excel.sheet specify what sheet from the excel spreadsheet you wanna read. Either a string (the name of a sheet), or an integer (the position of the sheet).
#' @param unalign return unaligned fasta files as output
#' @export
rename_sequences = function(df = NULL,
                            filename = NULL,
                            marker_names = NULL,
                            out = NULL,
                            format = "fasta",
                            excel.sheet = 1,
                            unalign = F){

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
} else {
  # Check: was `df` provided by the user?
  stopifnot(!is.null(df))
  # Forcing df to be a data frame (things do not work properly in the rest
  # of the function is df was given as a tibble and is not converted to a
  # data frame)
  df <- as.data.frame(df)
}

#remove all the dataframe columns before the "name" columns
df = df[,which(colnames(df)=="name"):ncol(df)]

#read files in the foldes and create a list
files=list.files(pattern = "\\.fas")

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

# this allow to not to use all the genes in the concatenations as it will remove from the alignments list all the ones not present in the correspondendce table
lR=alignments[names(alignments) %in% colnames(df)[-1]]


# Create new names for the sequences starting from the genbank accession number
new_names = get_genbank_table(df)
for (i in 2:ncol(new_names)){
  new_names[,i] = paste0(unlist(new_names[,i]),"_",unlist(new_names[,1]),"_",rep(marker_names[i-1],length(unlist(new_names[,1]))))
  }  # Matthieu I have to find a tidy way to remove the first "NA_" that gets putted at the beginning of the name if there is not a genbank accession number


#rename sequences in each alignment with the final sequence name based on the new_names table
for (j in 1:length(lR)){
  align=names(lR[j])
  lR[[j]]=.rename.seqs(lR[[j]],table=data.frame(name = new_names[,colnames(new_names)==align],df[,2:ncol(df)]),align=align)
  lR[[j]]=lR[[j]][!is.na(names(lR[[j]]))]#this delete all sequences not present in the correspondence table
  if (unalign){lR[[j]] = ape::del.gaps(lR[[j]])}
  }

# clean the newname table to keep onyl the sequences name that are really present in the alignment
for (i in 2:ncol(new_names)){
  sequences_in_alignments = new_names[,i] %in% names(lR[[match(colnames(new_names)[i],names(lR))]])
  new_names[!sequences_in_alignments,i] = ""
}


#Create directory where to save outputs file

if(!is.null(out)){dir_name = out}
if(is.null(out)){dir_name = "renamed"}

# this part will check if a directory already exists and to avoid overwrite it append
#a progressive number to the name of the new folder
base_dir_name = dir_name
N = 0
while (dir.exists(dir_name)) {
  N = N+1
  dir_name = paste0(base_dir_name,"_",N)
}

dir.create(dir_name)

# save renamed alignments and update the names of the alignments in the new_names dataframe that will be saved as new correspondence table
for (i in 1:length(lR)){
 original_alignment_name = names(lR)[i]
 write.alignment(lR[[i]],name=paste0(dir_name,"/renamed_",stringr::str_remove(names(lR)[[i]],".fas.*.*")),format=format)
 colnames(new_names)[match(original_alignment_name,colnames(new_names))]=paste0("renamed_",stringr::str_remove(names(lR)[[i]],".fas.*.*"),".fasta")
}

#save new correspondence table
writexl::write_xlsx(new_names,paste0(dir_name,"/renamed_correspondence_table.xlsx"))

}
