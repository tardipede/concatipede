#' Extract GenBank accession number from correspondence table
#'
#' Extract GenBank accession number from correspondence table formatted with the same requirements for concatipede()
#'
#' @param df The user-defined correspondence table, as a data frame or equivalent. This is used only if no \code{filename} argument is provided.
#' @param filename Filename of input correspondence table. Alternatively, if no filename is provided, the user can provide their own correspondence table as the \code{df} argument.
#' @param writetable if TRUE save the Genbank table as excel file in the working directory
#' @param excel.sheet specify what sheet from the excel spreadsheet you wanna read. Either a string (the name of a sheet), or an integer (the position of the sheet).
#' @param out if writetable == T, the name to be attached to the excel filename
#' @return Table with GenBank accession numbers
#' @export

get_genbank_table = function(df = NULL,
                             filename = NULL,
                             writetable = FALSE,
                             out = "",
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
    } else {
      # Check: was `df` provided by the user?
      stopifnot(!is.null(df))
      # Forcing df to be a data frame (things do not work properly in the rest
      # of the function is df was given as a tibble and is not converted to a
      # data frame)
      df <- as.data.frame(df)}

  corr_table = df

  #remove all the dataframe columns before the "name" columns
  df = df[,which(colnames(df)=="name"):ncol(df)]

  g_table = data.frame(lapply(df,.extract_accnos))
  g_table$name = df$name

  if(writetable==T){writexl::write_xlsx(g_table,path=paste0(out,"Genbank_accnos.xlsx"),col_names=T,format_headers=T)}
  if(writetable==F){return(g_table)}
}
