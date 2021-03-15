#' Extract GenBank accession number from correspondence table
#'
#' Extract GenBank accession number from correspondence table formatted with the same requirements for concatipede()
#'
#' @param corr_table a correlation table. Can be both a dataframe or a character string indicating the file path o the correspondence table in excel format
#' @param writetable if TRUE save the Genbank table as excel file in the working directory
#' @param out if writetable == T, the name to be attached to the excel filename
#' @return Table with GenBank accession numbers
#' @export

get_genbank_table = function(corr_table, writetable = F, out = ""){

  if (is.character(corr_table)){
    corr_table=readxl::read_xlsx(path=corr_table, col_names=TRUE)
    colnames(corr_table)=unlist(lapply(colnames(corr_table),.colname.clean))
    corr_table=as.data.frame(apply(corr_table,2,.clean.NA))
  }

  g_table = data.frame(lapply(corr_table,.extract_accnos))
  g_table$name = corr_table$name

  if(writetable==T){writexl::write_xlsx(g_table,path=paste0(out,"Genbank_accnos.xlsx"),col_names=T,format_headers=T)}
  if(writetable==F){return(g_table)}
}
