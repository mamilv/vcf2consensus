manip <- function()
{
  #setwd("C:/Users/pcs/Desktop/Harith_Req") ##Chnge this acoording to your convinience (where you save all the input files)

  ##loading all the input files - Start
  library(readr)
  df_barcode <- read_delim("barcode.vcf",
                           "\t", escape_double = FALSE,
                           trim_ws = TRUE)

  df_barcode <- df_barcode[order(-df_barcode$POS),]

  df_reference <- read_csv("reference.fa")

  seq_start_end <- read_table2("seq_start_end.txt")
  ##loading all the input files - end

  df_barcode <- df_barcode[df_barcode$QUAL >= seq_start_end$qual,]

  ##delete code(marking with 0) - Start
  total_string <<- df_reference[,1]

  for(i in df_barcode$POS )
  {
    total_string <<- paste(substr(total_string,1,(i-seq_start_end$start)),strrep(0, nchar(df_barcode$REF[df_barcode$POS == i]) ),
                           substr(total_string,(i-seq_start_end$start+1+nchar(df_barcode$REF[df_barcode$POS == i])),
                                  nchar(total_string)),sep = "")

  }
  ##delete code(marking with 0) - end

  ##adding code - start
  for(i in df_barcode$POS)
  {

    total_string <<- paste(substr(total_string,1,(i-seq_start_end$start)),df_barcode$ALT[df_barcode$POS==i],
                           substr(total_string,(i-seq_start_end$start+1),nchar(total_string)),sep = "")

  }
  ##adding code - end

  ##delete code - removing inserted 0s - Start
  total_string <<- gsub(pattern = "0",replacement = "",x = total_string)
  ##delete code - removing inserted 0s - end

  #writing data to out[ut text file - start
  fileConn<-file("MutatedFile.fa") # you can change your output file name here
  writeLines(c(colnames(df_reference),total_string), fileConn)
  close(fileConn)
  #writing data to output text file - end


}
