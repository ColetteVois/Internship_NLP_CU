#This is the file where the descriptions of the input data are created.
#By default, "There is no description available for this method"
#If you want to add one, go to load.data.i and add one to description

n.type.data <- length(list.files(paste(my_path,"/Intership_NLP_CU-master/load_data/", sep="")))



load_data_type_description <- c()
for(j in 1:n.type.data){
  
  temp <- paste(my_path,sprintf("/Intership_NLP_CU-master/load_data/load_data_%d.R", j), sep="")
  myfiles <- lapply(temp, read.csv, sep="\n", fill = TRUE , header = FALSE,  col.names = "text", stringsAsFactors = FALSE)#fill = TRUE,header = FALSE,stringsAsFactors = FALSE)#, col.names = rep("text",length(listfiles)+1))
  
  description <- "There is no description available for this load data. "
  for(i in 1:nrow(myfiles[[1]])) {
    if(str_detect(myfiles[[1]][i,1], "@description")) {
      description1 <- myfiles[[1]][i,1]
      description2 <- str_replace(description1, "#' ", "")
      description3 <- str_replace(description2, "#'", "")
      description <- str_replace(description3, "@description ", "")
    }
  }
  
  load_data_type_description <- c(load_data_type_description,
                                  paste0(sprintf("Load data %d: ",j), description))
}