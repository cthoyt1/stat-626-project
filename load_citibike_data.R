library(xml2)
library(stringr)

url <- "s3.amazonaws.com/tripdata/"
download_xml(url,'tripdata_file_xml.xml')

xml_raw <- read_xml('tripdata_file_xml.xml')
xml_list <- as_list(xml_raw)

contents_true <- names(xml_list$ListBucketResult) == "Contents"
bucket_results <- xml_list$ListBucketResult[contents_true]
node_count <- length(bucket_results)
url_list <- rep("",node_count)

# if (!dir.exists('raw_data')){
#   dir.create('raw_data')
# } 
existing_files <- list.files('raw_data')

for (i in 1:node_count){
  file_name <- bucket_results[i]$Contents$Key[[1]]
  print(file_name)
  url_list[i] <- file_name
  if (file_name != "index.html" && !(file_name %in% existing_files)){
    remote_path <- paste0("s3.amazonaws.com/tripdata/",file_name)
    local_path <- paste0("raw_data/",file_name)
    download.file(remote_path,local_path)
  }
}
