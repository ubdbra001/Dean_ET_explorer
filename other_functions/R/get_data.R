library(fs)


download_data <- function(url = "https://files.osf.io/v1/resources/5wunp/providers/osfstorage/5d9cf6bef6b03e001019fffe?action=download&direct=&version=1"){
  
  temp <- tempfile()
  download.file(url, destfile = temp, mode = "wb")
  
  return(temp)
}

unzip_data <- function(zipFile){
  
  tempDir <- tempdir()
  
  allFiles <- unzip(zipFile, list = T)
  dataFiles <- allFiles$Name[grep("/Results/", allFiles$Name)]
  
  unzip(zipFile, files = dataFiles, exdir = tempDir)
  return(tempDir)
}

move_data <- function(tempDir, rawDir = "./data/raw_data"){
  dataFiles <- list.files(path = tempDir, pattern = "*.mat$",
                          recursive = T, full.names = T)
  
  if (!dir.exists(rawDir)){
    dir.create(rawDir)
  }
  
  file_move(path = dataFiles, new_path = rawDir)
}
