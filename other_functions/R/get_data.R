library(fs)


download_data <- function(url = "https://files.osf.io/v1/resources/5wunp/providers/osfstorage/5d9cf6bef6b03e001019fffe?action=download&direct=&version=1"){
  # Download a zip of the files from the OSF repo 
  
  temp <- tempfile()
  download.file(url, destfile = temp, mode = "wb")
  
  return(temp)
}

unzip_data <- function(zipFile){
  # Unzip the files into a temporary directory
  
  tempDir <- tempdir()
  

  allFiles <- unzip(zipFile, list = T)
  # Only select the files in the Results dir
  dataFiles <- allFiles$Name[grep("/Results/", allFiles$Name)]
  
  unzip(zipFile, files = dataFiles, exdir = tempDir)
  return(tempDir)
}

move_data <- function(tempDir, rawDir = "./data/raw_data"){
  # Move the .mat files from the temp dir to the raw_data dir, creating the raw
  # data dir if it doesn't exist
  
  dataFiles <- list.files(path = tempDir, pattern = "*.mat$",
                          recursive = T, full.names = T)
  
  if (!dir.exists(rawDir)){
    dir.create(rawDir)
  }
  
  file_move(path = dataFiles, new_path = rawDir)
}
