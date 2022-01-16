library(R.matlab)


begin_mat_server <- function() {
  
  Matlab$startServer()
  
  conn <- Matlab()
  open(conn)
  
  return(conn)
} 

end_mat_server <- function(conn){
  close(conn)
}

run_mat_script <- function(conn, script){
  script_text <- readr::read_file(script)
  evaluate(conn, script_text)
}


# Now to write some code that grabs the data, extracts the events,
# and then saves them to an R readable format (csv is a good bet)

