copyLSN2temp <- function() {
  ## Create temporary .ssn directory to work with
  if(dir.exists(paste0(tempdir(),'/MiddleFork04.ssn'))) return()
  file.copy(from = system.file("lsndata/MiddleFork04.ssn",package = "SSN"), 
    to = tempdir(), recursive = TRUE,
    overwrite = FALSE, copy.mode = FALSE)
  if(.Platform$OS.type == 'unix') 
		system(paste0('chmod -R 777 ', tempdir(),'/MiddleFork04.ssn'))
}

