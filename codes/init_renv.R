install.packages("renv")
renv::init()  # initializes renv, creates renv.lock
install.packages("gmapsdistance")  # or pacman::p_load()
renv::snapshot()  # records the state of your library
