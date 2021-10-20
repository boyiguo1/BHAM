# .onLoad <- function(){
#   requireNamespace("survival", quietly = TRUE)
# }

.onUnload <- function(libpath){
  library.dynam.unload("BHAM", libpath)
}
