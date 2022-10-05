
fix_text <- function(data) {
  a = c(" ", "-", "á","é","í","ó","ú")
  b= c("_", "", "a", "e", "i", "o", "u")
  data = tolower(data)
  data =  mgsub::mgsub(data, a,b)
  return(data)
}