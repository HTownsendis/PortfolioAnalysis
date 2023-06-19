# little function to dynamically create data object names
# string <- x
execute.string <- function(string) {
  write(string, 'tmp.txt')
  out <- source('tmp.txt')
  unlink('tmp.txt')
  return(out$value)
}


## Note: 24-FEB-2022, I just discovered that   eval(parse(text="tmp.txt"))
## does the same ting as this little function
## "Life is a gradual release from ignorance" - Hunter S. Thompson