## ---- prepdatadir ----
prep.data.directory <- function(data.path) {
  if (!file.exists(data.path)) {
    dir.create(data.path)
  }
}

## ---- downloadurl ----
downloadmyurl <- function(url.string, target.name, data.path, inhibit.download=FALSE) {
  prep.data.directory(data.path)
  target.path <- paste(data.path, target.name, sep = "/")
  if (!inhibit.download) {
    download.file(url.string, destfile = target.path, method="curl")
  }
  
  return (target.path)
}

