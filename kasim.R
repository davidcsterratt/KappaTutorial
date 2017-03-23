options(kasim="c:/users/my_user_name/Documents/KappaBin_master/KappaBin/bin/KaSim.exe")

run.kasim <- function(files, l=1, p=0.01, u="time", cmd=getOption("kasim"),
                      outfile="data.csv") {
  unlink(outfile, force=TRUE)
  system(paste(cmd, paste("-i", files, collapse=" "),
               "-l", l, "-p", p, "-u", u, "-o", outfile))
  return(read.kasim(outfile))
}

read.kasim <- function(file="data.csv") {
  dat <- read.csv(file, skip=2)
  header <- readLines(file, n=3)
  header <- gsub("# ", "", header)
  header <- gsub("'", "", header)
  cmd <- header[1]
  names <- strsplit(header[3], ", ")[[1]]
  names <- gsub("\"", "", names)
  colnames(dat) <- names
  attr(dat, "cmd") <- cmd
  class(dat) <- c("kasim", class(dat))
  return(dat)
}

plot.kasim <- function(dat, xlab="Time", ylab="Quantity", yscale=1) {
  cols <- 1:(ncol(dat) - 1)
  matplot(dat[,1], dat[,-1]*yscale, type="s", xlab=xlab, ylab=ylab, lty=1,
          col=cols, main=attr(dat, "cmd"))
  legend("topright", colnames(dat)[-1], fill=cols)
}
