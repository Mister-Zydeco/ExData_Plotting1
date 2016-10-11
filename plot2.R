### This progrsm must be run from a directory/folder
### containing the 120Mb file household_power_consumption.txt

read_in_chunks<-function(fpath) {
  fh <- file(fpath, "r")
  keep <- readLines(fh, n=1)
  repeat {
    buf <- readLines(fh, 1000)
    #  End of file
    if (length(buf) == 0) {break;}
    # At February 3 2007, this and later chnunks no longer relevant
    if (grepl("^3/2/2007", buf[1])) { break;}
    
    matchers <- buf[grepl("^[12]/2/2007", buf)]
    keep <- c(keep, matchers)
  }
  close(fh)
  keep
}

make_tick_label<-function(rec) {
  dt_time <- paste(rec$Date, rec$Time)
  fmt_in <-"%d/%m/%Y %H:%M:%S"
  fmt_out <- "%a"
  strftime(as.Date(dt_time, format=fmt_in), fmt_out)
}

fname <- "household_power_consumption.txt";
ftext <- read_in_chunks(fname)
hpc <- read.csv(textConnection(ftext), sep=";", na.strings="?",
                stringsAsFactors=FALSE)


# Add one extra record at initial instant of Feb 3 2007 to get last
# x-axis tick mark labeled "Sat"
#
hpc_feb1_2 <- subset(hpc, Date =="1/2/2007" 
                     | Date =="2/2/2007")
hpc_feb1_2[2881,"Date"] <-"3/2/2007"
hpc_feb1_2[2881, "Time"] <- "00:00:00"

tick_indices <- c(1, 1441, 2881)
tick_recs <- hpc_feb1_2[tick_indices,]
tick_labels <- make_tick_label(tick_recs)



png("plot2.png", width=480, height=480)
par(xaxp=c(1,2881,2))
with(hpc_feb1_2, plot(Global_active_power, type="l", xaxt="n", xlab="",
                      ylab="Global Active Power (kilowatts)"))
axis(side=1, at=tick_indices, labels=tick_labels)
dev.off()

