con.fn.vec <- list.files('data/Paddock_Con_slow/',full.names = T,pattern = 'FluxCon_slow_flux')
library(data.table)

read.flux.slow.func <- function(fn.vec,nskip = 3){
  # 
  getData <- function(fn) {
    # h <- readLines(fn, n = 0)
    # h <- gsub(",([0-9])", "\\1", h)
    
    if(file.size(fn) < 1e4){
      return()
    }else{
      dat <- as.data.frame(fread(fn, skip = 0, header = F, 
                                 stringsAsFactors = FALSE, na.strings = c("NAN", "\"NAN\"")))
      # colnames <- gsub("\"", "", strsplit(paste(h[2], collapse = ""), 
      #                                     ",")[[1]])
      # names(dat) <- make.names(colnames)
      # names(dat)[1] <- "DateTime"
      
      dat.new <- dat[nskip:nrow(dat),]
      names(dat.new) <- paste0(dat[1,],'_',dat[2,])
      
      return(dat.new)
    }
    
   
    
  }
  
  # 
  tmp.ls <- lapply(fn.vec, getData)
  # tmp <-getData(con.fn.vec[1])
  
  return(do.call(rbind,tmp.ls))
  
}

flux.con.df <- read.flux.slow.func(con.fn.vec,nskip = 4)

flux.con.df$TIMESTAMP_TS <- strptime(flux.con.df$TIMESTAMP_TS,'%Y-%m-%d %H:%M:%S')

flux.con.df$TIMESTAMP_TS <- as.POSIXct(flux.con.df$TIMESTAMP_TS)
# 

for (i in c(3:9,42)){
  flux.con.df[,i] <- as.numeric(flux.con.df[,i])
}
flux.con.df$flag.agc <- 0
flux.con.df$flag.agc[flux.con.df$AGC_7500_Avg_ >=87] <- 1
flux.con.df$flag.agc <- as.factor(flux.con.df$flag.agc)
flux.con.df$flag.ustar <- 0
flux.con.df$flag.ustar[flux.con.df$`ustar_Avg_m/s` >=0.2] <- 1
flux.con.df$flag.ustar <- as.factor(flux.con.df$flag.ustar)

# 
saveRDS(flux.con.df,'cache/flux_con.rds')

# ############
iri.fn.vec <- list.files('data/Paddock_Irr_slow/',full.names = T,pattern = 'FluxIrr_slow_flux')
flux.iri.df <- read.flux.slow.func(con.fn.vec,nskip = 4)
flux.iri.df$TIMESTAMP_TS <- strptime(flux.iri.df$TIMESTAMP_TS,'%Y-%m-%d %H:%M:%S')

flux.iri.df$TIMESTAMP_TS <- as.POSIXct(flux.iri.df$TIMESTAMP_TS)
for (i in c(3:9,42)){
  flux.iri.df[,i] <- as.numeric(flux.iri.df[,i])
}
flux.iri.df$flag.agc <- 0
flux.iri.df$flag.agc[flux.iri.df$AGC_7500_Avg_ >=87] <- 1
flux.iri.df$flag.agc <- as.factor(flux.iri.df$flag.agc)
flux.iri.df$flag.ustar <- 0
flux.iri.df$flag.ustar[flux.iri.df$`ustar_Avg_m/s` >=0.2] <- 1
flux.iri.df$flag.ustar <- as.factor(flux.iri.df$flag.ustar)
# 
saveRDS(flux.iri.df,'cache/flux_iri.rds')


# 
# 

palette(c('red','black'))


# 
pdf('flux_con.pdf',width = 10,height = 10*.618)

par(mar=c(5,5,1,1),mfrow=c(2,1))
plot(`Fc_raw_Avg_mg/m^2/s`~TIMESTAMP_TS,ylim=c(-1,1),type='p',pch=16,
     data = flux.con.df[flux.con.df$flag.ustar == '1' & flux.con.df$flag.agc == '1',],
     xlab='',ylab=expression('C flux'~(mg~m^-2~s^-1)),xaxt='n',col=flag.ustar)
abline(h=0,col='grey',lwd=3)

plot(`Fe_raw_Avg_W/m^2`~TIMESTAMP_TS,ylim=c(0,500),type='p',pch=16,
     data = flux.con.df[flux.con.df$flag.ustar == '1' & flux.con.df$flag.agc == '1',],
     xlab='',ylab=expression('LE flux'~(W~m^-2)),xaxt='n',col=flag.ustar)
abline(h=0,col='grey',lwd=3)

date.range = range(flux.con.df$TIMESTAMP_TS,na.rm=T)
mons.vec =  seq(date.range[1],date.range[2],by='mon')

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
# mtext('2018',side = 1,adj=0,line = 3)
# mtext('2019',side = 1,adj=0.5,line = 3)
yr.vec <- unique(year(flux.con.df$TIMESTAMP_TS))
yr.vec <- yr.vec[!is.na(yr.vec)]
where.c <-which(mon.c =='01') / length(mon.c)
num.yr <- length(where.c)
mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)

plot(`Fe_raw_Avg_W/m^2`~TIMESTAMP_TS,data = flux.con.df[as.Date(flux.con.df$TIMESTAMP_TS) == as.Date('2020-1-20'),],
     type='p',pch=c(0,16)[flag.agc],col=flag.ustar,
     xlab='',ylab=expression('LE flux'~(W~m^-2)),main = '2020-1-20')

legend('topright',legend=c('ACG <88','U*<0.2','nomal'),pch=c(16,0,16),col=palette()[c(1,2,2)])

flag.df <- data.frame(n.obs =nrow(flux.con.df),
                      n.agc = nrow(flux.con.df[flux.con.df$flag.agc == '1',]),
                      n.ustar = nrow(flux.con.df[flux.con.df$flag.ustar == '1',]),
                      n.good = nrow(flux.con.df[flux.con.df$flag.ustar == '1' & flux.con.df$flag.agc == '1',])
                      )


barplot(as.matrix(flag.df),names.arg = c('No. Obs.','AGC > 88','U*>0.2','Both'))

dev.off()

