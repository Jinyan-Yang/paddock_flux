flux.con.df <- readRDS('cache/flux_con.rds')
# 
flux.iri.df <- readRDS('cache/flux_iri.rds')

flux.iri.df$Rnet <- flux.iri.df$`Fh_Avg_W/m^2` + flux.iri.df$`Fe_Avg_W/m^2`
flux.con.df$Rnet <- flux.con.df$`Fh_Avg_W/m^2` + flux.con.df$`Fe_Avg_W/m^2`
# 
plot.rn.func <- function(flux.iri.df,date.since = as.Date('2021-3-5')){
  
  df.plot <- flux.iri.df[!is.na(flux.iri.df$`Fh_Avg_W/m^2`),]
  
  
  plot(Rnet~TIMESTAMP_TS,data = df.plot[as.Date(df.plot$TIMESTAMP_TS) > date.since,],ylim=c(-100,500),type='l',
       xlab='',ylab='Energy Flux (W/m2)')
  points(`Fh_Avg_W/m^2`~TIMESTAMP_TS,data = df.plot[as.Date(df.plot$TIMESTAMP_TS) > date.since,],pch=16,col='red')
  points(`Fe_Avg_W/m^2`~TIMESTAMP_TS,data = df.plot[as.Date(df.plot$TIMESTAMP_TS) > date.since,],pch=16,col='blue')
  
  legend('top',legend = c('Rnet','Fh','Fe'),pch = c(NA,16,16),
         lty=c('solid',NA,NA),col=c('black','red','blue'))
}

# 

plot.rn.func(flux.con.df,date.since = as.Date('2020-11-24'))
title('Con-No Fh after 2020-12-2')

plot.rn.func(flux.iri.df)
title('Iri')
