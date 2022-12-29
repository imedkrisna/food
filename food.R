library("ARDL")
library("tidyverse")
library("readxl")
library("sjPlot")

## Reading data

ibs<-read_excel("ibs.xlsx",sheet="IBS2")
mikro<-read_excel("mikrokecil.xlsx",sheet="mikro")
kecil<-read_excel("mikrokecil.xlsx",sheet="kecil")
pdb<-read_excel("pdb.xlsx",sheet="pdb")

## Running ARDL(1,1)

ibs1x<-ardl(ln~lintm,data=ibs,order = c(1,1))
ibs2x<-ardl(lnaker~lintm,data=ibs,order = c(1,1))
ibs3x<-ardl(loutput~lintm,data=ibs,order = c(1,1))
ibs4x<-ardl(lva~lintm,data=ibs,order = c(1,1))
ibs5x<-ardl(ltax~lintm,data=ibs,order = c(1,1))
ibs6x<-ardl(lw~lintm,data=ibs,order = c(1,1))
ibs7x<-ardl(lvan~lintm,data=ibs,order = c(1,1))
ibs8x<-ardl(lvana~lintm,data=ibs,order = c(1,1))

mikro1x<-ardl(ln~lintm,data=mikro,order = c(1,1))
mikro2x<-ardl(lnaker~lintm,data=mikro,order = c(1,1)) 
mikro3x<-ardl(loutput~lintm,data=mikro,order = c(1,1))
mikro4x<-ardl(lva~lintm,data=mikro,order = c(1,1))
mikro5x<-ardl(lvan~lintm,data=mikro,order = c(1,1))
mikro6x<-ardl(lvana~lintm,data=mikro,order = c(1,1))
mikro7x<-ardl(lw~lintm,data=mikro,order = c(1,1))

kecil1x<-ardl(ln~lintm,data=kecil,order = c(1,1))   
kecil2x<-ardl(lnaker~lintm,data=kecil,order = c(1,1))
kecil3x<-ardl(loutput~lintm,data=kecil,order = c(1,1))
kecil4x<-ardl(lva~lintm,data=kecil,order = c(1,1))
kecil5x<-ardl(lvan~lintm,data=kecil,order = c(1,1))
kecil6x<-ardl(lvana~lintm,data=kecil,order = c(1,1))
kecil7x<-ardl(lw~lintm,data=kecil,order = c(1,1))

## Making regression table

tab_model(mikro1x,file="mikro1x.html")
tab_model(mikro2x,file="mikro2x.html")
tab_model(mikro3x,file="mikro3x.html")
tab_model(mikro4x,file="mikro4x.html")
tab_model(mikro5x,file="mikro5x.html")
tab_model(mikro6x,file="mikro6x.html")
tab_model(mikro7x,file="mikro7x.html")

tab_model(kecil1x,file="kecil1x.html")
tab_model(kecil2x,file="kecil2x.html")
tab_model(kecil3x,file="kecil3x.html")
tab_model(kecil4x,file="kecil4x.html")
tab_model(kecil5x,file="kecil5x.html")
tab_model(kecil6x,file="kecil6x.html")
tab_model(kecil7x,file="kecil7x.html")

## Bound F test table

bm2<-bounds_f_test(mikro2x,case=2)
bm3<-bounds_f_test(mikro3x,case=2)
bm4<-bounds_f_test(mikro4x,case=2)
bm5<-bounds_f_test(mikro5x,case=2)
bm6<-bounds_f_test(mikro6x,case=2)
bm7<-bounds_f_test(mikro7x,case=2)
bk2<-bounds_f_test(kecil2x,case=2)
bk3<-bounds_f_test(kecil3x,case=2)
bk4<-bounds_f_test(kecil4x,case=2)
bk5<-bounds_f_test(kecil5x,case=2)
bk6<-bounds_f_test(kecil6x,case=2)
bk7<-bounds_f_test(kecil7x,case=2)

bm<-tibble(variable=c("log output","log value added","log value added per firm","log value added per person","log average wage"),
            stat=c(bm3$statistic,bm4$statistic,bm5$statistic,bm6$statistic,bm7$statistic),
       pval=c(bm3$p.value,bm4$p.value,bm5$p.value,bm6$p.value,bm7$p.value))
bk<-tibble(variable=c("log output","log value added","log value added per firm","log value added per person","log average wage"),
           stat=c(bk3$statistic,bk4$statistic,bk5$statistic,bk6$statistic,bk7$statistic),
           pval=c(bk3$p.value,bk4$p.value,bk5$p.value,bk6$p.value,bk7$p.value))
tab_df(bm,file="bound_mikro.html")
tab_df(bk,file="bound_kecil.html")
#pdb1<-ardl(lmamin~lintm,data=pdb,order=c(1,3))|>summary()
#pdb2<-ardl(lresto~lintm,data=pdb,order=c(1,3))|>summary()



#================= EXPERIMENT =========================================


# ibs1<-auto_ardl(ln~lintm+lintx,data=ibs,max_order = 3)         #122
# ibs2<-auto_ardl(lnaker~lintm+lintx,data=ibs,max_order = 3)     #100
# ibs3<-auto_ardl(loutput~lintm+lintx,data=ibs,max_order = 3)    #231
# ibs4<-auto_ardl(lva~lintm+lintx,data=ibs,max_order = 3)        #120
# ibs5<-auto_ardl(ltax~lintm+lintx,data=ibs,max_order = 3)       #111
# ibs6<-auto_ardl(lw~lintm+lintx,data=ibs,max_order = 3)         #333
# 
# ibs1s<-ardl(ln~lintm+lintx,data=ibs,order = c(1,2,2))|>summary()         #122
# ibs2s<-ardl(lnaker~lintm+lintx,data=ibs,order = c(1,0,0))|>summary()     #100
# ibs3s<-ardl(loutput~lintm+lintx,data=ibs,order = c(2,3,1))|>summary()    #231
# ibs4s<-ardl(lva~lintm+lintx,data=ibs,order = c(1,2,0))|>summary()        #120
# ibs5s<-ardl(ltax~lintm+lintx,data=ibs,order = c(1,1,1))|>summary()       #111
# ibs6s<-ardl(lw~lintm+lintx,data=ibs,order = c(3,3,3))|>summary()         #333
# 
# mikro1<-auto_ardl(ln~lintm,data=mikro,max_order = 2)         #10
# mikro2<-auto_ardl(lnaker~lintm,data=mikro,max_order = 2)     #11
# mikro3<-auto_ardl(loutput~lintm,data=mikro,max_order = 2)    #22
# mikro4<-auto_ardl(lva~lintm,data=mikro,max_order = 2)        #22
# mikro5<-auto_ardl(lvan~lintm,data=mikro,max_order = 2)        #22
# mikro6<-auto_ardl(lvana~lintm,data=mikro,max_order = 2)         #22
# mikro7<-auto_ardl(lw~lintm,data=mikro,max_order = 2)        #22
# 
# mikro1s<-ardl(ln~lintm,data=mikro,order = c(1,0))   |>summary()      #10
# mikro2s<-ardl(lnaker~lintm,data=mikro,order = c(1,1)) |>summary()    #11
# mikro3s<-ardl(loutput~lintm,data=mikro,order = c(2,2))|>summary()    #22
# mikro4s<-ardl(lva~lintm,data=mikro,order = c(2,2))|>summary()        #22
# mikro5s<-ardl(lvan~lintm,data=mikro,order = c(2,2))|>summary()        #22
# mikro6s<-ardl(lvana~lintm,data=mikro,order = c(2,2))|>summary()         #22
# mikro7s<-ardl(lw~lintm,data=mikro,order = c(2,2))|>summary()        #22
# 
# kecil1<-auto_ardl(ln~lintm,data=kecil,max_order = 2)         #22
# kecil2<-auto_ardl(lnaker~lintm,data=kecil,max_order = 2)     #11
# kecil3<-auto_ardl(loutput~lintm,data=kecil,max_order = 2)    #22
# kecil4<-auto_ardl(lva~lintm,data=kecil,max_order = 2)        #22
# kecil5<-auto_ardl(lvan~lintm,data=kecil,max_order = 2)        #22
# kecil6<-auto_ardl(lvana~lintm,data=kecil,max_order = 2)         #22
# kecil7<-auto_ardl(lw~lintm,data=kecil,max_order = 2)        #22
# 
# kecil1s<-ardl(ln~lintm,data=kecil,order = c(2,2))  |>summary()       #22
# kecil2s<-ardl(lnaker~lintm,data=kecil,order = c(1,1))|>summary()     #11
# kecil3s<-ardl(loutput~lintm,data=kecil,order = c(2,2))|>summary()    #22
# kecil4s<-ardl(lva~lintm,data=kecil,order = c(2,2))|>summary()        #22
# kecil5s<-ardl(lvan~lintm,data=kecil,order = c(2,2))|>summary()        #22
# kecil6s<-ardl(lvana~lintm,data=kecil,order = c(2,2))|>summary()         #22
# kecil7s<-ardl(lw~lintm,data=kecil,order = c(2,2))|>summary()        #22
# 
# pdb1<-auto_ardl(lmamin~lintm+lintx,data=pdb,max_order=3)
# pdb1s<-ardl(lmamin~lintm+lintx,data=pdb,order=c(2,2,3))|>summary()
# 
# pdb2<-auto_ardl(lresto~lintm+lintx,data=pdb,max_order=3)
# pdb2s<-ardl(lresto~lintm+lintx,data=pdb,order=c(1,0,0))|>summary()
# 
# ardl(loutput~lintm,data=ibs,order=c(1,3))|>summary()