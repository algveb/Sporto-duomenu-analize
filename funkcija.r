f=function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,x,y){

dt_a=a[,1]-b[,1]
dk_a=a[,2]-b[,2]
db_a=q(a)[,1]-q(b)[,1]
dd_a=q(a)[,2]-q(b)[,2]
dtr_a=q(a)[,3]-q(b)[,3]
dp_a=a[,9]-b[,9]
dr_a=a[,10]-b[,10]
dt_c=c[,1]-d[,1]
dk_c=c[,2]-d[,2]
db_c=q(c)[,1]-q(d)[,1]
dd_c=q(c)[,2]-q(d)[,2]
dtr_c=q(c)[,3]-q(d)[,3]
dp_c=c[,9]-d[,9]
dr_c=c[,10]-d[,10]
dt_e=e[,1]-f[,1]
dk_e=e[,2]-f[,2]
db_e=q(e)[,1]-q(f)[,1]
dd_e=q(e)[,2]-q(f)[,2]
dtr_e=q(e)[,3]-q(f)[,3]
dp_e=e[,9]-f[,9]
dr_e=e[,10]-f[,10]
dt_g=g[,1]-h[,1]
dk_g=g[,2]-h[,2]
db_g=q(g)[,1]-q(h)[,1]
dd_g=q(g)[,2]-q(h)[,2]
dtr_g=q(g)[,3]-q(h)[,3]
dp_g=g[,9]-h[,9]
dr_g=g[,10]-h[,10]
dt_i=i[,1]-j[,1]
dk_i=i[,2]-j[,2]
db_i=q(i)[,1]-q(j)[,1]
dd_i=q(i)[,2]-q(j)[,2]
dtr_i=q(i)[,3]-q(j)[,3]
dp_i=i[,9]-j[,9]
dr_i=i[,10]-j[,10]
dt_k=k[,1]-l[,1]
dk_k=k[,2]-l[,2]
db_k=q(k)[,1]-q(l)[,1]
dd_k=q(k)[,2]-q(l)[,2]
dtr_k=q(k)[,3]-q(l)[,3]
dp_k=k[,9]-l[,9]
dr_k=k[,10]-l[,10]
dt_m=m[,1]-n[,1]
dk_m=m[,2]-n[,2]
db_m=q(m)[,1]-q(n)[,1]
dd_m=q(m)[,2]-q(n)[,2]
dtr_m=q(m)[,3]-q(n)[,3]
dp_m=m[,9]-n[,9]
dr_m=m[,10]-n[,10]
dt_o=o[,1]-p[,1]
dk_o=o[,2]-p[,2]
db_o=q(o)[,1]-q(p)[,1]
dd_o=q(o)[,2]-q(p)[,2]
dtr_o=q(o)[,3]-q(p)[,3]
dp_o=o[,9]-p[,9]
dr_o=o[,10]-p[,10]
dt_r=r[,1]-s[,1]
dk_r=r[,2]-s[,2]
db_r=q(r)[,1]-q(s)[,1]
dd_r=q(r)[,2]-q(s)[,2]
dtr_r=q(r)[,3]-q(s)[,3]
dp_r=r[,9]-s[,9]
dr_r=r[,10]-s[,10]
dt_t=t[,1]-u[,1]
dk_t=t[,2]-u[,2]
db_t=q(t)[,1]-q(u)[,1]
dd_t=q(t)[,2]-q(u)[,2]
dtr_t=q(t)[,3]-q(u)[,3]
dp_t=t[,9]-u[,9]
dr_t=t[,10]-u[,10]
dt_x=x[,1]-y[,1]
dk_x=x[,2]-y[,2]
db_x=q(x)[,1]-q(y)[,1]
dd_x=q(x)[,2]-q(y)[,2]
dtr_x=q(x)[,3]-q(y)[,3]
dp_x=x[,9]-y[,9]
dr_x=x[,10]-y[,10]

dtaskai = cbind(dt_a,dt_c,dt_e,dt_g,dt_i,dt_k,dt_m,dt_o,dt_r,dt_t,dt_x)
dklaidos = cbind(dk_a,dk_c,dk_e,dk_g,dk_i,dk_k,dk_m,dk_o,dk_r,dk_t,dk_x)
dbaudos = cbind(db_a,db_c,db_e,db_g,db_i,db_k,db_m,db_o,db_r,db_t,db_x)
ddvitaskiai = cbind(dd_a,dd_c,dd_e,dd_g,dd_i,dd_k,dd_m,dd_o,dd_r,dd_t,dd_x)
dtritaskiai = cbind(dtr_a,dtr_c,dtr_e,dtr_g,dtr_i,dtr_k,dtr_m,dtr_o,dtr_r,dtr_t,dtr_x)
dprazangos = cbind(dp_a,dp_c,dp_e,dp_g,dp_i,dp_k,dp_m,dp_o,dp_r,dp_t,dp_x)
datkovoti = cbind(dr_a,dr_c,dr_e,dr_g,dr_i,dr_k,dr_m,dr_o,dr_r,dr_t,dr_x)

dtmean=c(1:40)
z=0
 while (z<=40) {
 dtmean[z] <- mean(dtaskai[z,])
 z=z+1
 }

dkmean=c(1:40)
z=0
 while (z<=40) {
 dkmean[z] <- mean(dklaidos[z,])
 z=z+1
 }

dbmean=c(1:40)
z=0
 while (z<=40) {
 dbmean[z] <- mean(dbaudos[z,])
 z=z+1
 }

ddmean=c(1:40)
z=0
 while (z<=40) {
 ddmean[z] <- mean(ddvitaskiai[z,])
 z=z+1
 }

dtrmean=c(1:40)
z=0
 while (z<=40) {
 dtrmean[z] <- mean(dtritaskiai[z,])
 z=z+1
 }

dpmean=c(1:40)
z=0
 while (z<=40) {
 dpmean[z] <- mean(dprazangos[z,])
 z=z+1
 }

damean=c(1:40)
z=0
 while (z<=40) {
 damean[z] <- mean(datkovoti[z,])
 z=z+1
 }

par(mfrow=c(1,2))
plot(dtmean, col = 2, type = "l", xlab="Minutës", ylab="Pelnytø taðkø skirtimø vidurkiai", lty=2)
plot(dkmean, col = 2, type = "l", xlab="Minutës", ylab="Padarytø klaidu skirtumø vidurkiai", lty=2)

return=data.frame(cbind(dtmean,dkmean,dbmean,ddmean,dtrmean,dpmean,damean))

}



q=function(a)
{
k=ts(as.matrix(data.frame(cbind("1%"=rep(0,40),"2%"=rep(0,40),"3%"=rep(0,40)))))
i=0
for (i in 1:40)
{
if (a[i,3]==0) {k[i,1]=0}
 else {k[i,1]=(a[i,4]/a[i,3])*100}
}

for (i in 1:40)
{
if (a[i,5]==0) {k[i,2]=0}
 else {k[i,2]=(a[i,6]/a[i,5])*100}
}

for (i in 1:40)
{
if (a[i,7]==0) {k[i,3]=0}
 else {k[i,3]=(a[i,8]/a[i,7])*100}
}


return <- k
}