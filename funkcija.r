f=function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,x,y){
dt_a=a[,1]-b[,1]
dk_a=a[,2]-b[,2]
db_a=round((a[,4]/a[,3]-b[,4]/b[,3]),2)*100
dd_a=round((a[,6]/a[,5]-b[,6]/b[,5]),2)*100
dtr_a=round((a[,8]/a[,7]-b[,8]/b[,7]),2)*100
dp_a=a[,9]-b[,9]
dr_a=a[,10]-b[,10]
dt_c=c[,1]-d[,1]
dk_c=c[,2]-d[,2]
db_c=round((c[,4]/c[,3]-d[,4]/d[,3]),2)*100
dd_c=round((c[,6]/c[,5]-d[,6]/d[,5]),2)*100
dtr_c=round((c[,8]/c[,7]-d[,8]/d[,7]),2)*100
dp_c=c[,9]-d[,9]
dr_c=c[,10]-d[,10]
dt_e=e[,1]-f[,1]
dk_e=e[,2]-f[,2]
db_e=round((e[,4]/e[,3]-f[,4]/f[,3]),2)*100
dd_e=round((e[,6]/e[,5]-f[,6]/f[,5]),2)*100
dtr_e=round((e[,8]/e[,7]-f[,8]/f[,7]),2)*100
dp_e=e[,9]-f[,9]
dr_e=e[,10]-f[,10]
dt_g=g[,1]-h[,1]
dk_g=g[,2]-h[,2]
db_g=round((g[,4]/g[,3]-h[,4]/h[,3]),2)*100
dd_g=round((g[,6]/g[,5]-h[,6]/h[,5]),2)*100
dtr_g=round((g[,8]/g[,7]-h[,8]/h[,7]),2)*100
dp_g=g[,9]-h[,9]
dr_g=g[,10]-h[,10]
dt_i=i[,1]-j[,1]
dk_i=i[,2]-j[,2]
db_i=round((i[,4]/i[,3]-j[,4]/j[,3]),2)*100
dd_i=round((i[,6]/i[,5]-j[,6]/j[,5]),2)*100
dtr_i=round((i[,8]/i[,7]-j[,8]/j[,7]),2)*100
dp_i=i[,9]-j[,9]
dr_i=i[,10]-j[,10]
dt_k=k[,1]-l[,1]
dk_k=k[,2]-l[,2]
db_k=round((k[,4]/k[,3]-l[,4]/l[,3]),2)*100
dd_k=round((k[,6]/k[,5]-l[,6]/l[,5]),2)*100
dtr_k=round((k[,8]/k[,7]-l[,8]/l[,7]),2)*100
dp_k=k[,9]-l[,9]
dr_k=k[,10]-l[,10]
dt_m=m[,1]-n[,1]
dk_m=m[,2]-n[,2]
db_m=round((m[,4]/m[,3]-n[,4]/n[,3]),2)*100
dd_m=round((m[,6]/m[,5]-n[,6]/n[,5]),2)*100
dtr_m=round((m[,8]/m[,7]-n[,8]/n[,7]),2)*100
dp_m=m[,9]-n[,9]
dr_m=m[,10]-n[,10]
dt_o=o[,1]-p[,1]
dk_o=o[,2]-p[,2]
db_o=round((o[,4]/o[,3]-p[,4]/p[,3]),2)*100
dd_o=round((o[,6]/o[,5]-p[,6]/p[,5]),2)*100
dtr_o=round((o[,8]/o[,7]-p[,8]/p[,7]),2)*100
dp_o=o[,9]-p[,9]
dr_o=o[,10]-p[,10]
dt_r=r[,1]-s[,1]
dk_r=r[,2]-s[,2]
db_r=round((r[,4]/r[,3]-s[,4]/s[,3]),2)*100
dd_r=round((r[,6]/r[,5]-s[,6]/s[,5]),2)*100
dtr_r=round((r[,8]/r[,7]-s[,8]/s[,7]),2)*100
dp_r=r[,9]-s[,9]
dr_r=r[,10]-s[,10]
dt_t=t[,1]-u[,1]
dk_t=t[,2]-u[,2]
db_t=round((t[,4]/t[,3]-u[,4]/u[,3]),2)*100
dd_t=round((t[,6]/t[,5]-u[,6]/u[,5]),2)*100
dtr_t=round((t[,8]/t[,7]-u[,8]/u[,7]),2)*100
dp_t=t[,9]-u[,9]
dr_t=t[,10]-u[,10]
dt_x=x[,1]-y[,1]
dk_x=x[,2]-y[,2]
db_x=round((x[,4]/x[,3]-y[,4]/y[,3]),2)*100
dd_x=round((x[,6]/x[,5]-y[,6]/y[,5]),2)*100
dtr_x=round((x[,8]/x[,7]-y[,8]/y[,7]),2)*100
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