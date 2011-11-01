f=function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,x,y){
dt_a=a[,1]-b[,1]
dk_a=a[,2]-b[,2]
dt_c=c[,1]-d[,1]
dk_c=c[,2]-d[,2]
dt_e=e[,1]-f[,1]
dk_e=e[,2]-f[,2]
dt_g=g[,1]-h[,1]
dk_g=g[,2]-h[,2]
dt_i=i[,1]-j[,1]
dk_i=i[,2]-j[,2]
dt_k=k[,1]-l[,1]
dk_k=k[,2]-l[,2]
dt_m=m[,1]-n[,1]
dk_m=m[,2]-n[,2]
dt_o=o[,1]-p[,1]
dk_o=o[,2]-p[,2]
dt_r=r[,1]-s[,1]
dk_r=r[,2]-s[,2]
dt_t=t[,1]-u[,1]
dk_t=t[,2]-u[,2]
dt_x=x[,1]-y[,1]
dk_x=x[,2]-y[,2]

dtaskai = cbind(dt_a,dt_c,dt_e,dt_g,dt_i,dt_k,dt_m,dt_o,dt_r,dt_t,dt_x)
dklaidos = cbind(dk_a,dk_c,dk_e,dk_g,dk_i,dk_k,dk_m,dk_o,dk_r,dk_t,dk_x)

dtmean=c(1:40)
z=0
 while (z<=40) {
 dtmean[z] <- mean(dtaskai[z,])
 z=z+1
 }
dtmean

dkmean=c(1:40)
z=0
 while (z<=40) {
 dkmean[z] <- mean(dklaidos[z,])
 z=z+1
 }

par(mfrow=c(1,2))
plot(dtmean, col = 2, type = "l", xlab="Minutës", ylab="Pelnytø taðkø skirtimø vidurkiai", lty=2)
plot(dkmean, col = 2, type = "l", xlab="Minutës", ylab="Padarytø klaidu skirtumø vidurkiai", lty=2)

}