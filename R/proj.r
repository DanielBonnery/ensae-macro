proj<-function(Z,X){
  MASS::ginv((t(Z)%*%Z))%*%t(Z)%*%X
}