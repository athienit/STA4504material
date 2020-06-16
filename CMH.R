MIOC = array(c(4,62,2,224,
               9,33,12,390,
               4,26,33,330,
               6,9,65,362,
               6,5,93,301),
             dim=c(2,2,5),
             dimnames=list(
               Status=c("Case","Control"),
               OCuse=c("Yes","No"),
               Agegrp=c("1","2","3","4","5")))

MIOC

OR=function(matrix,adjust=TRUE){
  if(adjust==TRUE){mat=matrix+0.5}
  OR=(mat[1,1]*mat[2,2])/(mat[1,2]*mat[2,1])
  return(OR)
}
apply(MIOC,3,OR)

mantelhaen.test(MIOC)