 ; 
 ; **** Routine compiled from DATA-QWIK Procedure SCAENC ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  SCA Password Encryption|SCAENC|||||||1
SCAENC ; 
 ;
 S ENC=$$ENC(X)
 ;
 Q 
 ;
ENC(PWD) ; 
 ;
 I $E(PWD,1)=$char(1) Q $E(PWD,2,999)
 ;
 N ENC,I,L,SUM,XOR
 ;
 S PWD=$ZCONVERT(PWD,"U")
 S XOR=$$XOR^%ZFUNC(PWD)
 S L=$L(PWD)
 ;
 S SUM=0
 F I=1:1:L-1 S SUM=SUM+($A(PWD,I)*$A(PWD,I+1)*(I+1))
 S SUM=SUM+($A(PWD,L)*(L+1))+(XOR*L)
 S ENC=SUM#999999
 ;
 Q ENC
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^1091" ; Signature - LTD^TIME^USER^SIZE
