 ; 
 ; **** Routine compiled from DATA-QWIK Procedure DBSSCRTMP ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  Linked Screen Template|DBSLNKT|||||||1
DBSLNKT ; DBS - U - V7.0 - Multiple page screens base program
 ;
V0 ; 
V1 ; 
 S:'($D(%PG)#2) %PG=1 S %PGSV=%PG S MULSCR=""
 ;
VNEW ; Initailize new record
 Q 
 ;
VPG ; 
 ;
VPG0 ; 
 ;
VPAGE ; 
 ;
VBTM ; 
 I %ProcessMode=4,IO'=$P Q 
 D ^DBSCRT8A
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^877" ; Signature - LTD^TIME^USER^SIZE
