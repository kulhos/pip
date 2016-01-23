 ; 
 ; **** Routine compiled from DATA-QWIK Procedure FILEUTL ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  Filesystem utilities|FILEUTL||||rpcvar|rpcvar1||1
 ;
SIZE(file) ; 
 ;
 I ($get(file)="") Q 0
 I '$$EXISTS(file) Q 0
 ;
 N LSCMD
 N output
 N error S error=0
 ;
 ;     #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 ;*** Start of code by-passed by compiler
 if $zv["Linux" set LSCMD="stat -c %s "_file
 else  if $zv["AIX" set LSCMD="du -k "_file
 else  set LSCMD="wc -c "_file
 open "size":(shell="/bin/sh":command=LSCMD:READONLY)::"pipe"
 use "size" read output:30 if '$T set error=1
 close "size"
 ;*** End of code by-passed by compiler ***
 ;
 I error S $ZE="0,"_$ZPOS_","_"DEX-E-SIZERR, Error reading file size for "_file,$EC=",U1001,"
 N size S size=+$piece($$vStrTrim(output,0," ")," ",1)
 ;
 ;     #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 ;*** Start of code by-passed by compiler
 if $zv["AIX" set size=1024*size
 ;*** End of code by-passed by compiler ***
 ;
 Q size
 ;
EXISTS(file) ; 
 ;
 I ($get(file)="") Q 0
 ;
 N readcheck S readcheck=$$vClVobj($ST,"IO")
 N isthere
 ;
 D
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch1^"_$T(+0)
 .	;
 .	;      #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 .	S $P(vobj(readcheck,1),"|",2)=$$PARSE^%ZFUNC(file,"DIRECTORY")
 .	;      #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 .	S $P(vobj(readcheck,1),"|",1)=$$PARSE^%ZFUNC(file,"NAME")_$$PARSE^%ZFUNC(file,"TYPE")
 .	S $P(vobj(readcheck,1),"|",3)="READ"
 .	S $P(vobj(readcheck,1),"|",4)=5
 .	D open^UCIO(readcheck,$T(+0),"EXISTS","readcheck")
 .	D close^UCIO(readcheck) ; file exists
 .	S isthere=1
 .	Q 
 ;
 K vobj(+$G(readcheck)) Q isthere
 ;
SRCDIR(typ,topdir) ; 
 ;
 N types
 ;         #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 D TYPEINIT^TBXINST(.types)
 ;
 N det S det=$get(types(typ))
 I (det="") Q ""
 ;
 ;      #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 I '($get(topdir)="") Q $$SUBDIR^%TRNLNM(topdir,$piece(det,"|",1))
 ;
 ;         #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 Q $$SCAU^%TRNLNM("DQDIR",$piece(det,"|",1))
 ;
SRCFILE(typ,elem) ; get PSL source file location
 ;
 N types
 ;         #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 D TYPEINIT^TBXINST(.types)
 ;
 N det S det=$get(types(typ))
 I (det="") Q ""
 ;
 ;         #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 Q $$SCAU^%TRNLNM("DQDIR",$$SUBDIR^%TRNLNM($piece(det,"|",1),$translate(elem,"%","_")_"."_typ))
 ;
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^2906" ; Signature - LTD^TIME^USER^SIZE
 ; ----------------
 ;  #OPTION ResultClass 1
vStrTrim(object,p1,p2) ; String.trim
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 I p1'<0 S object=$$RTCHR^%ZFUNC(object,p2)
 I p1'>0 F  Q:$E(object,1)'=p2  S object=$E(object,2,1048575)
 Q object
 ;
vClVobj(vSt,vCls) ; Create a new object
 ;
 N vOid
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)=vCls_$C(9)_vSt
 Q vOid
 ;
vCatch1 ; Error trap
 ;
 N error,$ET,$ES S error=$ZE,$EC="",$ET="Q",$ZE=""
 I ($D(readcheck)#2) D close^UCIO(readcheck)
 ;          #ACCEPT Date=10/15/2013; PGM=JK; CR=4
 CLOSE file ; just for sure
 S isthere=0
 D ZX^UCGMR(voxMrk) Q 
