 ; 
 ; **** Routine compiled from DATA-QWIK Procedure DBSPROC ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  DATA-QWIK Procedure Definition|DBSPROC|||||||1
DBSPROC ; Procedure definition
 ; I18N=QUIT
 ;
 Q 
 ;
EXT(%ProcessMode) ; 
 N vTp
 ;
 N SEQ N vER
 N DBTBL N filstr N KEYS N PROCID N TAB N VFMQ
 ;
 D ^DBSDEUTL(%ProcessMode,"DBTBL25")
 ;
 Q:VFMQ="Q" 
 ;
 S PROCID=$get(KEYS(2)) ; KEYS array defined in DBSDEUTL.
 ;
 I (PROCID="") Q 
 ;
 S DBTBL("SYSDEV",25,PROCID)="" ; Prevent warning on lock
 L +DBTBL("SYSDEV",25,PROCID):2
 E  D  Q 
 .	S ER=1
 .	S RM=$$^MSG(7354,"Procedure")
 .	Q 
 ;
 I %ProcessMode=3 D  Q  ; Delete old definition
 .	D vDbDe1()
 .  ZWI ^DBTBL("SYSDEV",25,PROCID)
 .	;   #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	N x S x=$$DELETE^%OSSCRPT($$SRCFILE^FILEUTL("PROC",PROCID))
 .	;
 .	; remove compiled code
 .	; check SYSMAP references, schedule for recompile
 .	;
 .	L -DBTBL("SYSDEV",25,PROCID)
 .	Q 
 ;
 N DQFILE S DQFILE=$$SRCFILE^FILEUTL("PROC",PROCID)
 N ds,vos1,vos2,vos3,vos4 S ds=$$vOpen1()
 ;
 I '$G(vos1) D  ; New procedure
 .	;
 .	;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	I $$EXISTS^FILEUTL(DQFILE),$$SIZE^FILEUTL(DQFILE)>0 Q  ; use existing file
 .	;
 .	N io S io=$$vClVobj($ST,"IO")
 .	;   #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	S $P(vobj(io,1),"|",2)=$$PARSE^%ZFUNC(DQFILE,"DIRECTORY")
 .	;   #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	S $P(vobj(io,1),"|",1)=$$PARSE^%ZFUNC(DQFILE,"NAME")_$$PARSE^%ZFUNC(DQFILE,"TYPE")
 .	S $P(vobj(io,1),"|",3)="WRITE/NEWV"
 .	;
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch1^"_$T(+0)
 .	;
 .	D open^UCIO(io,$T(+0),"EXT","io")
 .	;
 .	N proc S proc=$$vRCgetRecord0Opt^RecordDBTBL25("SYSDEV",PROCID,0,"")
 .	D write^UCIO(io,"//DO NOT MODIFY  "_$P(proc,$C(124),1)_"|"_$P(proc,$C(124),2)_"||||"_$P(proc,$C(124),6)_"|"_$P(proc,$C(124),7))
 .	;
 .	S TAB=$char(9)
 .	;
 .	;Header information
 .	D write^UCIO(io,TAB_"/*") ; All is in PSL format
 .	D write^UCIO(io,TAB_" ORIG: "_%UserID_" - "_%SystemDate) ; Developer , date
 .	D write^UCIO(io,TAB_" DESC: ") ; Description
 .	D write^UCIO(io,TAB)
 .	D write^UCIO(io,TAB_" ---- Comments --------------------------------------------------------")
 .	D write^UCIO(io,TAB)
 .	D write^UCIO(io,TAB_" ---- Revision History ------------------------------------------------")
 .	D write^UCIO(io,TAB)
 .	D write^UCIO(io,TAB_" ****** Consider using setAuditFlag for all objects in this procedure")
 .	D write^UCIO(io,TAB_"   example :do dep.setAuditFlag(1)")
 .	D write^UCIO(io,TAB)
 .	D write^UCIO(io,TAB_"*/")
 .	D close^UCIO(io)
 . K vobj(+$G(io)) Q 
 ;
 ; Get editor option
 N EDTOPT S EDTOPT="" ; Default
 ;
 I '($get(%UserID)="") D
 .	;
 .	N scau S scau=$G(^SCAU(1,%UserID))
 .	S EDTOPT=$P(scau,$C(124),21)
 .	I (EDTOPT="DBS") S EDTOPT=""
 . Q 
 ;
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 N updated S updated='$$EDTOPT^%OSSCRPT(EDTOPT,DQFILE)
 ;
 ; Restore auto-wrap
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 USE 0 WRITE $$SCRAWON^%TRMVT
 ;
 I 'updated D  Q  ; <F11> exit
 .	L -DBTBL("SYSDEV",25,PROCID)
 .	I %ProcessMode=1 S RM=$$^MSG(6710,PROCID) ; Not Modified
 .	Q 
 ;
 ; Update audit information and the Time
 N dbtbl25 S dbtbl25=$$vRCgetRecord1^RecordDBTBL25("SYSDEV",PROCID,0)
  S $P(vobj(dbtbl25),$C(124),4)=%UserID
  S $P(vobj(dbtbl25),$C(124),5)=%CurrentTime
 ;
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 I $$EXISTS^FILEUTL(DQFILE),$$SIZE^FILEUTL(DQFILE)>0 D
 .	;
 .	N io S io=$$vClVobj($ST,"IO")
 .	;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	S $P(vobj(io,1),"|",2)=$$PARSE^%ZFUNC(DQFILE,"DIRECTORY")
 .	;
 .	;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	S $P(vobj(io,1),"|",1)=$$PARSE^%ZFUNC(DQFILE,"NAME")_$$PARSE^%ZFUNC(DQFILE,"TYPE")
 .	S $P(vobj(io,1),"|",3)="READ"
 .	;
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch2^"_$T(+0)
 .	;
 .	D open^UCIO(io,$T(+0),"EXT","io")
 .	;
 .	N hdr S hdr=$$read^UCIO(io)
 .	D close^UCIO(io)
 .	;
 .	I '(hdr["//DO NOT MODIFY") K vobj(+$G(io)) Q 
 .	;
 .  S $P(vobj(dbtbl25),$C(124),2)=$piece(hdr,"|",2)
 .  S $P(vobj(dbtbl25),$C(124),6)=$piece(hdr,"|",6)
 .  S $P(vobj(dbtbl25),$C(124),7)=$piece(hdr,"|",7)
 .	;
 .	K vobj(+$G(io)) Q 
 ;
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL25(dbtbl25,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl25,-100) S vobj(dbtbl25,-2)=1 TC:vTp  
 ;
 L -DBTBL("SYSDEV",25,PROCID)
 ;
 K vobj(+$G(dbtbl25)) Q 
 ;
PP ; Post processor to check duplicate name
 ;
 I (X="") Q 
 ;
 I ($D(^DBTBL("SYSDEV",25,X))#2) D
 .	S ER=1
 .	S RM=$$^MSG(253)
 .	Q 
 ;
 Q 
 ;
COPY ; Copy definition (Called by function DBSPROCCO)
 N vTp
 ;
 N %FRAME N SEQ
 N PROCIDF N PROCIDT N %TAB N %READ N VFMQ
 ;
 S %TAB("PROCIDF")="/DES=From Procedure Name/LE=12/TYP=U/TBL=[DBTBL25]PROCID"
 S %TAB("PROCIDT")="/DES=To Procedure Name/LE=12/TYP=U/TBL=[DBTBL25]PROCID:NOVAL/XPP=D PP^DBSPROC"
 S %READ="@@%FN,,PROCIDF/REQ,PROCIDT/REQ,"
 S %FRAME=2
 ;
 D ^UTLREAD
 ;
 Q:VFMQ="Q" 
 ;
 N dbtbl25f S dbtbl25f=$$vRCgetRecord0^RecordDBTBL25("SYSDEV",PROCIDF,0)
 N dbtbl25t S dbtbl25t=$$vReCp1(dbtbl25f) ; Copy header
 ;
  S vobj(dbtbl25t,-4)=PROCIDT
  S $P(vobj(dbtbl25t),$C(124),2)="" ; Remove Old name
 S vobj(dbtbl25t,-2)=0
 ;
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL25(dbtbl25t,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl25t,-100) S vobj(dbtbl25t,-2)=1 TC:vTp  
 ;
 ; TODO check existence of PROCIDF
 ;
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 N tmp S tmp=$$COPYFIL^%OSSCRPT($$SRCFILE^FILEUTL("PROC",PROCIDF),$$SRCFILE^FILEUTL("PROC",PROCIDT))
 ;
 N ds,vos1,vos2,vos3,vos4 S ds=$$vOpen2()
 ;
 ; Copy detail
 F  Q:'$$vFetch2()  D
 . N dfrom,vop1 S vop1=$P(ds,$C(9),3),dfrom=$$vRCgetRecord1Opt^RecordDBTBL25D($P(ds,$C(9),1),$P(ds,$C(9),2),vop1,1,"")
 .	S SEQ=vop1
 .	;
 .	I SEQ=1 D
 ..		I ($P(dfrom,$C(12),1)["//DO NOT MODIFY") D  Q 
 ...			; TODO set new header and quit
 ...			Q 
 ..		; TODO set new header and add dfrom first line
 ..		Q 
 .	;
 .	N dto S dto=$$vcdmNew^RecordDBTBL25D() S vobj(dto,-3)="SYSDEV" S vobj(dto,-4)=PROCIDT S vobj(dto,-5)=SEQ
 .  S $P(vobj(dto),$C(12),1)=$P(dfrom,$C(12),1)
 .	;
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL25D(dto,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dto,-100) S vobj(dto,-2)=1 TC:vTp  
 . K vobj(+$G(dto)) Q 
 ;
 K vobj(+$G(dbtbl25f)),vobj(+$G(dbtbl25t)) Q 
 ;
BUILDALL ; Build all procedure routines (called) by FILER.COM)
 ;
 N rs,vos1,vos2,vos3 S rs=$$vOpen3()
 F  Q:'$$vFetch3()  D COMPILE(rs)
 ;
 Q 
 ;
BUILD ; Build run-time routine (Called by function DBSPROCB)
 ;
 N CNT
 ;
  N V1 S V1=%ProcessID D vDbDe2()
 ;
 S CNT=$$LIST^DBSGETID("DBTBL25") ; Select procedure ID(s)
 Q:(+CNT=0) 
 ;
 N ds,vos1,vos2,vos3,vos4  N V2 S V2=%ProcessID S ds=$$vOpen4()
 F  Q:'$$vFetch4()  D
 . N tmpdq,vop1 S vop1=$P(ds,$C(9),2),tmpdq=$$vRCgetRecord1Opt^RecordTMPDQ($P(ds,$C(9),1),vop1,1,"")
 .	D COMPILE(vop1)
 . Q 
 ;
  N V3 S V3=%ProcessID D vDbDe3()
 ;
 ; "Press any key" message and pause
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 WRITE $$MSG^%TRMVT("",,1)
 ;
 Q 
 ;
COMPILE(PROCID,PGM) ; Generated program name /NOREQ/MECH=REFNAM:W
 N vTp
 ;
 N LTD
 N count N FCOUNT N SIZE
 N %LIBS N cmpType N code N m2src N SIG N TIME N TPGM N USER
 ;
 S ER=0
 S RM=""
 S count=0
 ;
 S %LIBS="SYSDEV"
 ;
 ; Invalid Procedure name
 I '$$vDbEx2() D  Q 
 .	S ER=1
 .	S RM=$$^MSG(1408,PROCID)
 .	Q 
 ;
 WRITE !,PROCID,!
 ;
 N dbtbl25 S dbtbl25=$$vRCgetRecord0Opt^RecordDBTBL25(%LIBS,PROCID,0,"")
 ; type RecordDBTBL25D dbtbl25d = Db.getRecord("DBTBL25D","%LIBS=:%LIBS,PROCID=:PROCID,SEQ=1")
 ;
 I ($P(dbtbl25,$C(124),2)="") D  Q 
 .	S RM=$$^MSG(3056,PROCID)
 .	;   #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 .	WRITE $$MSG^%TRMVT(RM)
 .	HANG 2
 .	Q 
 ;
 S PGM=$P(dbtbl25,$C(124),2) ; Routine name
 ;
 N procfile S procfile=$$SRCFILE^FILEUTL("PROC",PROCID)
 ;
 N io S io=$$vClVobj($ST,"IO")
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 S $P(vobj(io,1),"|",2)=$$PARSE^%ZFUNC(procfile,"DIRECTORY")
 ;  #ACCEPT Date=20-JAN-2016;PGM=kulhan;WARN=ACCESS;CR=4
 S $P(vobj(io,1),"|",1)=$$PARSE^%ZFUNC(procfile,"NAME")_$$PARSE^%ZFUNC(procfile,"TYPE")
 S $P(vobj(io,1),"|",3)="READ"
 ;
 D
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch3^"_$T(+0)
 .	;
 .	D open^UCIO(io,$T(+0),"COMPILE","io")
 .	;
 .	N REC
 .	F  S REC=$$read^UCIO(io) D
 ..		S FCOUNT=FCOUNT+1
 ..		S m2src(FCOUNT)=REC
 ..		S SIZE=SIZE+$L(REC)
 ..		Q 
 .	Q 
 ;
 I ($E(PROCID,1,6)="Record") S cmpType="Filer"
 E  D
 .	;
 .	; Add signature tag
 .	S SIG=LTD_"^"_TIME_"^"_USER_"^"_SIZE
 .	S m2src(FCOUNT+1)=" #OPTION ResultClass ON"
 .	S m2src(FCOUNT+2)="public String vSIG()"_$char(9)_"quit """_SIG_""""_$char(9)_"// Signature - LTD^TIME^USER^SIZE"
 .	;
 .	S cmpType="Procedure"_"~"_SIG
 .	Q 
 ;
 N cmperr S cmperr=$$cmpA2F^PSLC(.m2src,,PGM,,PROCID_"~"_cmpType)
 ; do cmpA2F^UCGM(.m2src,PGM,,,,,,PROCID_ "~"_ cmpType)
 ;
 ; replace DBTBL25D content only after successful compilation (cache last correct version)
 I cmperr=0,cmpType'="Filer" D
 .	D vDbDe4() ; Delete existing records
 .	;
 .	N SEQ S SEQ=0
 .	F  S SEQ=$order(m2src(SEQ)) Q:(SEQ>FCOUNT)!(SEQ="")  D
 ..		I SEQ=1,(m2src(SEQ)["//DO NOT MODIFY") Q 
 ..		;
 ..		N dbtbl25d S dbtbl25d=$$vcdmNew^RecordDBTBL25D() S vobj(dbtbl25d,-3)="SYSDEV" S vobj(dbtbl25d,-4)=PROCID S vobj(dbtbl25d,-5)=SEQ
 ..	  S $P(vobj(dbtbl25d),$C(12),1)=m2src(SEQ)
 ..	 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL25D(dbtbl25d,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl25d,-100) S vobj(dbtbl25d,-2)=1 TC:vTp  
 ..		K vobj(+$G(dbtbl25d)) Q 
 .	Q 
 ;
 K vobj(+$G(io)) Q 
 ;
ERR ; 
 ;
 WRITE !,RM
 HANG 2
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^11449" ; Signature - LTD^TIME^USER^SIZE
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe1() ; DELETE FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4 S vRs=$$vOpen5()
 F  Q:'$$vFetch5()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^DBTBL(v1,25,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe2() ; DELETE FROM TMPDQ WHERE PID=:V1
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4 S vRs=$$vOpen6()
 F  Q:'$$vFetch6()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^TEMP(v1,v2)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe3() ; DELETE FROM TMPDQ WHERE PID=:V3
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4 S vRs=$$vOpen7()
 F  Q:'$$vFetch7()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^TEMP(v1,v2)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe4() ; DELETE FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4 S vRs=$$vOpen8()
 F  Q:'$$vFetch8()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^DBTBL(v1,25,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ;
vClVobj(vSt,vCls) ; Create a new object
 ;
 N vOid
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)=vCls_$C(9)_vSt
 Q vOid
 ;
vDbEx2() ; min(1): DISTINCT %LIBS,PROCID FROM DBTBL25 WHERE %LIBS=:%LIBS AND PROCID=:PROCID
 ;
 N vsql1,vsql2
 S vsql1=$$BYTECHAR^SQLUTL(254)
 S vsql2=$G(%LIBS) I vsql2="" Q 0
 ;
 I '($D(^DBTBL(vsql2,25,PROCID))#2) Q 0
 Q 1
 ;
vOpen1() ; %LIBS,PROCID,SEQ FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCID ORDER BY SEQ ASC
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(PROCID) I vos3="" G vL1a0
 S vos4=""
vL1a4 S vos4=$O(^DBTBL("SYSDEV",25,vos3,vos4),1) I vos4="" G vL1a0
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds="SYSDEV"_$C(9)_vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen2() ; %LIBS,PROCID,SEQ FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCIDF ORDER BY SEQ ASC
 ;
 ;
 S vos1=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos1=0 Q
vL2a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(PROCIDF) I vos3="" G vL2a0
 S vos4=""
vL2a4 S vos4=$O(^DBTBL("SYSDEV",25,vos3,vos4),1) I vos4="" G vL2a0
 Q
 ;
vFetch2() ;
 ;
 ;
 I vos1=1 D vL2a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds="SYSDEV"_$C(9)_vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen3() ; PROCID FROM DBTBL25 WHERE %LIBS='SYSDEV' ORDER BY PROCID ASC
 ;
 ;
 S vos1=2
 D vL3a1
 Q ""
 ;
vL3a0 S vos1=0 Q
vL3a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=""
vL3a3 S vos3=$O(^DBTBL("SYSDEV",25,vos3),1) I vos3="" G vL3a0
 Q
 ;
vFetch3() ;
 ;
 ;
 I vos1=1 D vL3a3
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S rs=$S(vos3=vos2:"",1:vos3)
 ;
 Q 1
 ;
vOpen4() ; PID,ELEMENT FROM TMPDQ WHERE PID=:V2 ORDER BY ELEMENT ASC
 ;
 ;
 S vos1=2
 D vL4a1
 Q ""
 ;
vL4a0 S vos1=0 Q
vL4a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V2)
 S vos4=""
vL4a4 S vos4=$O(^TEMP(vos3,vos4),1) I vos4="" G vL4a0
 Q
 ;
vFetch4() ;
 ;
 ;
 I vos1=1 D vL4a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen5() ; %LIBS,PROCID,SEQ FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCID
 ;
 ;
 S vos1=2
 D vL5a1
 Q ""
 ;
vL5a0 S vos1=0 Q
vL5a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(PROCID) I vos3="" G vL5a0
 S vos4=""
vL5a4 S vos4=$O(^DBTBL("SYSDEV",25,vos3,vos4),1) I vos4="" G vL5a0
 Q
 ;
vFetch5() ;
 ;
 ;
 I vos1=1 D vL5a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs="SYSDEV"_$C(9)_vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen6() ; PID,ELEMENT FROM TMPDQ WHERE PID=:V1
 ;
 ;
 S vos1=2
 D vL6a1
 Q ""
 ;
vL6a0 S vos1=0 Q
vL6a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1)
 S vos4=""
vL6a4 S vos4=$O(^TEMP(vos3,vos4),1) I vos4="" G vL6a0
 Q
 ;
vFetch6() ;
 ;
 ;
 I vos1=1 D vL6a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen7() ; PID,ELEMENT FROM TMPDQ WHERE PID=:V3
 ;
 ;
 S vos1=2
 D vL7a1
 Q ""
 ;
vL7a0 S vos1=0 Q
vL7a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V3)
 S vos4=""
vL7a4 S vos4=$O(^TEMP(vos3,vos4),1) I vos4="" G vL7a0
 Q
 ;
vFetch7() ;
 ;
 ;
 I vos1=1 D vL7a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen8() ; %LIBS,PROCID,SEQ FROM DBTBL25D WHERE %LIBS='SYSDEV' AND PROCID=:PROCID
 ;
 ;
 S vos1=2
 D vL8a1
 Q ""
 ;
vL8a0 S vos1=0 Q
vL8a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(PROCID) I vos3="" G vL8a0
 S vos4=""
vL8a4 S vos4=$O(^DBTBL("SYSDEV",25,vos3,vos4),1) I vos4="" G vL8a0
 Q
 ;
vFetch8() ;
 ;
 ;
 I vos1=1 D vL8a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs="SYSDEV"_$C(9)_vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vReCp1(v1) ; RecordDBTBL25.copy: DBTBL25
 ;
 Q $$copy^UCGMR(dbtbl25f)
 ;
vCatch3 ; Error trap
 ;
 N iox,$ET,$ES S iox=$ZE,$EC="",$ET="Q",$ZE=""
 I '($P(vobj(io,1),"|",6)="") D close^UCIO(io)
 I iox["IOEOF" D ZX^UCGMR(voxMrk) Q 
 S $ZE=iox,$EC=",U1001,"
 D ZX^UCGMR(voxMrk) Q 
 ;
vCatch2 ; Error trap
 ;
 N ioerr,$ET,$ES S ioerr=$ZE,$EC="",$ET="Q",$ZE=""
 I '($P(vobj(io,1),"|",6)="") D close^UCIO(io)
 I ioerr'["PSL-E-IO" S $ZE=ioerr,$EC=",U1001,"
 ;
 WRITE "I/O error: ",$P(ioerr,",",3),!
 D ZX^UCGMR(voxMrk) Q 
 ;
vCatch1 ; Error trap
 ;
 N ioerr,$ET,$ES S ioerr=$ZE,$EC="",$ET="Q",$ZE=""
 I '($P(vobj(io,1),"|",6)="") D close^UCIO(io)
 I ioerr'["PSL-E-IO" S $ZE=ioerr,$EC=",U1001,"
 ;
 WRITE "I/O error: ",$P(ioerr,",",3),!
 D ZX^UCGMR(voxMrk) Q 
