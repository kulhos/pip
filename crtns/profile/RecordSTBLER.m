 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordSTBLER ****
 ; 
 ; 01/23/2016 18:59 - kulhan
 ; 
 ;
 ; Record Class code for table STBLER
 ;
 ; Generated by PSLRecordBuilder on 01/23/2016 at 18:59 by 1
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSTBLER",vobj(vOid,-2)=0,vobj(vOid)=""
 S vobj(vOid,-3)=""
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0(v1,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSTBLER"
 S vobj(vOid)=$G(^STBL("ER",v1))
 I vobj(vOid)="",'($D(^STBL("ER",v1))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,STBLER",$EC=",U1001,"
 S vobj(vOid,-3)=v1
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord1(v1,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSTBLER"
 S vobj(vOid)=$G(^STBL("ER",v1))
 I vobj(vOid)="",'($D(^STBL("ER",v1))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,vfromDbSet,v2out) ; 
 N stbler
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S stbler=$G(^STBL("ER",v1))
 I stbler="",'($D(^STBL("ER",v1))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,STBLER",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q stbler
 ;
vRCgetRecord1Opt(v1,vfromDbSet,v2out) ; 
 N stbler
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S stbler=$G(^STBL("ER",v1))
 I stbler="",'($D(^STBL("ER",v1))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q stbler
 ;
vBypassSave(this) ; 
 D vSave(this,"/NOJOURNAL/NOTRIGAFT/NOTRIGBEF/NOVALDD/NOVALREQ/NOVALRI/NOVALST",0)
 Q 
 ;
vSave(this,vRCparams,vauditLogSeq) ; 
 N vRCaudit N vRCauditIns
 N %O S %O=$G(vobj(this,-2))
 I ($get(vRCparams)="") S vRCparams="/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/"
 I (%O=0) D
 .	D AUDIT^UCUTILN(this,.vRCauditIns,1,"|")
 .	D vRCsetDefaults(this)
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,%O)
 .	D vRCmiscValidations(this,vRCparams,%O)
 .	D vRCupdateDB(this,%O,vRCparams,.vRCaudit,.vRCauditIns)
 .	Q 
 E  I (%O=1) D
 .	D AUDIT^UCUTILN(this,.vRCaudit,1,"|")
 .	I ($D(vobj(this,-100,"1*","KEY"))&($P($E($G(vobj(this,-100,"1*","KEY")),5,9999),$C(124))'=vobj(this,-3))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForUpdate(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD1(this)
 .	D vRCmiscValidations(this,vRCparams,%O)
 .	D vRCupdateDB(this,%O,vRCparams,.vRCaudit,.vRCauditIns)
 .	Q 
 E  I (%O=2) D
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,%O)
 .	D vRCmiscValidations(this,vRCparams,2)
 .	Q 
 E  I (%O=3) D
 .	  N V1 S V1=vobj(this,-3) Q:'($D(^STBL("ER",V1))#2) 
 .	D vRCdelete(this,vRCparams,.vRCaudit,0)
 .	Q 
 Q 
 ;
vcheckAccessRights() ; 
 Q ""
 ;
vinsertAccess(userclass) ; 
 Q 1
 ;
vinsertOK(this,userclass) ; PUBLIC access is allowed, no restrict clause
 Q 1
 ;
vupdateAccess(userclass) ; 
 Q 1
 ;
vupdateOK(this,userclass) ; PUBLIC access is allowed, no restrict clause
 Q 1
 ;
vdeleteAccess(userclass) ; 
 Q 1
 ;
vdeleteOK(this,userclass) ; PUBLIC access is allowed, no restrict clause
 Q 1
 ;
vselectAccess(userclass,restrict,from) ; 
 S (restrict,from)=""
 Q 1
 ;
vselectOK(this,userclass) ; PUBLIC access is allowed, no restrict clause
 Q 1
 ;
vselectOptmOK(userclass,stbler,vkey1) ; PUBLIC access is allowed, no restrict clause
 Q 1
 ;
vgetLogging() ; 
 Q "0"
 ;
logUserclass(operation) ; 
 I (operation="INSERT") Q 0
 E  I (operation="UPDATE") Q 0
 E  I (operation="DELETE") Q 0
 E  I (operation="SELECT") Q 0
 Q 0
 ;
vlogSelect(statement,using) ; 
 Q 0
 ;
columnList() ; 
 Q $$vStrRep("BELL,BRK,DSPET,DSPSEQ,GO,HALT,HANG,KEY,LOG,MAIL,QAFTERR,RM,RM2,VAR",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-3)="KEY:T:"
 S map(-1)="BELL:L:3;BRK:L:8;DSPET:L:4;DSPSEQ:L:13;GO:T:10;HALT:L:7;HANG:N:12;LOG:L:2;MAIL:L:11;QAFTERR:L:9;RM:T:1;RM2:T:6;VAR:T:5"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N stbler S stbler=$$vRCgetRecord0^RecordSTBLER(KEY,0)
 .	S vobj(stbler,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSTBLER(stbler,$$initPar^UCUTILN(params)) K vobj(stbler,-100) S vobj(stbler,-2)=1 TC:vTp  
 .	K vobj(+$G(stbler)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1 S V1=vobj(this,-3) I '(''($D(^STBL("ER",V1))#2)=''processMode) D
 .	N errmsg
 .	I (+processMode'=+0) S errmsg=$$^MSG(7932)
 .	E  S errmsg=$$^MSG(2327)
 .	D throwError(errmsg)
 .	Q 
 Q 
 ;
vRCupdateDB(this,processMode,vRCparams,vRCaudit,vRCauditIns) ; 
 I '(("/"_vRCparams_"/")["/NOUPDATE/") D
 .	I '(("/"_vRCparams_"/")["/NOLOG/") D
 ..		I (processMode=1) D ^DBSLOGIT(this,1,.vRCaudit) Q 
 ..		D ^DBSLOGIT(this,0,.vRCauditIns)
 ..		Q 
 .	;   #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	if $D(vobj(this)) S ^STBL("ER",vobj(this,-3))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 I '(("/"_vRCparams_"/")["/NOLOG/") D ^DBSLOGIT(this,3)
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^STBL("ER",vobj(this,-3))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCsetDefaults(this) ; 
 I ($P(vobj(this),$C(124),3)="")  S $P(vobj(this),$C(124),3)=0
 I ($P(vobj(this),$C(124),8)="")  S $P(vobj(this),$C(124),8)=0
 I ($P(vobj(this),$C(124),4)="")  S $P(vobj(this),$C(124),4)=0
 I ($P(vobj(this),$C(124),13)="")  S $P(vobj(this),$C(124),13)=0
 I ($P(vobj(this),$C(124),7)="")  S $P(vobj(this),$C(124),7)=0
 I ($P(vobj(this),$C(124),12)="")  S $P(vobj(this),$C(124),12)=999999
 I ($P(vobj(this),$C(124),2)="")  S $P(vobj(this),$C(124),2)=0
 I ($P(vobj(this),$C(124),11)="")  S $P(vobj(this),$C(124),11)=0
 I ($P(vobj(this),$C(124),9)="")  S $P(vobj(this),$C(124),9)=0
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I ($P(vobj(this),$C(124),3)="") D vRCrequiredErr("BELL")
 I ($P(vobj(this),$C(124),8)="") D vRCrequiredErr("BRK")
 I ($P(vobj(this),$C(124),4)="") D vRCrequiredErr("DSPET")
 I ($P(vobj(this),$C(124),13)="") D vRCrequiredErr("DSPSEQ")
 I ($P(vobj(this),$C(124),7)="") D vRCrequiredErr("HALT")
 I (vobj(this,-3)="") D vRCrequiredErr("KEY")
 I ($P(vobj(this),$C(124),2)="") D vRCrequiredErr("LOG")
 I ($P(vobj(this),$C(124),11)="") D vRCrequiredErr("MAIL")
 I ($P(vobj(this),$C(124),9)="") D vRCrequiredErr("QAFTERR")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("KEY")
 I ($D(vobj(this,-100,"0*","BELL"))&($P($E($G(vobj(this,-100,"0*","BELL")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))),($P(vobj(this),$C(124),3)="") D vRCrequiredErr("BELL")
 I ($D(vobj(this,-100,"0*","BRK"))&($P($E($G(vobj(this,-100,"0*","BRK")),5,9999),$C(124))'=$P(vobj(this),$C(124),8))),($P(vobj(this),$C(124),8)="") D vRCrequiredErr("BRK")
 I ($D(vobj(this,-100,"0*","DSPET"))&($P($E($G(vobj(this,-100,"0*","DSPET")),5,9999),$C(124))'=$P(vobj(this),$C(124),4))),($P(vobj(this),$C(124),4)="") D vRCrequiredErr("DSPET")
 I ($D(vobj(this,-100,"0*","DSPSEQ"))&($P($E($G(vobj(this,-100,"0*","DSPSEQ")),5,9999),$C(124))'=$P(vobj(this),$C(124),13))),($P(vobj(this),$C(124),13)="") D vRCrequiredErr("DSPSEQ")
 I ($D(vobj(this,-100,"0*","HALT"))&($P($E($G(vobj(this,-100,"0*","HALT")),5,9999),$C(124))'=$P(vobj(this),$C(124),7))),($P(vobj(this),$C(124),7)="") D vRCrequiredErr("HALT")
 I ($D(vobj(this,-100,"0*","LOG"))&($P($E($G(vobj(this,-100,"0*","LOG")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))),($P(vobj(this),$C(124),2)="") D vRCrequiredErr("LOG")
 I ($D(vobj(this,-100,"0*","MAIL"))&($P($E($G(vobj(this,-100,"0*","MAIL")),5,9999),$C(124))'=$P(vobj(this),$C(124),11))),($P(vobj(this),$C(124),11)="") D vRCrequiredErr("MAIL")
 I ($D(vobj(this,-100,"0*","QAFTERR"))&($P($E($G(vobj(this,-100,"0*","QAFTERR")),5,9999),$C(124))'=$P(vobj(this),$C(124),9))),($P(vobj(this),$C(124),9)="") D vRCrequiredErr("QAFTERR")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("STBLER","MSG",1767,"STBLER."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("KEY",$$^MSG(1076,12))
 I '(($P(vobj(this),$C(124),3)=1)!($P(vobj(this),$C(124),3)=0)) D vRCvalidateDDerr("BELL",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),8)=1)!($P(vobj(this),$C(124),8)=0)) D vRCvalidateDDerr("BRK",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),4)=1)!($P(vobj(this),$C(124),4)=0)) D vRCvalidateDDerr("DSPET",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),13)=1)!($P(vobj(this),$C(124),13)=0)) D vRCvalidateDDerr("DSPSEQ",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),10))>17) D vRCvalidateDDerr("GO",$$^MSG(1076,17))
 I '(($P(vobj(this),$C(124),7)=1)!($P(vobj(this),$C(124),7)=0)) D vRCvalidateDDerr("HALT",$$^MSG(742,"L"))
 I '($P(vobj(this),$C(124),12)=""),'(+$P(vobj(this),$C(124),12)=$P(vobj(this),$C(124),12))  S $P(vobj(this),$C(124),12)=$$vRCtrimNumber($P(vobj(this),$C(124),12))
 S X=$P(vobj(this),$C(124),12) I '(X=""),(X'?1.6N),(X'?1"-"1.5N) D vRCvalidateDDerr("HANG",$$^MSG(742,"N"))
 I '(($P(vobj(this),$C(124),2)=1)!($P(vobj(this),$C(124),2)=0)) D vRCvalidateDDerr("LOG",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),11)=1)!($P(vobj(this),$C(124),11)=0)) D vRCvalidateDDerr("MAIL",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),9)=1)!($P(vobj(this),$C(124),9)=0)) D vRCvalidateDDerr("QAFTERR",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),1))>88) D vRCvalidateDDerr("RM",$$^MSG(1076,88))
 I ($L($P(vobj(this),$C(124),6))>60) D vRCvalidateDDerr("RM2",$$^MSG(1076,60))
 I ($L($P(vobj(this),$C(124),5))>60) D vRCvalidateDDerr("VAR",$$^MSG(1076,60))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","KEY"))&($P($E($G(vobj(this,-100,"1*","KEY")),5,9999),$C(124))'=vobj(this,-3))) I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("KEY",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"0*","BELL"))&($P($E($G(vobj(this,-100,"0*","BELL")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))) I '(($P(vobj(this),$C(124),3)=1)!($P(vobj(this),$C(124),3)=0)) D vRCvalidateDDerr("BELL",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","BRK"))&($P($E($G(vobj(this,-100,"0*","BRK")),5,9999),$C(124))'=$P(vobj(this),$C(124),8))) I '(($P(vobj(this),$C(124),8)=1)!($P(vobj(this),$C(124),8)=0)) D vRCvalidateDDerr("BRK",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","DSPET"))&($P($E($G(vobj(this,-100,"0*","DSPET")),5,9999),$C(124))'=$P(vobj(this),$C(124),4))) I '(($P(vobj(this),$C(124),4)=1)!($P(vobj(this),$C(124),4)=0)) D vRCvalidateDDerr("DSPET",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","DSPSEQ"))&($P($E($G(vobj(this,-100,"0*","DSPSEQ")),5,9999),$C(124))'=$P(vobj(this),$C(124),13))) I '(($P(vobj(this),$C(124),13)=1)!($P(vobj(this),$C(124),13)=0)) D vRCvalidateDDerr("DSPSEQ",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","GO"))&($P($E($G(vobj(this,-100,"0*","GO")),5,9999),$C(124))'=$P(vobj(this),$C(124),10))) I ($L($P(vobj(this),$C(124),10))>17) D vRCvalidateDDerr("GO",$$^MSG(1076,17))
 I ($D(vobj(this,-100,"0*","HALT"))&($P($E($G(vobj(this,-100,"0*","HALT")),5,9999),$C(124))'=$P(vobj(this),$C(124),7))) I '(($P(vobj(this),$C(124),7)=1)!($P(vobj(this),$C(124),7)=0)) D vRCvalidateDDerr("HALT",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","HANG"))&($P($E($G(vobj(this,-100,"0*","HANG")),5,9999),$C(124))'=$P(vobj(this),$C(124),12))),'($P(vobj(this),$C(124),12)=""),'(+$P(vobj(this),$C(124),12)=$P(vobj(this),$C(124),12))  S $P(vobj(this),$C(124),12)=$$vRCtrimNumber($P(vobj(this),$C(124),12))
 I ($D(vobj(this,-100,"0*","HANG"))&($P($E($G(vobj(this,-100,"0*","HANG")),5,9999),$C(124))'=$P(vobj(this),$C(124),12))) S X=$P(vobj(this),$C(124),12) I '(X=""),(X'?1.6N),(X'?1"-"1.5N) D vRCvalidateDDerr("HANG",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"0*","LOG"))&($P($E($G(vobj(this,-100,"0*","LOG")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) I '(($P(vobj(this),$C(124),2)=1)!($P(vobj(this),$C(124),2)=0)) D vRCvalidateDDerr("LOG",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","MAIL"))&($P($E($G(vobj(this,-100,"0*","MAIL")),5,9999),$C(124))'=$P(vobj(this),$C(124),11))) I '(($P(vobj(this),$C(124),11)=1)!($P(vobj(this),$C(124),11)=0)) D vRCvalidateDDerr("MAIL",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","QAFTERR"))&($P($E($G(vobj(this,-100,"0*","QAFTERR")),5,9999),$C(124))'=$P(vobj(this),$C(124),9))) I '(($P(vobj(this),$C(124),9)=1)!($P(vobj(this),$C(124),9)=0)) D vRCvalidateDDerr("QAFTERR",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","RM"))&($P($E($G(vobj(this,-100,"0*","RM")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) I ($L($P(vobj(this),$C(124),1))>88) D vRCvalidateDDerr("RM",$$^MSG(1076,88))
 I ($D(vobj(this,-100,"0*","RM2"))&($P($E($G(vobj(this,-100,"0*","RM2")),5,9999),$C(124))'=$P(vobj(this),$C(124),6))) I ($L($P(vobj(this),$C(124),6))>60) D vRCvalidateDDerr("RM2",$$^MSG(1076,60))
 I ($D(vobj(this,-100,"0*","VAR"))&($P($E($G(vobj(this,-100,"0*","VAR")),5,9999),$C(124))'=$P(vobj(this),$C(124),5))) I ($L($P(vobj(this),$C(124),5))>60) D vRCvalidateDDerr("VAR",$$^MSG(1076,60))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("STBLER","MSG",979,"STBLER."_column_" "_errmsg)
 I ER D throwError($get(RM))
 Q 
 ;
vRCtrimNumber(str) ; 
 I ($E(str,1)="0") S str=$$vStrTrim(str,-1,"0") I (str="") S str="0"
 I (str["."),($E(str,$L(str))="0") S str=$$RTCHR^%ZFUNC(str,"0") I ($E(str,$L(str))=".") S str=$E(str,1,$L(str)-1) I (str="") S str="0"
 Q str
 ;
vRCkeyChanged(this,vRCparams,vRCaudit) ; 
 N vTp
 N newkeys N oldkeys N vRCauditIns
 N newKey1 S newKey1=vobj(this,-3)
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","KEY")):$P($E(vobj(this,-100,"1*","KEY"),5,9999),$C(124)),1:vobj(this,-3))
  N V1 S V1=vobj(this,-3) I ($D(^STBL("ER",V1))#2) D throwError($$^MSG(2327))
 S newkeys=newKey1
 S oldkeys=oldKey1
  S vobj(this,-3)=oldKey1
 S vRCparams=$$setPar^UCUTILN(vRCparams,"NOINDEX")
 I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,1)
 D vRCmiscValidations(this,vRCparams,1)
 D vRCupdateDB(this,1,vRCparams,.vRCaudit,.vRCauditIns)
  S vobj(this,-3)=newKey1
 N newrec S newrec=$$vReCp1(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSTBLER(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("STBLER",oldkeys,newkeys)
 .	I ER D throwError($get(RM))
 .	Q 
  S vobj(this,-3)=oldKey1
 S vRCparams=$$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/")
 D vRCdelete(this,vRCparams,.vRCaudit,1)
  S vobj(this,-3)=newKey1
 K vobj(+$G(newrec)) Q 
 ;
throwError(MSG) ; 
 S $ZE="0,"_$ZPOS_","_"%PSL-E-DBFILER,"_$translate(MSG,",","~"),$EC=",U1001,"
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vStrRep(object,p1,p2,p3,p4,qt) ; String.replace
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 ;
 I p3<0 Q object
 I $L(p1)=1,$L(p2)<2,'p3,'p4,(qt="") Q $translate(object,p1,p2)
 ;
 N y S y=0
 F  S y=$$vStrFnd(object,p1,y,p4,qt) Q:y=0  D
 .	S object=$E(object,1,y-$L(p1)-1)_p2_$E(object,y,1048575)
 .	S y=y+$L(p2)-$L(p1)
 .	I p3 S p3=p3-1 I p3=0 S y=$L(object)+1
 .	Q 
 Q object
 ; ----------------
 ;  #OPTION ResultClass 1
vStrTrim(object,p1,p2) ; String.trim
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 I p1'<0 S object=$$RTCHR^%ZFUNC(object,p2)
 I p1'>0 F  Q:$E(object,1)'=p2  S object=$E(object,2,1048575)
 Q object
 ; ----------------
 ;  #OPTION ResultClass 1
vStrFnd(object,p1,p2,p3,qt) ; String.find
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 ;
 I (p1="") Q $S(p2<1:1,1:+p2)
 I p3 S object=$ZCONVERT(object,"U") S p1=$ZCONVERT(p1,"U")
 S p2=$F(object,p1,p2)
 I '(qt=""),$L($E(object,1,p2-1),qt)#2=0 D
 .	F  S p2=$F(object,p1,p2) Q:p2=0!($L($E(object,1,p2-1),qt)#2) 
 .	Q 
 Q p2
 ;
vReCp1(v1) ; RecordSTBLER.copy: STBLER
 ;
 Q $$copy^UCGMR(this)
