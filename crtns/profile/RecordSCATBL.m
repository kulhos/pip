 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordSCATBL ****
 ; 
 ; 01/23/2016 18:58 - kulhan
 ; 
 ;
 ; Record Class code for table SCATBL
 ;
 ; Generated by PSLRecordBuilder on 01/23/2016 at 18:58 by 1
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSCATBL",vobj(vOid,-2)=0,vobj(vOid)=""
 S vobj(vOid,-3)=""
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0(v1,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSCATBL"
 S vobj(vOid)=$G(^SCATBL(1,v1))
 I vobj(vOid)="",'($D(^SCATBL(1,v1))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,SCATBL",$EC=",U1001,"
 S vobj(vOid,-3)=v1
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord1(v1,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordSCATBL"
 S vobj(vOid)=$G(^SCATBL(1,v1))
 I vobj(vOid)="",'($D(^SCATBL(1,v1))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,vfromDbSet,v2out) ; 
 N scatbl
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S scatbl=$G(^SCATBL(1,v1))
 I scatbl="",'($D(^SCATBL(1,v1))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,SCATBL",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q scatbl
 ;
vRCgetRecord1Opt(v1,vfromDbSet,v2out) ; 
 N scatbl
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S scatbl=$G(^SCATBL(1,v1))
 I scatbl="",'($D(^SCATBL(1,v1))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q scatbl
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
 .	I ($D(vobj(this,-100,"1*","FN"))&($P($E($G(vobj(this,-100,"1*","FN")),5,9999),$C(124))'=vobj(this,-3))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1 S V1=vobj(this,-3) Q:'($D(^SCATBL(1,V1))#2) 
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
vselectOptmOK(userclass,scatbl,vkey1) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("%LIBS,%SN,BPSFLG,BREAK,DDP,DESC,FN,NOHOST,NOREPOST,PGM,POP,PRP,QUEUE,RESTORE,SALON,TFK,TIMBEG,TIMEND",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-3)="FN:T:"
 S map(-1)="%LIBS:T:10;%SN:T:11;BPSFLG:L:17;BREAK:L:15;DDP:L:5;DESC:T:1;NOHOST:L:16;NOREPOST:L:19;PGM:T:4;POP:T:3;PRP:T:2;QUEUE:L:18;RESTORE:L:14;SALON:L:6;TFK:T:12;TIMBEG:C:7;TIMEND:C:8"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N scatbl S scatbl=$$vRCgetRecord0^RecordSCATBL(FN,0)
 .	S vobj(scatbl,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSCATBL(scatbl,$$initPar^UCUTILN(params)) K vobj(scatbl,-100) S vobj(scatbl,-2)=1 TC:vTp  
 .	K vobj(+$G(scatbl)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1 S V1=vobj(this,-3) I '(''($D(^SCATBL(1,V1))#2)=''processMode) D
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
 .	if $D(vobj(this)) S ^SCATBL(1,vobj(this,-3))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 I '(("/"_vRCparams_"/")["/NOLOG/") D ^DBSLOGIT(this,3)
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^SCATBL(1,vobj(this,-3))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCsetDefaults(this) ; 
 I ($P(vobj(this),$C(124),11)="")  S $P(vobj(this),$C(124),11)="PBS"
 I ($P(vobj(this),$C(124),17)="")  S $P(vobj(this),$C(124),17)=0
 I ($P(vobj(this),$C(124),15)="")  S $P(vobj(this),$C(124),15)=0
 I ($P(vobj(this),$C(124),5)="")  S $P(vobj(this),$C(124),5)=0
 I ($P(vobj(this),$C(124),16)="")  S $P(vobj(this),$C(124),16)=0
 I ($P(vobj(this),$C(124),19)="")  S $P(vobj(this),$C(124),19)=0
 I ($P(vobj(this),$C(124),18)="")  S $P(vobj(this),$C(124),18)=0
 I ($P(vobj(this),$C(124),14)="")  S $P(vobj(this),$C(124),14)=0
 I ($P(vobj(this),$C(124),6)="")  S $P(vobj(this),$C(124),6)=0
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I ($P(vobj(this),$C(124),17)="") D vRCrequiredErr("BPSFLG")
 I ($P(vobj(this),$C(124),15)="") D vRCrequiredErr("BREAK")
 I ($P(vobj(this),$C(124),5)="") D vRCrequiredErr("DDP")
 I (vobj(this,-3)="") D vRCrequiredErr("FN")
 I ($P(vobj(this),$C(124),16)="") D vRCrequiredErr("NOHOST")
 I ($P(vobj(this),$C(124),19)="") D vRCrequiredErr("NOREPOST")
 I ($P(vobj(this),$C(124),18)="") D vRCrequiredErr("QUEUE")
 I ($P(vobj(this),$C(124),14)="") D vRCrequiredErr("RESTORE")
 I ($P(vobj(this),$C(124),6)="") D vRCrequiredErr("SALON")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("FN")
 I ($D(vobj(this,-100,"0*","BPSFLG"))&($P($E($G(vobj(this,-100,"0*","BPSFLG")),5,9999),$C(124))'=$P(vobj(this),$C(124),17))),($P(vobj(this),$C(124),17)="") D vRCrequiredErr("BPSFLG")
 I ($D(vobj(this,-100,"0*","BREAK"))&($P($E($G(vobj(this,-100,"0*","BREAK")),5,9999),$C(124))'=$P(vobj(this),$C(124),15))),($P(vobj(this),$C(124),15)="") D vRCrequiredErr("BREAK")
 I ($D(vobj(this,-100,"0*","DDP"))&($P($E($G(vobj(this,-100,"0*","DDP")),5,9999),$C(124))'=$P(vobj(this),$C(124),5))),($P(vobj(this),$C(124),5)="") D vRCrequiredErr("DDP")
 I ($D(vobj(this,-100,"0*","NOHOST"))&($P($E($G(vobj(this,-100,"0*","NOHOST")),5,9999),$C(124))'=$P(vobj(this),$C(124),16))),($P(vobj(this),$C(124),16)="") D vRCrequiredErr("NOHOST")
 I ($D(vobj(this,-100,"0*","NOREPOST"))&($P($E($G(vobj(this,-100,"0*","NOREPOST")),5,9999),$C(124))'=$P(vobj(this),$C(124),19))),($P(vobj(this),$C(124),19)="") D vRCrequiredErr("NOREPOST")
 I ($D(vobj(this,-100,"0*","QUEUE"))&($P($E($G(vobj(this,-100,"0*","QUEUE")),5,9999),$C(124))'=$P(vobj(this),$C(124),18))),($P(vobj(this),$C(124),18)="") D vRCrequiredErr("QUEUE")
 I ($D(vobj(this,-100,"0*","RESTORE"))&($P($E($G(vobj(this,-100,"0*","RESTORE")),5,9999),$C(124))'=$P(vobj(this),$C(124),14))),($P(vobj(this),$C(124),14)="") D vRCrequiredErr("RESTORE")
 I ($D(vobj(this,-100,"0*","SALON"))&($P($E($G(vobj(this,-100,"0*","SALON")),5,9999),$C(124))'=$P(vobj(this),$C(124),6))),($P(vobj(this),$C(124),6)="") D vRCrequiredErr("SALON")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("SCATBL","MSG",1767,"SCATBL."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("FN",$$^MSG(1076,12))
 I ($L($P(vobj(this),$C(124),10))>20) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,20))
 S X=$P(vobj(this),$C(124),11) I '(X=""),'($D(^SCATBL(2,X))#2) D vRCvalidateDDerr("%SN",$$^MSG(1485,X))
 I '(($P(vobj(this),$C(124),17)=1)!($P(vobj(this),$C(124),17)=0)) D vRCvalidateDDerr("BPSFLG",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),15)=1)!($P(vobj(this),$C(124),15)=0)) D vRCvalidateDDerr("BREAK",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),5)=1)!($P(vobj(this),$C(124),5)=0)) D vRCvalidateDDerr("DDP",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),1))>50) D vRCvalidateDDerr("DESC",$$^MSG(1076,50))
 I '(($P(vobj(this),$C(124),16)=1)!($P(vobj(this),$C(124),16)=0)) D vRCvalidateDDerr("NOHOST",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),19)=1)!($P(vobj(this),$C(124),19)=0)) D vRCvalidateDDerr("NOREPOST",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),4))>200) D vRCvalidateDDerr("PGM",$$^MSG(1076,200))
 I ($L($P(vobj(this),$C(124),3))>255) D vRCvalidateDDerr("POP",$$^MSG(1076,255))
 I ($L($P(vobj(this),$C(124),2))>255) D vRCvalidateDDerr("PRP",$$^MSG(1076,255))
 I '(($P(vobj(this),$C(124),18)=1)!($P(vobj(this),$C(124),18)=0)) D vRCvalidateDDerr("QUEUE",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),14)=1)!($P(vobj(this),$C(124),14)=0)) D vRCvalidateDDerr("RESTORE",$$^MSG(742,"L"))
 I '(($P(vobj(this),$C(124),6)=1)!($P(vobj(this),$C(124),6)=0)) D vRCvalidateDDerr("SALON",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),12))>12) D vRCvalidateDDerr("TFK",$$^MSG(1076,12))
 S X=$P(vobj(this),$C(124),7) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIMBEG",$$^MSG(742,"C"))
 S X=$P(vobj(this),$C(124),8) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIMEND",$$^MSG(742,"C"))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","FN"))&($P($E($G(vobj(this,-100,"1*","FN")),5,9999),$C(124))'=vobj(this,-3))) I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("FN",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"0*","%LIBS"))&($P($E($G(vobj(this,-100,"0*","%LIBS")),5,9999),$C(124))'=$P(vobj(this),$C(124),10))) I ($L($P(vobj(this),$C(124),10))>20) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,20))
 I ($D(vobj(this,-100,"0*","%SN"))&($P($E($G(vobj(this,-100,"0*","%SN")),5,9999),$C(124))'=$P(vobj(this),$C(124),11))) S X=$P(vobj(this),$C(124),11) I '(X=""),'($D(^SCATBL(2,X))#2) D vRCvalidateDDerr("%SN",$$^MSG(1485,X))
 I ($D(vobj(this,-100,"0*","BPSFLG"))&($P($E($G(vobj(this,-100,"0*","BPSFLG")),5,9999),$C(124))'=$P(vobj(this),$C(124),17))) I '(($P(vobj(this),$C(124),17)=1)!($P(vobj(this),$C(124),17)=0)) D vRCvalidateDDerr("BPSFLG",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","BREAK"))&($P($E($G(vobj(this,-100,"0*","BREAK")),5,9999),$C(124))'=$P(vobj(this),$C(124),15))) I '(($P(vobj(this),$C(124),15)=1)!($P(vobj(this),$C(124),15)=0)) D vRCvalidateDDerr("BREAK",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","DDP"))&($P($E($G(vobj(this,-100,"0*","DDP")),5,9999),$C(124))'=$P(vobj(this),$C(124),5))) I '(($P(vobj(this),$C(124),5)=1)!($P(vobj(this),$C(124),5)=0)) D vRCvalidateDDerr("DDP",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","DESC"))&($P($E($G(vobj(this,-100,"0*","DESC")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) I ($L($P(vobj(this),$C(124),1))>50) D vRCvalidateDDerr("DESC",$$^MSG(1076,50))
 I ($D(vobj(this,-100,"0*","NOHOST"))&($P($E($G(vobj(this,-100,"0*","NOHOST")),5,9999),$C(124))'=$P(vobj(this),$C(124),16))) I '(($P(vobj(this),$C(124),16)=1)!($P(vobj(this),$C(124),16)=0)) D vRCvalidateDDerr("NOHOST",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","NOREPOST"))&($P($E($G(vobj(this,-100,"0*","NOREPOST")),5,9999),$C(124))'=$P(vobj(this),$C(124),19))) I '(($P(vobj(this),$C(124),19)=1)!($P(vobj(this),$C(124),19)=0)) D vRCvalidateDDerr("NOREPOST",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","PGM"))&($P($E($G(vobj(this,-100,"0*","PGM")),5,9999),$C(124))'=$P(vobj(this),$C(124),4))) I ($L($P(vobj(this),$C(124),4))>200) D vRCvalidateDDerr("PGM",$$^MSG(1076,200))
 I ($D(vobj(this,-100,"0*","POP"))&($P($E($G(vobj(this,-100,"0*","POP")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))) I ($L($P(vobj(this),$C(124),3))>255) D vRCvalidateDDerr("POP",$$^MSG(1076,255))
 I ($D(vobj(this,-100,"0*","PRP"))&($P($E($G(vobj(this,-100,"0*","PRP")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) I ($L($P(vobj(this),$C(124),2))>255) D vRCvalidateDDerr("PRP",$$^MSG(1076,255))
 I ($D(vobj(this,-100,"0*","QUEUE"))&($P($E($G(vobj(this,-100,"0*","QUEUE")),5,9999),$C(124))'=$P(vobj(this),$C(124),18))) I '(($P(vobj(this),$C(124),18)=1)!($P(vobj(this),$C(124),18)=0)) D vRCvalidateDDerr("QUEUE",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","RESTORE"))&($P($E($G(vobj(this,-100,"0*","RESTORE")),5,9999),$C(124))'=$P(vobj(this),$C(124),14))) I '(($P(vobj(this),$C(124),14)=1)!($P(vobj(this),$C(124),14)=0)) D vRCvalidateDDerr("RESTORE",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","SALON"))&($P($E($G(vobj(this,-100,"0*","SALON")),5,9999),$C(124))'=$P(vobj(this),$C(124),6))) I '(($P(vobj(this),$C(124),6)=1)!($P(vobj(this),$C(124),6)=0)) D vRCvalidateDDerr("SALON",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","TFK"))&($P($E($G(vobj(this,-100,"0*","TFK")),5,9999),$C(124))'=$P(vobj(this),$C(124),12))) I ($L($P(vobj(this),$C(124),12))>12) D vRCvalidateDDerr("TFK",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"0*","TIMBEG"))&($P($E($G(vobj(this,-100,"0*","TIMBEG")),5,9999),$C(124))'=$P(vobj(this),$C(124),7))) S X=$P(vobj(this),$C(124),7) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIMBEG",$$^MSG(742,"C"))
 I ($D(vobj(this,-100,"0*","TIMEND"))&($P($E($G(vobj(this,-100,"0*","TIMEND")),5,9999),$C(124))'=$P(vobj(this),$C(124),8))) S X=$P(vobj(this),$C(124),8) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIMEND",$$^MSG(742,"C"))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("SCATBL","MSG",979,"SCATBL."_column_" "_errmsg)
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
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","FN")):$P($E(vobj(this,-100,"1*","FN"),5,9999),$C(124)),1:vobj(this,-3))
  N V1 S V1=vobj(this,-3) I ($D(^SCATBL(1,V1))#2) D throwError($$^MSG(2327))
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
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSCATBL(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("SCATBL",oldkeys,newkeys)
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
vReCp1(v1) ; RecordSCATBL.copy: SCATBL
 ;
 Q $$copy^UCGMR(this)
