 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordDBBUF ****
 ; 
 ; 01/23/2016 18:58 - kulhan
 ; 
 ;
 ; Record Class code for table DBBUF
 ;
 ; Generated by PSLRecordBuilder on 01/23/2016 at 18:58 by 1
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBBUF",vobj(vOid,-2)=0,vobj(vOid)=""
 S vobj(vOid,-3)=""
 S vobj(vOid,-4)=""
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0(v1,v2,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBBUF"
 S vobj(vOid)=$G(^DBBUF(v1,v2))
 I vobj(vOid)="",'($D(^DBBUF(v1,v2))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBBUF",$EC=",U1001,"
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord1(v1,v2,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBBUF"
 S vobj(vOid)=$G(^DBBUF(v1,v2))
 I vobj(vOid)="",'($D(^DBBUF(v1,v2))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,vfromDbSet,v2out) ; 
 N dbbuf
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbbuf=$G(^DBBUF(v1,v2))
 I dbbuf="",'($D(^DBBUF(v1,v2))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBBUF",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q dbbuf
 ;
vRCgetRecord1Opt(v1,v2,vfromDbSet,v2out) ; 
 N dbbuf
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbbuf=$G(^DBBUF(v1,v2))
 I dbbuf="",'($D(^DBBUF(v1,v2))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q dbbuf
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
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,%O)
 .	D vRCmiscValidations(this,vRCparams,%O)
 .	D vRCupdateDB(this,%O,vRCparams,.vRCaudit,.vRCauditIns)
 .	Q 
 E  I (%O=1) D
 .	D AUDIT^UCUTILN(this,.vRCaudit,1,"|")
 .	I ($D(vobj(this,-100,"1*","TOKEN"))&($P($E($G(vobj(this,-100,"1*","TOKEN")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","BUFFER"))&($P($E($G(vobj(this,-100,"2*","BUFFER")),5,9999),$C(124))'=vobj(this,-4))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) Q:'($D(^DBBUF(V1,V2))#2) 
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
vselectOptmOK(userclass,dbbuf,vkey1,vkey2) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("BEFD,BUFFER,BUFSTAT,TOKEN",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-4)="BUFFER:T:"
 S map(-3)="TOKEN:T:"
 S map(-1)="BEFD:N:2;BUFSTAT:N:1"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N dbbuf S dbbuf=$$vRCgetRecord0^RecordDBBUF(TOKEN,BUFFER,0)
 .	S vobj(dbbuf,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBBUF(dbbuf,$$initPar^UCUTILN(params)) K vobj(dbbuf,-100) S vobj(dbbuf,-2)=1 TC:vTp  
 .	K vobj(+$G(dbbuf)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I '(''($D(^DBBUF(V1,V2))#2)=''processMode) D
 .	N errmsg
 .	I (+processMode'=+0) S errmsg=$$^MSG(7932)
 .	E  S errmsg=$$^MSG(2327)
 .	D throwError(errmsg)
 .	Q 
 Q 
 ;
vRCupdateDB(this,processMode,vRCparams,vRCaudit,vRCauditIns) ; 
 I '(("/"_vRCparams_"/")["/NOUPDATE/") D
 .	;   #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	if $D(vobj(this)) S ^DBBUF(vobj(this,-3),vobj(this,-4))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^DBBUF(vobj(this,-3),vobj(this,-4))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I (vobj(this,-4)="") D vRCrequiredErr("BUFFER")
 I (vobj(this,-3)="") D vRCrequiredErr("TOKEN")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("TOKEN")
 I (vobj(this,-4)="") D vRCrequiredErr("BUFFER")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBBUF","MSG",1767,"DBBUF."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("TOKEN",$$^MSG(1076,12))
 I ($L(vobj(this,-4))>40) D vRCvalidateDDerr("BUFFER",$$^MSG(1076,40))
 I '($P(vobj(this),$C(124),2)=""),'(+$P(vobj(this),$C(124),2)=$P(vobj(this),$C(124),2))  S $P(vobj(this),$C(124),2)=$$vRCtrimNumber($P(vobj(this),$C(124),2))
 S X=$P(vobj(this),$C(124),2) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("BEFD",$$^MSG(742,"N"))
 I '($P(vobj(this),$C(124),1)=""),'(+$P(vobj(this),$C(124),1)=$P(vobj(this),$C(124),1))  S $P(vobj(this),$C(124),1)=$$vRCtrimNumber($P(vobj(this),$C(124),1))
 S X=$P(vobj(this),$C(124),1) I '(X=""),(X'?1.2N),(X'?1"-"1.1N) D vRCvalidateDDerr("BUFSTAT",$$^MSG(742,"N"))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","TOKEN"))&($P($E($G(vobj(this,-100,"1*","TOKEN")),5,9999),$C(124))'=vobj(this,-3))) I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("TOKEN",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"2*","BUFFER"))&($P($E($G(vobj(this,-100,"2*","BUFFER")),5,9999),$C(124))'=vobj(this,-4))) I ($L(vobj(this,-4))>40) D vRCvalidateDDerr("BUFFER",$$^MSG(1076,40))
 I ($D(vobj(this,-100,"0*","BEFD"))&($P($E($G(vobj(this,-100,"0*","BEFD")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))),'($P(vobj(this),$C(124),2)=""),'(+$P(vobj(this),$C(124),2)=$P(vobj(this),$C(124),2))  S $P(vobj(this),$C(124),2)=$$vRCtrimNumber($P(vobj(this),$C(124),2))
 I ($D(vobj(this,-100,"0*","BEFD"))&($P($E($G(vobj(this,-100,"0*","BEFD")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) S X=$P(vobj(this),$C(124),2) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("BEFD",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"0*","BUFSTAT"))&($P($E($G(vobj(this,-100,"0*","BUFSTAT")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))),'($P(vobj(this),$C(124),1)=""),'(+$P(vobj(this),$C(124),1)=$P(vobj(this),$C(124),1))  S $P(vobj(this),$C(124),1)=$$vRCtrimNumber($P(vobj(this),$C(124),1))
 I ($D(vobj(this,-100,"0*","BUFSTAT"))&($P($E($G(vobj(this,-100,"0*","BUFSTAT")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) S X=$P(vobj(this),$C(124),1) I '(X=""),(X'?1.2N),(X'?1"-"1.1N) D vRCvalidateDDerr("BUFSTAT",$$^MSG(742,"N"))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBBUF","MSG",979,"DBBUF."_column_" "_errmsg)
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
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","TOKEN")):$P($E(vobj(this,-100,"1*","TOKEN"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","BUFFER")):$P($E(vobj(this,-100,"2*","BUFFER"),5,9999),$C(124)),1:vobj(this,-4))
  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I ($D(^DBBUF(V1,V2))#2) D throwError($$^MSG(2327))
 S newkeys=newKey1_","_newKey2
 S oldkeys=oldKey1_","_oldKey2
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
 S vRCparams=$$setPar^UCUTILN(vRCparams,"NOINDEX")
 I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,1)
 D vRCmiscValidations(this,vRCparams,1)
 D vRCupdateDB(this,1,vRCparams,.vRCaudit,.vRCauditIns)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
 N newrec S newrec=$$vReCp1(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBBUF(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("DBBUF",oldkeys,newkeys)
 .	I ER D throwError($get(RM))
 .	Q 
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
 S vRCparams=$$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/")
 D vRCdelete(this,vRCparams,.vRCaudit,1)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
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
vReCp1(v1) ; RecordDBBUF.copy: DBBUF
 ;
 Q $$copy^UCGMR(this)
