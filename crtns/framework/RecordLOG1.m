 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordLOG1 ****
 ; 
 ; 01/23/2016 18:58 - kulhan
 ; 
 ;
 ; Record Class code for table LOG1
 ;
 ; Generated by PSLRecordBuilder on 01/23/2016 at 18:58 by 1
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordLOG1",vobj(vOid,-2)=0,vobj(vOid)=""
 S vobj(vOid,-3)=""
 S vobj(vOid,-4)=""
 S vobj(vOid,-5)=""
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0(v1,v2,v3,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordLOG1"
 S vobj(vOid)=$G(^LOG(v1,v2,v3))
 I '(v1>0)
 E  I vobj(vOid)="",'($D(^LOG(v1,v2,v3))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,LOG1",$EC=",U1001,"
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 S vobj(vOid,-5)=v3
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord1(v1,v2,v3,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordLOG1"
 S vobj(vOid)=$G(^LOG(v1,v2,v3))
 I '(v1>0)
 E  I vobj(vOid)="",'($D(^LOG(v1,v2,v3))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 S vobj(vOid,-5)=v3
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,v3,vfromDbSet,v2out) ; 
 N log1
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S log1=$G(^LOG(v1,v2,v3))
 I '(v1>0)
 E  I log1="",'($D(^LOG(v1,v2,v3))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,LOG1",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q log1
 ;
vRCgetRecord1Opt(v1,v2,v3,vfromDbSet,v2out) ; 
 N log1
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S log1=$G(^LOG(v1,v2,v3))
 I '(v1>0)
 E  I log1="",'($D(^LOG(v1,v2,v3))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q log1
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
 .	I ($D(vobj(this,-100,"1*","TJD"))&($P($E($G(vobj(this,-100,"1*","TJD")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","SEQ"))&($P($E($G(vobj(this,-100,"2*","SEQ")),5,9999),$C(124))'=vobj(this,-4)))!($D(vobj(this,-100,"3*","SUBSEQ"))&($P($E($G(vobj(this,-100,"3*","SUBSEQ")),5,9999),$C(124))'=vobj(this,-5))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) Q:'$$vDbEx1() 
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
vselectOptmOK(userclass,log1,vkey1,vkey2,vkey3) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("DILIST,DVLIST,OVLIST,SEQ,SUBSEQ,TJD",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-5)="SUBSEQ:N:"
 S map(-4)="SEQ:N:"
 S map(-3)="TJD:D:"
 S map(-1)="DILIST:T:1;DVLIST:T:2;OVLIST:T:3"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N log1 S log1=$$vRCgetRecord0^RecordLOG1(TJD,SEQ,SUBSEQ,0)
 .	S vobj(log1,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordLOG1(log1,$$initPar^UCUTILN(params)) K vobj(log1,-100) S vobj(log1,-2)=1 TC:vTp  
 .	K vobj(+$G(log1)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) I '(''$$vDbEx2()=''processMode) D
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
 .	if $D(vobj(this)) S ^LOG(vobj(this,-3),vobj(this,-4),vobj(this,-5))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^LOG(vobj(this,-3),vobj(this,-4),vobj(this,-5))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I (vobj(this,-4)="") D vRCrequiredErr("SEQ")
 I (vobj(this,-5)="") D vRCrequiredErr("SUBSEQ")
 I (vobj(this,-3)="") D vRCrequiredErr("TJD")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("TJD")
 I (vobj(this,-4)="") D vRCrequiredErr("SEQ")
 I (vobj(this,-5)="") D vRCrequiredErr("SUBSEQ")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("LOG1","MSG",1767,"LOG1."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 S X=vobj(this,-3) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TJD",$$^MSG(742,"D"))
 I '(vobj(this,-4)=""),'(+vobj(this,-4)=vobj(this,-4))  S vobj(this,-4)=$$vRCtrimNumber(vobj(this,-4))
 S X=vobj(this,-4) I '(X=""),(X'?1.18N),(X'?1"-"1.17N) D vRCvalidateDDerr("SEQ",$$^MSG(742,"N"))
 I '(vobj(this,-5)=""),'(+vobj(this,-5)=vobj(this,-5))  S vobj(this,-5)=$$vRCtrimNumber(vobj(this,-5))
 S X=vobj(this,-5) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("SUBSEQ",$$^MSG(742,"N"))
 I ($L($P(vobj(this),$C(124),1))>256) D vRCvalidateDDerr("DILIST",$$^MSG(1076,256))
 I ($L($P(vobj(this),$C(124),2))>256) D vRCvalidateDDerr("DVLIST",$$^MSG(1076,256))
 I ($L($P(vobj(this),$C(124),3))>256) D vRCvalidateDDerr("OVLIST",$$^MSG(1076,256))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","TJD"))&($P($E($G(vobj(this,-100,"1*","TJD")),5,9999),$C(124))'=vobj(this,-3))) S X=vobj(this,-3) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TJD",$$^MSG(742,"D"))
 I ($D(vobj(this,-100,"2*","SEQ"))&($P($E($G(vobj(this,-100,"2*","SEQ")),5,9999),$C(124))'=vobj(this,-4))),'(vobj(this,-4)=""),'(+vobj(this,-4)=vobj(this,-4))  S vobj(this,-4)=$$vRCtrimNumber(vobj(this,-4))
 I ($D(vobj(this,-100,"2*","SEQ"))&($P($E($G(vobj(this,-100,"2*","SEQ")),5,9999),$C(124))'=vobj(this,-4))) S X=vobj(this,-4) I '(X=""),(X'?1.18N),(X'?1"-"1.17N) D vRCvalidateDDerr("SEQ",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"3*","SUBSEQ"))&($P($E($G(vobj(this,-100,"3*","SUBSEQ")),5,9999),$C(124))'=vobj(this,-5))),'(vobj(this,-5)=""),'(+vobj(this,-5)=vobj(this,-5))  S vobj(this,-5)=$$vRCtrimNumber(vobj(this,-5))
 I ($D(vobj(this,-100,"3*","SUBSEQ"))&($P($E($G(vobj(this,-100,"3*","SUBSEQ")),5,9999),$C(124))'=vobj(this,-5))) S X=vobj(this,-5) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("SUBSEQ",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"0*","DILIST"))&($P($E($G(vobj(this,-100,"0*","DILIST")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) I ($L($P(vobj(this),$C(124),1))>256) D vRCvalidateDDerr("DILIST",$$^MSG(1076,256))
 I ($D(vobj(this,-100,"0*","DVLIST"))&($P($E($G(vobj(this,-100,"0*","DVLIST")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) I ($L($P(vobj(this),$C(124),2))>256) D vRCvalidateDDerr("DVLIST",$$^MSG(1076,256))
 I ($D(vobj(this,-100,"0*","OVLIST"))&($P($E($G(vobj(this,-100,"0*","OVLIST")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))) I ($L($P(vobj(this),$C(124),3))>256) D vRCvalidateDDerr("OVLIST",$$^MSG(1076,256))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("LOG1","MSG",979,"LOG1."_column_" "_errmsg)
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
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","TJD")):$P($E(vobj(this,-100,"1*","TJD"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","SEQ")):$P($E(vobj(this,-100,"2*","SEQ"),5,9999),$C(124)),1:vobj(this,-4))
 N newKey3 S newKey3=vobj(this,-5)
 N oldKey3 S oldKey3=$S($D(vobj(this,-100,"3*","SUBSEQ")):$P($E(vobj(this,-100,"3*","SUBSEQ"),5,9999),$C(124)),1:vobj(this,-5))
  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) I $$vDbEx3() D throwError($$^MSG(2327))
 S newkeys=newKey1_","_newKey2_","_newKey3
 S oldkeys=oldKey1_","_oldKey2_","_oldKey3
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
  S vobj(this,-5)=oldKey3
 S vRCparams=$$setPar^UCUTILN(vRCparams,"NOINDEX")
 I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,1)
 D vRCmiscValidations(this,vRCparams,1)
 D vRCupdateDB(this,1,vRCparams,.vRCaudit,.vRCauditIns)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
  S vobj(this,-5)=newKey3
 N newrec S newrec=$$vReCp1(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordLOG1(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("LOG1",oldkeys,newkeys)
 .	I ER D throwError($get(RM))
 .	Q 
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
  S vobj(this,-5)=oldKey3
 S vRCparams=$$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/")
 D vRCdelete(this,vRCparams,.vRCaudit,1)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
  S vobj(this,-5)=newKey3
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
vDbEx1() ; min(1): DISTINCT TJD,SEQ,SUBSEQ FROM LOG1 WHERE TJD=:V1 and SEQ=:V2 and SUBSEQ=:V3
 ;
 N vsql1
 S vsql1=$$BYTECHAR^SQLUTL(254)
 ;
 ;
 ;
 ;
 I '($D(^LOG(V1,V2,V3))#2) Q 0
 I '(V1>0) Q 0
 Q 1
 ;
vDbEx2() ; min(1): DISTINCT TJD,SEQ,SUBSEQ FROM LOG1 WHERE TJD=:V1 and SEQ=:V2 and SUBSEQ=:V3
 ;
 N vsql1
 S vsql1=$$BYTECHAR^SQLUTL(254)
 ;
 ;
 ;
 ;
 I '($D(^LOG(V1,V2,V3))#2) Q 0
 I '(V1>0) Q 0
 Q 1
 ;
vDbEx3() ; min(1): DISTINCT TJD,SEQ,SUBSEQ FROM LOG1 WHERE TJD=:V1 and SEQ=:V2 and SUBSEQ=:V3
 ;
 N vsql1
 S vsql1=$$BYTECHAR^SQLUTL(254)
 ;
 ;
 ;
 ;
 I '($D(^LOG(V1,V2,V3))#2) Q 0
 I '(V1>0) Q 0
 Q 1
 ;
vReCp1(v1) ; RecordLOG1.copy: LOG1
 ;
 Q $$copy^UCGMR(this)
