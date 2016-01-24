 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordDBTBL8 ****
 ; 
 ; 02/24/2010 18:40 - pip
 ; 
 ;
 ; Record Class code for table DBTBL8
 ;
 ; Generated by PSLRecordBuilder on 02/24/2010 at 18:40 by
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL8",vobj(vOid,-2)=0,vobj(vOid)=""
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL8"
 S vobj(vOid)=$G(^DBTBL(v1,8,v2,v3))
 I vobj(vOid)="",'($D(^DBTBL(v1,8,v2,v3))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBTBL8",$EC=",U1001,"
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL8"
 S vobj(vOid)=$G(^DBTBL(v1,8,v2,v3))
 I vobj(vOid)="",'($D(^DBTBL(v1,8,v2,v3))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 S vobj(vOid,-5)=v3
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,v3,vfromDbSet,v2out) ; 
 N dbtbl8
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbtbl8=$G(^DBTBL(v1,8,v2,v3))
 I dbtbl8="",'($D(^DBTBL(v1,8,v2,v3))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBTBL8",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q dbtbl8
 ;
vRCgetRecord1Opt(v1,v2,v3,vfromDbSet,v2out) ; 
 N dbtbl8
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbtbl8=$G(^DBTBL(v1,8,v2,v3))
 I dbtbl8="",'($D(^DBTBL(v1,8,v2,v3))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q dbtbl8
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
 .	I (("/"_vRCparams_"/")["/TRIGAFT/") D vRCafterInsTrigs(this,vRCparams)
 .	Q 
 E  I (%O=1) D
 .	D AUDIT^UCUTILN(this,.vRCaudit,1,"|")
 .	I ($D(vobj(this,-100,"1*","%LIBS"))&($P($E($G(vobj(this,-100,"1*","%LIBS")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","FID"))&($P($E($G(vobj(this,-100,"2*","FID")),5,9999),$C(124))'=vobj(this,-4)))!($D(vobj(this,-100,"3*","INDEXNM"))&($P($E($G(vobj(this,-100,"3*","INDEXNM")),5,9999),$C(124))'=vobj(this,-5))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForUpdate(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD1(this)
 .	D vRCmiscValidations(this,vRCparams,%O)
 .	D vRCupdateDB(this,%O,vRCparams,.vRCaudit,.vRCauditIns)
 .	I (("/"_vRCparams_"/")["/TRIGAFT/") D vRCafterUpdTrigs(this,vRCparams)
 .	Q 
 E  I (%O=2) D
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,%O)
 .	D vRCmiscValidations(this,vRCparams,2)
 .	I (("/"_vRCparams_"/")["/TRIGAFT/") D vRCafterInsTrigs(this,vRCparams)
 .	Q 
 E  I (%O=3) D
 .	  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) Q:'($D(^DBTBL(V1,8,V2,V3))#2) 
 .	D vRCdelete(this,vRCparams,.vRCaudit,0)
 .	I (("/"_vRCparams_"/")["/TRIGAFT/") D vRCafterDelTrigs(this,vRCparams)
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
vselectOptmOK(userclass,dbtbl8,vkey1,vkey2,vkey3) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("%LIBS,FID,GLOBAL,IDXDESC,INDEXNM,LTD,ORDERBY,PARFID,TIME,UPCASE,USER",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-5)="INDEXNM:U:"
 S map(-4)="FID:U:"
 S map(-3)="%LIBS:T:"
 S map(-1)="GLOBAL:T:2;IDXDESC:T:5;LTD:D:12;ORDERBY:T:3;PARFID:T:15;TIME:C:16;UPCASE:L:14;USER:T:13"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N dbtbl8 S dbtbl8=$$vRCgetRecord0^RecordDBTBL8(%LIBS,FID,INDEXNM,0)
 .	S vobj(dbtbl8,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL8(dbtbl8,$$initPar^UCUTILN(params)) K vobj(dbtbl8,-100) S vobj(dbtbl8,-2)=1 TC:vTp  
 .	K vobj(+$G(dbtbl8)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) I '(''($D(^DBTBL(V1,8,V2,V3))#2)=''processMode) D
 .	N errmsg
 .	I (+processMode'=+0) S errmsg=$$^MSG(7932)
 .	E  S errmsg=$$^MSG(2327)
 .	D throwError(errmsg)
 .	Q 
 I (("/"_vRCparams_"/")["/VALFK/") D vRCcheckForeignKeys(this)
 I (("/"_vRCparams_"/")["/VALRI/") D vRCsetForeignKeys(this)
 Q 
 ;
vRCupdateDB(this,processMode,vRCparams,vRCaudit,vRCauditIns) ; 
 I '(("/"_vRCparams_"/")["/NOUPDATE/") D
 .  S $P(vobj(this),$C(124),12)=$P($H,",",1)
 .  S $P(vobj(this),$C(124),16)=$P($H,",",2)
 .	I '(+$P($G(vobj(this,-100,"0*","USER")),$C(124),2)&($P($E($G(vobj(this,-100,"0*","USER")),5,9999),$C(124))'=$P(vobj(this),$C(124),13)))  S $P(vobj(this),$C(124),13)=$E($$USERNAM^%ZFUNC,1,20)
 .	;   #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	if $D(vobj(this)) S ^DBTBL(vobj(this,-3),8,vobj(this,-4),vobj(this,-5))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^DBTBL(vobj(this,-3),8,vobj(this,-4),vobj(this,-5))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCsetDefaults(this) ; 
 I ($P(vobj(this),$C(124),14)="")  S $P(vobj(this),$C(124),14)=0
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("%LIBS")
 I (vobj(this,-4)="") D vRCrequiredErr("FID")
 I ($P(vobj(this),$C(124),5)="") D vRCrequiredErr("IDXDESC")
 I (vobj(this,-5)="") D vRCrequiredErr("INDEXNM")
 I ($P(vobj(this),$C(124),14)="") D vRCrequiredErr("UPCASE")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("%LIBS")
 I (vobj(this,-4)="") D vRCrequiredErr("FID")
 I (vobj(this,-5)="") D vRCrequiredErr("INDEXNM")
 I ($D(vobj(this,-100,"0*","IDXDESC"))&($P($E($G(vobj(this,-100,"0*","IDXDESC")),5,9999),$C(124))'=$P(vobj(this),$C(124),5))),($P(vobj(this),$C(124),5)="") D vRCrequiredErr("IDXDESC")
 I ($D(vobj(this,-100,"0*","UPCASE"))&($P($E($G(vobj(this,-100,"0*","UPCASE")),5,9999),$C(124))'=$P(vobj(this),$C(124),14))),($P(vobj(this),$C(124),14)="") D vRCrequiredErr("UPCASE")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBTBL8","MSG",1767,"DBTBL8."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCsetForeignKeys(this) ; 
 I '(vobj(this,-4)="") S vfkey("^DBTBL("_""""_vobj(this,-3)_""""_","_1_","_""""_vobj(this,-4)_""""_")")="DBTBL8(%LIBS,FID) -> DBTBL1"
 Q 
 ;
vRCcheckForeignKeys(this) ; 
  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I '($D(^DBTBL(V1,1,V2))) D throwError($$^MSG(8563,"DBTBL8(%LIBS,FID) -> DBTBL1"))
 Q 
 ;
vRCTrig1(this,dbtbl8,vpar) ; Trigger AFTER_DELETE - After delete - AD
 ;
 N ZFID N IDXNM N SUBFID
 ;
 S ZFID=vobj(dbtbl8,-4)
 S IDXNM=vobj(dbtbl8,-5)
 ;
 N rs,vos1,vos2,vos3,vos4 S rs=$$vOpen1()
 I '$G(vos1) Q 
 F  Q:'$$vFetch1()  D
 . S SUBFID=rs
 .	D vDbDe1()
 .	Q 
 Q 
 ;
vRCTrig2(this,dbtbl8,vpar) ; Trigger AFTER_UPDATE - After insert/update - AI AU
 N vTp
 ;
 N ZFID N IDXNM N subfid
 ;
 S ZFID=vobj(dbtbl8,-4)
 S IDXNM=vobj(dbtbl8,-5)
 ;
 ; Copy super type index information into sub type file
 ;
 N sub S sub=$$vReCp1(dbtbl8)
 ;
 N rs,vos1,vos2,vos3,vos4 S rs=$$vOpen2()
 I '$G(vos1) K vobj(+$G(sub)) Q 
 F  Q:'$$vFetch2()  D
 . S subfid=rs
 .  S vobj(sub,-4)=subfid
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL8(sub,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(sub,-100) S vobj(sub,-2)=1 TC:vTp  
 .	Q 
 K vobj(+$G(sub)) Q 
 ;
vRCafterInsTrigs(this,vRCparams) ; 
 N ER S ER=0
 N vRCfire
 N RM S RM=""
 N %LIBS S %LIBS=vobj(this,-3)
 N FID S FID=vobj(this,-4)
 N INDEXNM S INDEXNM=vobj(this,-5)
 D vRCTrig2(this,this,vRCparams) I ER D throwError($get(RM))
 Q 
 ;
vRCafterUpdTrigs(this,vRCparams) ; 
 N ER S ER=0
 N vRCfire
 N RM S RM=""
 N %LIBS S %LIBS=vobj(this,-3)
 N FID S FID=vobj(this,-4)
 N INDEXNM S INDEXNM=vobj(this,-5)
 Q:'$D(vobj(this,-100)) 
 D vRCTrig2(this,this,vRCparams) I ER D throwError($get(RM))
 Q 
 ;
vRCafterDelTrigs(this,vRCparams) ; 
 N ER S ER=0
 N vRCfire
 N RM S RM=""
 N %LIBS S %LIBS=vobj(this,-3)
 N FID S FID=vobj(this,-4)
 N INDEXNM S INDEXNM=vobj(this,-5)
 D vRCTrig1(this,this,vRCparams) I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,12))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("FID",$$^MSG(1476))
 I ($L(vobj(this,-4))>25) D vRCvalidateDDerr("FID",$$^MSG(1076,25))
 S X=vobj(this,-5) I '(X="") S errmsg=$$VAL^DBSVER("U",16,1,,"X?1A.AN") I '(errmsg="") D vRCvalidateDDerr("INDEXNM",$$^MSG(979,"DBTBL8.INDEXNM"_" "_errmsg))
 I ($L($P(vobj(this),$C(124),2))>40) D vRCvalidateDDerr("GLOBAL",$$^MSG(1076,40))
 I ($L($P(vobj(this),$C(124),5))>29) D vRCvalidateDDerr("IDXDESC",$$^MSG(1076,29))
 S X=$P(vobj(this),$C(124),12) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("LTD",$$^MSG(742,"D"))
 I ($L($P(vobj(this),$C(124),3))>120) D vRCvalidateDDerr("ORDERBY",$$^MSG(1076,120))
 I ($L($P(vobj(this),$C(124),15))>8) D vRCvalidateDDerr("PARFID",$$^MSG(1076,8))
 S X=$P(vobj(this),$C(124),16) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIME",$$^MSG(742,"C"))
 I '(($P(vobj(this),$C(124),14)=1)!($P(vobj(this),$C(124),14)=0)) D vRCvalidateDDerr("UPCASE",$$^MSG(742,"L"))
 I ($L($P(vobj(this),$C(124),13))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","%LIBS"))&($P($E($G(vobj(this,-100,"1*","%LIBS")),5,9999),$C(124))'=vobj(this,-3))) I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,12))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("FID",$$^MSG(1476))
 I ($D(vobj(this,-100,"2*","FID"))&($P($E($G(vobj(this,-100,"2*","FID")),5,9999),$C(124))'=vobj(this,-4))) I ($L(vobj(this,-4))>25) D vRCvalidateDDerr("FID",$$^MSG(1076,25))
 I ($D(vobj(this,-100,"3*","INDEXNM"))&($P($E($G(vobj(this,-100,"3*","INDEXNM")),5,9999),$C(124))'=vobj(this,-5))) S X=vobj(this,-5) I '(X="") S errmsg=$$VAL^DBSVER("U",16,1,,"X?1A.AN") I '(errmsg="") D vRCvalidateDDerr("INDEXNM",$$^MSG(979,"DBTBL8.INDEXNM"_" "_errmsg))
 I ($D(vobj(this,-100,"0*","GLOBAL"))&($P($E($G(vobj(this,-100,"0*","GLOBAL")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) I ($L($P(vobj(this),$C(124),2))>40) D vRCvalidateDDerr("GLOBAL",$$^MSG(1076,40))
 I ($D(vobj(this,-100,"0*","IDXDESC"))&($P($E($G(vobj(this,-100,"0*","IDXDESC")),5,9999),$C(124))'=$P(vobj(this),$C(124),5))) I ($L($P(vobj(this),$C(124),5))>29) D vRCvalidateDDerr("IDXDESC",$$^MSG(1076,29))
 I ($D(vobj(this,-100,"0*","LTD"))&($P($E($G(vobj(this,-100,"0*","LTD")),5,9999),$C(124))'=$P(vobj(this),$C(124),12))) S X=$P(vobj(this),$C(124),12) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("LTD",$$^MSG(742,"D"))
 I ($D(vobj(this,-100,"0*","ORDERBY"))&($P($E($G(vobj(this,-100,"0*","ORDERBY")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))) I ($L($P(vobj(this),$C(124),3))>120) D vRCvalidateDDerr("ORDERBY",$$^MSG(1076,120))
 I ($D(vobj(this,-100,"0*","PARFID"))&($P($E($G(vobj(this,-100,"0*","PARFID")),5,9999),$C(124))'=$P(vobj(this),$C(124),15))) I ($L($P(vobj(this),$C(124),15))>8) D vRCvalidateDDerr("PARFID",$$^MSG(1076,8))
 I ($D(vobj(this,-100,"0*","TIME"))&($P($E($G(vobj(this,-100,"0*","TIME")),5,9999),$C(124))'=$P(vobj(this),$C(124),16))) S X=$P(vobj(this),$C(124),16) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TIME",$$^MSG(742,"C"))
 I ($D(vobj(this,-100,"0*","UPCASE"))&($P($E($G(vobj(this,-100,"0*","UPCASE")),5,9999),$C(124))'=$P(vobj(this),$C(124),14))) I '(($P(vobj(this),$C(124),14)=1)!($P(vobj(this),$C(124),14)=0)) D vRCvalidateDDerr("UPCASE",$$^MSG(742,"L"))
 I ($D(vobj(this,-100,"0*","USER"))&($P($E($G(vobj(this,-100,"0*","USER")),5,9999),$C(124))'=$P(vobj(this),$C(124),13))) I ($L($P(vobj(this),$C(124),13))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBTBL8","MSG",979,"DBTBL8."_column_" "_errmsg)
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
 N oldparams S oldparams=vRCparams
 N newKey1 S newKey1=vobj(this,-3)
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","%LIBS")):$P($E(vobj(this,-100,"1*","%LIBS"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","FID")):$P($E(vobj(this,-100,"2*","FID"),5,9999),$C(124)),1:vobj(this,-4))
 N newKey3 S newKey3=vobj(this,-5)
 N oldKey3 S oldKey3=$S($D(vobj(this,-100,"3*","INDEXNM")):$P($E(vobj(this,-100,"3*","INDEXNM"),5,9999),$C(124)),1:vobj(this,-5))
  N V1,V2,V3 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5) I ($D(^DBTBL(V1,8,V2,V3))#2) D throwError($$^MSG(2327))
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
 N newrec S newrec=$$vReCp2(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL8(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("DBTBL8",oldkeys,newkeys)
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
 S vRCparams=oldparams
 I (("/"_vRCparams_"/")["/TRIGAFT/") D vRCafterUpdTrigs(this,vRCparams)
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
vDbDe1() ; DELETE FROM DBTBL8 WHERE %LIBS='SYSDEV' AND FID=:SUBFID AND INDEXNM=:IDXNM
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 TS (vobj):transactionid="CS"
 N vRec S vRec=$$vRCgetRecord1^RecordDBTBL8("SYSDEV",SUBFID,IDXNM,0)
 I $G(vobj(vRec,-2))=1 S vobj(vRec,-2)=3 D
 .	;     #ACCEPT Date=07/09/2008; Pgm=RussellDS; CR=30801; Group=BYPASS
 .	;*** Start of code by-passed by compiler
 .	D vSave^RecordDBTBL8(vRec,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/",0)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 K vobj(+$G(vRec)) Q 
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
vOpen1() ; FID FROM DBTBL1 WHERE %LIBS='SYSDEV' AND PARFID=:ZFID
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(ZFID) I vos3="",'$D(ZFID) G vL1a0
 S vos4=""
vL1a4 S vos4=$O(^DBINDX("SYSDEV","PARFID",vos3,vos4),1) I vos4="" G vL1a0
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S rs=$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen2() ; FID FROM DBTBL1 WHERE %LIBS='SYSDEV' AND PARFID=:ZFID
 ;
 ;
 S vos1=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos1=0 Q
vL2a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(ZFID) I vos3="",'$D(ZFID) G vL2a0
 S vos4=""
vL2a4 S vos4=$O(^DBINDX("SYSDEV","PARFID",vos3,vos4),1) I vos4="" G vL2a0
 Q
 ;
vFetch2() ;
 ;
 ;
 I vos1=1 D vL2a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S rs=$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vReCp1(v1) ; RecordDBTBL8.copy: DBTBL8
 ;
 Q $$copy^UCGMR(dbtbl8)
 ;
vReCp2(v1) ; RecordDBTBL8.copy: DBTBL8
 ;
 Q $$copy^UCGMR(this)