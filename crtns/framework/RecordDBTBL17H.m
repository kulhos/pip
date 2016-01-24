 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordDBTBL17H ****
 ; 
 ; 02/24/2010 18:39 - pip
 ; 
 ;
 ; Record Class code for table DBTBL17H
 ;
 ; Generated by PSLRecordBuilder on 02/24/2010 at 18:39 by
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL17H",vobj(vOid,-2)=0,vobj(vOid)=""
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL17H"
 S vobj(vOid)=$G(^DBTBL(v1,17,v2))
 I vobj(vOid)="",'($D(^DBTBL(v1,17,v2))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBTBL17H",$EC=",U1001,"
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBTBL17H"
 S vobj(vOid)=$G(^DBTBL(v1,17,v2))
 I vobj(vOid)="",'($D(^DBTBL(v1,17,v2))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,vfromDbSet,v2out) ; 
 N dbtbl17h
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbtbl17h=$G(^DBTBL(v1,17,v2))
 I dbtbl17h="",'($D(^DBTBL(v1,17,v2))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBTBL17H",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q dbtbl17h
 ;
vRCgetRecord1Opt(v1,v2,vfromDbSet,v2out) ; 
 N dbtbl17h
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dbtbl17h=$G(^DBTBL(v1,17,v2))
 I dbtbl17h="",'($D(^DBTBL(v1,17,v2))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q dbtbl17h
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
 .	I ($D(vobj(this,-100,"1*","%LIBS"))&($P($E($G(vobj(this,-100,"1*","%LIBS")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","EXPID"))&($P($E($G(vobj(this,-100,"2*","EXPID")),5,9999),$C(124))'=vobj(this,-4))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) Q:'($D(^DBTBL(V1,17,V2))#2) 
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
vselectOptmOK(userclass,dbtbl17h,vkey1,vkey2) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("%LIBS,DESC,EXPID,TLD,USER",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-4)="EXPID:U:"
 S map(-3)="%LIBS:T:"
 S map(-1)="DESC:T:1;TLD:D:2;USER:T:3"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N dbtbl17h S dbtbl17h=$$vRCgetRecord0^RecordDBTBL17H(%LIBS,EXPID,0)
 .	S vobj(dbtbl17h,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL17H(dbtbl17h,$$initPar^UCUTILN(params)) K vobj(dbtbl17h,-100) S vobj(dbtbl17h,-2)=1 TC:vTp  
 .	K vobj(+$G(dbtbl17h)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I '(''($D(^DBTBL(V1,17,V2))#2)=''processMode) D
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
 .	if $D(vobj(this)) S ^DBTBL(vobj(this,-3),17,vobj(this,-4))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^DBTBL(vobj(this,-3),17,vobj(this,-4))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("%LIBS")
 I (vobj(this,-4)="") D vRCrequiredErr("EXPID")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("%LIBS")
 I (vobj(this,-4)="") D vRCrequiredErr("EXPID")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBTBL17H","MSG",1767,"DBTBL17H."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,12))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("EXPID",$$^MSG(1476))
 I ($L(vobj(this,-4))>12) D vRCvalidateDDerr("EXPID",$$^MSG(1076,12))
 I ($L($P(vobj(this),$C(124),1))>65) D vRCvalidateDDerr("DESC",$$^MSG(1076,65))
 S X=$P(vobj(this),$C(124),2) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TLD",$$^MSG(742,"D"))
 I ($L($P(vobj(this),$C(124),3))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","%LIBS"))&($P($E($G(vobj(this,-100,"1*","%LIBS")),5,9999),$C(124))'=vobj(this,-3))) I ($L(vobj(this,-3))>12) D vRCvalidateDDerr("%LIBS",$$^MSG(1076,12))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("EXPID",$$^MSG(1476))
 I ($D(vobj(this,-100,"2*","EXPID"))&($P($E($G(vobj(this,-100,"2*","EXPID")),5,9999),$C(124))'=vobj(this,-4))) I ($L(vobj(this,-4))>12) D vRCvalidateDDerr("EXPID",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"0*","DESC"))&($P($E($G(vobj(this,-100,"0*","DESC")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) I ($L($P(vobj(this),$C(124),1))>65) D vRCvalidateDDerr("DESC",$$^MSG(1076,65))
 I ($D(vobj(this,-100,"0*","TLD"))&($P($E($G(vobj(this,-100,"0*","TLD")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) S X=$P(vobj(this),$C(124),2) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TLD",$$^MSG(742,"D"))
 I ($D(vobj(this,-100,"0*","USER"))&($P($E($G(vobj(this,-100,"0*","USER")),5,9999),$C(124))'=$P(vobj(this),$C(124),3))) I ($L($P(vobj(this),$C(124),3))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBTBL17H","MSG",979,"DBTBL17H."_column_" "_errmsg)
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
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","%LIBS")):$P($E(vobj(this,-100,"1*","%LIBS"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","EXPID")):$P($E(vobj(this,-100,"2*","EXPID"),5,9999),$C(124)),1:vobj(this,-4))
  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I ($D(^DBTBL(V1,17,V2))#2) D throwError($$^MSG(2327))
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
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL17H(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("DBTBL17H",oldkeys,newkeys)
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
vReCp1(v1) ; RecordDBTBL17H.copy: DBTBL17H
 ;
 Q $$copy^UCGMR(this)
