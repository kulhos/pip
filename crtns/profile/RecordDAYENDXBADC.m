 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordDAYENDXBADC ****
 ; 
 ; 02/24/2010 18:39 - pip
 ; 
 ;
 ; Record Class code for table DAYENDXBADC
 ;
 ; Generated by PSLRecordBuilder on 02/24/2010 at 18:39 by
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDAYENDXBADC",vobj(vOid,-2)=0,vobj(vOid)=""
 S vobj(vOid,-3)=""
 S vobj(vOid,-4)=""
 S vobj(vOid,-5)=""
 S vobj(vOid,-6)=""
 S vobj(vOid,-7)=""
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0(v1,v2,v3,v4,v5,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDAYENDXBADC"
 S vobj(vOid)=$G(^DAYEND(v1,"XBADC",v2,v3,v4,v5))
 I vobj(vOid)="",'($D(^DAYEND(v1,"XBADC",v2,v3,v4,v5))#2)
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DAYENDXBADC",$EC=",U1001,"
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 S vobj(vOid,-5)=v3
 S vobj(vOid,-6)=v4
 S vobj(vOid,-7)=v5
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord1(v1,v2,v3,v4,v5,vfromDbSet) ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDAYENDXBADC"
 S vobj(vOid)=$G(^DAYEND(v1,"XBADC",v2,v3,v4,v5))
 I vobj(vOid)="",'($D(^DAYEND(v1,"XBADC",v2,v3,v4,v5))#2)
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 S vobj(vOid,-5)=v3
 S vobj(vOid,-6)=v4
 S vobj(vOid,-7)=v5
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,v3,v4,v5,vfromDbSet,v2out) ; 
 N dayendxbadc
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dayendxbadc=$G(^DAYEND(v1,"XBADC",v2,v3,v4,v5))
 I dayendxbadc="",'($D(^DAYEND(v1,"XBADC",v2,v3,v4,v5))#2)
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DAYENDXBADC",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q dayendxbadc
 ;
vRCgetRecord1Opt(v1,v2,v3,v4,v5,vfromDbSet,v2out) ; 
 N dayendxbadc
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S dayendxbadc=$G(^DAYEND(v1,"XBADC",v2,v3,v4,v5))
 I dayendxbadc="",'($D(^DAYEND(v1,"XBADC",v2,v3,v4,v5))#2)
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q dayendxbadc
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
 .	I ($D(vobj(this,-100,"1*","TJD"))&($P($E($G(vobj(this,-100,"1*","TJD")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","%UID"))&($P($E($G(vobj(this,-100,"2*","%UID")),5,9999),$C(124))'=vobj(this,-4)))!($D(vobj(this,-100,"3*","ACN"))&($P($E($G(vobj(this,-100,"3*","ACN")),5,9999),$C(124))'=vobj(this,-5)))!($D(vobj(this,-100,"4*","SEQ"))&($P($E($G(vobj(this,-100,"4*","SEQ")),5,9999),$C(124))'=vobj(this,-6)))!($D(vobj(this,-100,"5*","ET"))&($P($E($G(vobj(this,-100,"5*","ET")),5,9999),$C(124))'=vobj(this,-7))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1,V2,V3,V4,V5 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5),V4=vobj(this,-6),V5=vobj(this,-7) Q:'($D(^DAYEND(V1,"XBADC",V2,V3,V4,V5))#2) 
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
vselectOptmOK(userclass,dayendxbadc,vkey1,vkey2,vkey3,vkey4,vkey5) ; PUBLIC access is allowed, no restrict clause
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
 Q $$vStrRep("%UID,ACN,ET,IDENT,SEQ,TJD,UID",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-7)="ET:T:"
 S map(-6)="SEQ:N:"
 S map(-5)="ACN:N:"
 S map(-4)="%UID:T:"
 S map(-3)="TJD:D:"
 S map(-1)="IDENT:T:2;UID:T:1"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N dayendxbadc S dayendxbadc=$$vRCgetRecord0^RecordDAYENDXBADC(TJD,%UID,ACN,SEQ,ET,0)
 .	S vobj(dayendxbadc,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDAYENDXBADC(dayendxbadc,$$initPar^UCUTILN(params)) K vobj(dayendxbadc,-100) S vobj(dayendxbadc,-2)=1 TC:vTp  
 .	K vobj(+$G(dayendxbadc)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2,V3,V4,V5 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5),V4=vobj(this,-6),V5=vobj(this,-7) I '(''($D(^DAYEND(V1,"XBADC",V2,V3,V4,V5))#2)=''processMode) D
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
 .	if $D(vobj(this)) S ^DAYEND(vobj(this,-3),"XBADC",vobj(this,-4),vobj(this,-5),vobj(this,-6),vobj(this,-7))=vobj(this)
 .	;*** End of code by-passed by compiler ***
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 ZWI ^DAYEND(vobj(this,-3),"XBADC",vobj(this,-4),vobj(this,-5),vobj(this,-6),vobj(this,-7))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCchkReqForInsert(this) ; 
 I (vobj(this,-4)="") D vRCrequiredErr("%UID")
 I (vobj(this,-5)="") D vRCrequiredErr("ACN")
 I (vobj(this,-7)="") D vRCrequiredErr("ET")
 I (vobj(this,-6)="") D vRCrequiredErr("SEQ")
 I (vobj(this,-3)="") D vRCrequiredErr("TJD")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
 I (vobj(this,-3)="") D vRCrequiredErr("TJD")
 I (vobj(this,-4)="") D vRCrequiredErr("%UID")
 I (vobj(this,-5)="") D vRCrequiredErr("ACN")
 I (vobj(this,-6)="") D vRCrequiredErr("SEQ")
 I (vobj(this,-7)="") D vRCrequiredErr("ET")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DAYENDXBADC","MSG",1767,"DAYENDXBADC."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 S X=vobj(this,-3) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TJD",$$^MSG(742,"D"))
 I ($L(vobj(this,-4))>20) D vRCvalidateDDerr("%UID",$$^MSG(1076,20))
 I '(vobj(this,-5)=""),'(+vobj(this,-5)=vobj(this,-5))  S vobj(this,-5)=$$vRCtrimNumber(vobj(this,-5))
 S X=vobj(this,-5) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("ACN",$$^MSG(742,"N"))
 I '(vobj(this,-6)=""),'(+vobj(this,-6)=vobj(this,-6))  S vobj(this,-6)=$$vRCtrimNumber(vobj(this,-6))
 S X=vobj(this,-6) I '(X=""),(X'?1.3N),(X'?1"-"1.2N) D vRCvalidateDDerr("SEQ",$$^MSG(742,"N"))
 I ($L(vobj(this,-7))>12) D vRCvalidateDDerr("ET",$$^MSG(1076,12))
 I ($L($P(vobj(this),$C(124),2))>75) D vRCvalidateDDerr("IDENT",$$^MSG(1076,75))
 I ($L($P(vobj(this),$C(124),1))>20) D vRCvalidateDDerr("UID",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDD1(this) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,-100,"1*","TJD"))&($P($E($G(vobj(this,-100,"1*","TJD")),5,9999),$C(124))'=vobj(this,-3))) S X=vobj(this,-3) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("TJD",$$^MSG(742,"D"))
 I ($D(vobj(this,-100,"2*","%UID"))&($P($E($G(vobj(this,-100,"2*","%UID")),5,9999),$C(124))'=vobj(this,-4))) I ($L(vobj(this,-4))>20) D vRCvalidateDDerr("%UID",$$^MSG(1076,20))
 I ($D(vobj(this,-100,"3*","ACN"))&($P($E($G(vobj(this,-100,"3*","ACN")),5,9999),$C(124))'=vobj(this,-5))),'(vobj(this,-5)=""),'(+vobj(this,-5)=vobj(this,-5))  S vobj(this,-5)=$$vRCtrimNumber(vobj(this,-5))
 I ($D(vobj(this,-100,"3*","ACN"))&($P($E($G(vobj(this,-100,"3*","ACN")),5,9999),$C(124))'=vobj(this,-5))) S X=vobj(this,-5) I '(X=""),(X'?1.12N),(X'?1"-"1.11N) D vRCvalidateDDerr("ACN",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"4*","SEQ"))&($P($E($G(vobj(this,-100,"4*","SEQ")),5,9999),$C(124))'=vobj(this,-6))),'(vobj(this,-6)=""),'(+vobj(this,-6)=vobj(this,-6))  S vobj(this,-6)=$$vRCtrimNumber(vobj(this,-6))
 I ($D(vobj(this,-100,"4*","SEQ"))&($P($E($G(vobj(this,-100,"4*","SEQ")),5,9999),$C(124))'=vobj(this,-6))) S X=vobj(this,-6) I '(X=""),(X'?1.3N),(X'?1"-"1.2N) D vRCvalidateDDerr("SEQ",$$^MSG(742,"N"))
 I ($D(vobj(this,-100,"5*","ET"))&($P($E($G(vobj(this,-100,"5*","ET")),5,9999),$C(124))'=vobj(this,-7))) I ($L(vobj(this,-7))>12) D vRCvalidateDDerr("ET",$$^MSG(1076,12))
 I ($D(vobj(this,-100,"0*","IDENT"))&($P($E($G(vobj(this,-100,"0*","IDENT")),5,9999),$C(124))'=$P(vobj(this),$C(124),2))) I ($L($P(vobj(this),$C(124),2))>75) D vRCvalidateDDerr("IDENT",$$^MSG(1076,75))
 I ($D(vobj(this,-100,"0*","UID"))&($P($E($G(vobj(this,-100,"0*","UID")),5,9999),$C(124))'=$P(vobj(this),$C(124),1))) I ($L($P(vobj(this),$C(124),1))>20) D vRCvalidateDDerr("UID",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DAYENDXBADC","MSG",979,"DAYENDXBADC."_column_" "_errmsg)
 I ER D throwError($get(RM))
 Q 
 ;
vRCtrimNumber(str) ; 
 I ($E(str,1)="0") S str=$$vStrTrim(str,-1,"0") I (str="") S str="0"
 I (str["."),($E(str,$L(str))="0") S str=$$RTCHR^%ZFUNC(str,"0") I ($E(str,$L(str))=".") S str=$E(str,1,$L(str)-1) I (str="") S str="0"
 Q str
 ;
archive(archiveDir,archiveNum,archiveDate) ; 
 Q 0 ; Shell method
 ;
getArchiveFile(archiveTable,option,archiveKey) ; 
 Q "" ; Shell method
 ;
vRCkeyChanged(this,vRCparams,vRCaudit) ; 
 N vTp
 N newkeys N oldkeys N vRCauditIns
 N newKey1 S newKey1=vobj(this,-3)
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","TJD")):$P($E(vobj(this,-100,"1*","TJD"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","%UID")):$P($E(vobj(this,-100,"2*","%UID"),5,9999),$C(124)),1:vobj(this,-4))
 N newKey3 S newKey3=vobj(this,-5)
 N oldKey3 S oldKey3=$S($D(vobj(this,-100,"3*","ACN")):$P($E(vobj(this,-100,"3*","ACN"),5,9999),$C(124)),1:vobj(this,-5))
 N newKey4 S newKey4=vobj(this,-6)
 N oldKey4 S oldKey4=$S($D(vobj(this,-100,"4*","SEQ")):$P($E(vobj(this,-100,"4*","SEQ"),5,9999),$C(124)),1:vobj(this,-6))
 N newKey5 S newKey5=vobj(this,-7)
 N oldKey5 S oldKey5=$S($D(vobj(this,-100,"5*","ET")):$P($E(vobj(this,-100,"5*","ET"),5,9999),$C(124)),1:vobj(this,-7))
  N V1,V2,V3,V4,V5 S V1=vobj(this,-3),V2=vobj(this,-4),V3=vobj(this,-5),V4=vobj(this,-6),V5=vobj(this,-7) I ($D(^DAYEND(V1,"XBADC",V2,V3,V4,V5))#2) D throwError($$^MSG(2327))
 S newkeys=newKey1_","_newKey2_","_newKey3_","_newKey4_","_newKey5
 S oldkeys=oldKey1_","_oldKey2_","_oldKey3_","_oldKey4_","_oldKey5
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
  S vobj(this,-5)=oldKey3
  S vobj(this,-6)=oldKey4
  S vobj(this,-7)=oldKey5
 S vRCparams=$$setPar^UCUTILN(vRCparams,"NOINDEX")
 I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,1)
 D vRCmiscValidations(this,vRCparams,1)
 D vRCupdateDB(this,1,vRCparams,.vRCaudit,.vRCauditIns)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
  S vobj(this,-5)=newKey3
  S vobj(this,-6)=newKey4
  S vobj(this,-7)=newKey5
 N newrec S newrec=$$vReCp1(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDAYENDXBADC(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("DAYENDXBADC",oldkeys,newkeys)
 .	I ER D throwError($get(RM))
 .	Q 
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
  S vobj(this,-5)=oldKey3
  S vobj(this,-6)=oldKey4
  S vobj(this,-7)=oldKey5
 S vRCparams=$$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/")
 D vRCdelete(this,vRCparams,.vRCaudit,1)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
  S vobj(this,-5)=newKey3
  S vobj(this,-6)=newKey4
  S vobj(this,-7)=newKey5
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
vReCp1(v1) ; RecordDAYENDXBADC.copy: DAYENDXBADC
 ;
 Q $$copy^UCGMR(this)