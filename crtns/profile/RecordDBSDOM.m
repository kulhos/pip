 ; 
 ; **** Routine compiled from DATA-QWIK Filer RecordDBSDOM ****
 ; 
 ; 01/23/2016 18:58 - kulhan
 ; 
 ;
 ; Record Class code for table DBSDOM
 ;
 ; Generated by PSLRecordBuilder on 01/23/2016 at 18:58 by 1
 ;
vcdmNew() ; 
 N vOid
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBSDOM",vobj(vOid,-2)=0
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBSDOM"
 I '$D(^DBCTL("SYS","DOM",v1,v2))
 S vobj(vOid,-2)=1
 I $T K vobj(vOid) S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBSDOM",$EC=",U1001,"
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
 S vOid=$O(vobj(""),-1)+1,vobj(vOid,-1)="RecordDBSDOM"
 I '$D(^DBCTL("SYS","DOM",v1,v2))
 S vobj(vOid,-2)='$T
 S vobj(vOid,-3)=v1
 S vobj(vOid,-4)=v2
 ;*** End of code by-passed by compiler ***
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=SCOPE
 Q vOid
 ;
vRCgetRecord0Opt(v1,v2,vfromDbSet,v2out) ; 
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 I '$D(^DBCTL("SYS","DOM",v1,v2))
 S v2out=1
 I $T S $ZE="0,"_$ZPOS_",%PSL-E-RECNOFL,,DBSDOM",$EC=",U1001,"
 ;*** End of code by-passed by compiler ***
 Q ""
 ;
vRCgetRecord1Opt(v1,v2,vfromDbSet,v2out) ; 
 ;  #ACCEPT DATE=02/26/2008; PGM=Dan Russell; CR=30801; Group=BYPASS
 ;*** Start of code by-passed by compiler
 I '$D(^DBCTL("SYS","DOM",v1,v2))
 S v2out='$T
 ;*** End of code by-passed by compiler ***
 Q ""
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
 .	D AUDIT^UCUTILN(this,.vRCauditIns,10,"|")
 .	D vRCsetDefaults(this)
 .	I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 .	I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,%O)
 .	D vRCmiscValidations(this,vRCparams,%O)
 .	D vRCupdateDB(this,%O,vRCparams,.vRCaudit,.vRCauditIns)
 .	Q 
 E  I (%O=1) D
 .	Q:'$D(vobj(this,-100)) 
 .	D AUDIT^UCUTILN(this,.vRCaudit,10,"|")
 .	I ($D(vobj(this,-100,"1*","SYSSN"))&($P($E($G(vobj(this,-100,"1*","SYSSN")),5,9999),$C(124))'=vobj(this,-3)))!($D(vobj(this,-100,"2*","DOM"))&($P($E($G(vobj(this,-100,"2*","DOM")),5,9999),$C(124))'=vobj(this,-4))) D vRCkeyChanged(this,vRCparams,.vRCaudit) Q 
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
 .	  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) Q:'($D(^DBCTL("SYS","DOM",V1,V2))>9) 
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
vselectOptmOK(userclass,dbsdom,vkey1,vkey2) ; PUBLIC access is allowed, no restrict clause
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
 N columns S columns=""
 S columns=columns_"DEC,DES,DFT,DOM,IPF,LEN,LTD,MAX,MIN,MSK,MSU,NLV,OPF,PRDEC,PRDES,PRDFT,PRIPF,PRLEN,PRMAX,PRMIN,"
 S columns=columns_"PRMSK,PRMSU,PRNLV,PROPF,PRPTN,PRRHD,PRSIZ,PRTBL,PRTYP,PRVLD,PTN,RHD,SIZ,SYSSN,TBL,TYP,USER,VLD,"
 Q $$vStrRep("DEC,DES,DFT,DOM,IPF,LEN,LTD,MAX,MIN,MSK,MSU,NLV,OPF,PRDEC,PRDES,PRDFT,PRIPF,PRLEN,PRMAX,PRMIN,PRMSK,PRMSU,PRNLV,PROPF,PRPTN,PRRHD,PRSIZ,PRTBL,PRTYP,PRVLD,PTN,RHD,SIZ,SYSSN,TBL,TYP,USER,VLD",",",$char(9),0,0,"")
 ;
columnListBM() ; 
 Q ""
 ;
columnListCMP() ; 
 Q $$vStrRep("",",",$char(9),0,0,"")
 ;
getColumnMap(map) ; 
 ;
 S map(-4)="DOM:U:"
 S map(-3)="SYSSN:U:"
 S map(0)="DEC:N:15;DES:T:1;DFT:T:14;IPF:T:12;LEN:N:3;LTD:D:19;MAX:T:9;MIN:T:8;MSK:T:17;MSU:T:16;NLV:T:7;OPF:T:11;PTN:T:10;RHD:T:6;SIZ:N:4;TBL:T:5;TYP:U:2;USER:T:20;VLD:T:13"
 S map(1)="PRDEC:L:15;PRDES:L:1;PRDFT:L:14;PRIPF:L:12;PRLEN:L:3;PRMAX:L:9;PRMIN:L:8;PRMSK:L:17;PRMSU:L:16;PRNLV:L:7;PROPF:L:11;PRPTN:L:10;PRRHD:L:6;PRSIZ:L:4;PRTBL:L:5;PRTYP:L:2;PRVLD:L:13"
 Q 
 ;
vlegacy(processMode,params) ; 
 N vTp
 I (processMode=2) D
 .	N dbsdom S dbsdom=$$vRCgetRecord0^RecordDBSDOM(SYSSN,DOM,0)
 .	S vobj(dbsdom,-2)=2
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBSDOM(dbsdom,$$initPar^UCUTILN(params)) K vobj(dbsdom,-100) S vobj(dbsdom,-2)=1 TC:vTp  
 .	K vobj(+$G(dbsdom)) Q 
 Q 
 ;
vhasLiterals() ; 
 Q 0
 ;
vRCmiscValidations(this,vRCparams,processMode) ; 
 I (("/"_vRCparams_"/")["/VALST/")  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I '(''($D(^DBCTL("SYS","DOM",V1,V2))>9)=''processMode) D
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
 .	N n
 .	S n=-1
 .	F  S n=$order(vobj(this,n)) Q:(n="")  D
 ..		Q:'($D(vobj(this,n))#2) 
 ..		;    #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; GROUP=BYPASS
 ..		;*** Start of code by-passed by compiler
 ..		S ^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),n)=vobj(this,n)
 ..		;*** End of code by-passed by compiler ***
 ..		Q 
 .	Q 
 Q 
 ;
vRCdelete(this,vRCparams,vRCaudit,isKeyChange) ; 
 I '$get(isKeyChange),$D(vobj(this,-100)) D throwError("Deleted object cannot be modified")
 I '(("/"_vRCparams_"/")["/NOLOG/") D ^DBSLOGIT(this,3)
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 kill ^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4))
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCsetDefaults(this) ; 
  S:'$D(vobj(this,0)) vobj(this,0)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),0)),1:"")
  S:'$D(vobj(this,1)) vobj(this,1)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),1)),1:"")
 I ($P(vobj(this,0),$C(124),19)="")  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),19)=$P($H,",",1)
 I ($P(vobj(this,1),$C(124),15)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),15)=0
 I ($P(vobj(this,1),$C(124),1)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),1)=0
 I ($P(vobj(this,1),$C(124),14)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),14)=0
 I ($P(vobj(this,1),$C(124),12)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),12)=0
 I ($P(vobj(this,1),$C(124),3)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),3)=1
 I ($P(vobj(this,1),$C(124),9)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),9)=0
 I ($P(vobj(this,1),$C(124),8)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),8)=0
 I ($P(vobj(this,1),$C(124),17)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),17)=0
 I ($P(vobj(this,1),$C(124),16)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),16)=0
 I ($P(vobj(this,1),$C(124),7)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),7)=0
 I ($P(vobj(this,1),$C(124),11)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),11)=0
 I ($P(vobj(this,1),$C(124),10)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),10)=0
 I ($P(vobj(this,1),$C(124),6)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),6)=0
 I ($P(vobj(this,1),$C(124),4)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),4)=0
 I ($P(vobj(this,1),$C(124),5)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),5)=0
 I ($P(vobj(this,1),$C(124),2)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),2)=1
 I ($P(vobj(this,1),$C(124),13)="")  S vobj(this,-100,1)="" S $P(vobj(this,1),$C(124),13)=0
 I ($P(vobj(this,0),$C(124),20)="")  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),20)=$$USERNAM^%ZFUNC
 Q 
 ;
vRCchkReqForInsert(this) ; 
  S:'$D(vobj(this,0)) vobj(this,0)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),0)),1:"")
  S:'$D(vobj(this,1)) vobj(this,1)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),1)),1:"")
 I ($P(vobj(this,0),$C(124),1)="") D vRCrequiredErr("DES")
 I (vobj(this,-4)="") D vRCrequiredErr("DOM")
 I ($P(vobj(this,1),$C(124),15)="") D vRCrequiredErr("PRDEC")
 I ($P(vobj(this,1),$C(124),1)="") D vRCrequiredErr("PRDES")
 I ($P(vobj(this,1),$C(124),14)="") D vRCrequiredErr("PRDFT")
 I ($P(vobj(this,1),$C(124),12)="") D vRCrequiredErr("PRIPF")
 I ($P(vobj(this,1),$C(124),3)="") D vRCrequiredErr("PRLEN")
 I ($P(vobj(this,1),$C(124),9)="") D vRCrequiredErr("PRMAX")
 I ($P(vobj(this,1),$C(124),8)="") D vRCrequiredErr("PRMIN")
 I ($P(vobj(this,1),$C(124),17)="") D vRCrequiredErr("PRMSK")
 I ($P(vobj(this,1),$C(124),16)="") D vRCrequiredErr("PRMSU")
 I ($P(vobj(this,1),$C(124),7)="") D vRCrequiredErr("PRNLV")
 I ($P(vobj(this,1),$C(124),11)="") D vRCrequiredErr("PROPF")
 I ($P(vobj(this,1),$C(124),10)="") D vRCrequiredErr("PRPTN")
 I ($P(vobj(this,1),$C(124),6)="") D vRCrequiredErr("PRRHD")
 I ($P(vobj(this,1),$C(124),4)="") D vRCrequiredErr("PRSIZ")
 I ($P(vobj(this,1),$C(124),5)="") D vRCrequiredErr("PRTBL")
 I ($P(vobj(this,1),$C(124),2)="") D vRCrequiredErr("PRTYP")
 I ($P(vobj(this,1),$C(124),13)="") D vRCrequiredErr("PRVLD")
 I (vobj(this,-3)="") D vRCrequiredErr("SYSSN")
 I ($P(vobj(this,0),$C(124),2)="") D vRCrequiredErr("TYP")
 Q 
 ;
vRCchkReqForUpdate(this) ; 
  S:'$D(vobj(this,0)) vobj(this,0)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),0)),1:"")
  S:'$D(vobj(this,1)) vobj(this,1)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),1)),1:"")
 I (vobj(this,-3)="") D vRCrequiredErr("SYSSN")
 I (vobj(this,-4)="") D vRCrequiredErr("DOM")
 I ($D(vobj(this,-100,0))>9) D
 .	I ($D(vobj(this,-100,0,"DES"))&($P($E($G(vobj(this,-100,0,"DES")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),1))),($P(vobj(this,0),$C(124),1)="") D vRCrequiredErr("DES")
 .	I ($D(vobj(this,-100,0,"TYP"))&($P($E($G(vobj(this,-100,0,"TYP")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),2))),($P(vobj(this,0),$C(124),2)="") D vRCrequiredErr("TYP")
 .	Q 
 I ($D(vobj(this,-100,1))>9) D
 .	I ($D(vobj(this,-100,1,"PRDES"))&($P($E($G(vobj(this,-100,1,"PRDES")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),1))),($P(vobj(this,1),$C(124),1)="") D vRCrequiredErr("PRDES")
 .	I ($D(vobj(this,-100,1,"PRTYP"))&($P($E($G(vobj(this,-100,1,"PRTYP")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),2))),($P(vobj(this,1),$C(124),2)="") D vRCrequiredErr("PRTYP")
 .	I ($D(vobj(this,-100,1,"PRLEN"))&($P($E($G(vobj(this,-100,1,"PRLEN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),3))),($P(vobj(this,1),$C(124),3)="") D vRCrequiredErr("PRLEN")
 .	I ($D(vobj(this,-100,1,"PRSIZ"))&($P($E($G(vobj(this,-100,1,"PRSIZ")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),4))),($P(vobj(this,1),$C(124),4)="") D vRCrequiredErr("PRSIZ")
 .	I ($D(vobj(this,-100,1,"PRTBL"))&($P($E($G(vobj(this,-100,1,"PRTBL")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),5))),($P(vobj(this,1),$C(124),5)="") D vRCrequiredErr("PRTBL")
 .	I ($D(vobj(this,-100,1,"PRRHD"))&($P($E($G(vobj(this,-100,1,"PRRHD")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),6))),($P(vobj(this,1),$C(124),6)="") D vRCrequiredErr("PRRHD")
 .	I ($D(vobj(this,-100,1,"PRNLV"))&($P($E($G(vobj(this,-100,1,"PRNLV")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),7))),($P(vobj(this,1),$C(124),7)="") D vRCrequiredErr("PRNLV")
 .	I ($D(vobj(this,-100,1,"PRMIN"))&($P($E($G(vobj(this,-100,1,"PRMIN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),8))),($P(vobj(this,1),$C(124),8)="") D vRCrequiredErr("PRMIN")
 .	I ($D(vobj(this,-100,1,"PRMAX"))&($P($E($G(vobj(this,-100,1,"PRMAX")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),9))),($P(vobj(this,1),$C(124),9)="") D vRCrequiredErr("PRMAX")
 .	I ($D(vobj(this,-100,1,"PRPTN"))&($P($E($G(vobj(this,-100,1,"PRPTN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),10))),($P(vobj(this,1),$C(124),10)="") D vRCrequiredErr("PRPTN")
 .	I ($D(vobj(this,-100,1,"PROPF"))&($P($E($G(vobj(this,-100,1,"PROPF")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),11))),($P(vobj(this,1),$C(124),11)="") D vRCrequiredErr("PROPF")
 .	I ($D(vobj(this,-100,1,"PRIPF"))&($P($E($G(vobj(this,-100,1,"PRIPF")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),12))),($P(vobj(this,1),$C(124),12)="") D vRCrequiredErr("PRIPF")
 .	I ($D(vobj(this,-100,1,"PRVLD"))&($P($E($G(vobj(this,-100,1,"PRVLD")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),13))),($P(vobj(this,1),$C(124),13)="") D vRCrequiredErr("PRVLD")
 .	I ($D(vobj(this,-100,1,"PRDFT"))&($P($E($G(vobj(this,-100,1,"PRDFT")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),14))),($P(vobj(this,1),$C(124),14)="") D vRCrequiredErr("PRDFT")
 .	I ($D(vobj(this,-100,1,"PRDEC"))&($P($E($G(vobj(this,-100,1,"PRDEC")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),15))),($P(vobj(this,1),$C(124),15)="") D vRCrequiredErr("PRDEC")
 .	I ($D(vobj(this,-100,1,"PRMSU"))&($P($E($G(vobj(this,-100,1,"PRMSU")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),16))),($P(vobj(this,1),$C(124),16)="") D vRCrequiredErr("PRMSU")
 .	I ($D(vobj(this,-100,1,"PRMSK"))&($P($E($G(vobj(this,-100,1,"PRMSK")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),17))),($P(vobj(this,1),$C(124),17)="") D vRCrequiredErr("PRMSK")
 .	Q 
 ; Node "1*" - only one required column
 I ($D(vobj(this,-100,"1*","SYSSN"))&($P($E($G(vobj(this,-100,"1*","SYSSN")),5,9999),$C(124))'=vobj(this,-3))),(vobj(this,-3)="") D vRCrequiredErr("SYSSN")
 ; Node "2*" - only one required column
 I ($D(vobj(this,-100,"2*","DOM"))&($P($E($G(vobj(this,-100,"2*","DOM")),5,9999),$C(124))'=vobj(this,-4))),(vobj(this,-4)="") D vRCrequiredErr("DOM")
 Q 
 ;
vRCrequiredErr(column) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBSDOM","MSG",1767,"DBSDOM."_column)
 I ER D throwError($get(RM))
 Q 
 ;
vRCforceLoad(this) ; 
 N n S n=""
 ;  #ACCEPT DATE=04/22/04; PGM=Dan Russell; CR=20602; Group=BYPASS
 ;*** Start of code by-passed by compiler
 for  set n=$order(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),n)) quit:n=""  if '$D(vobj(this,n)),$D(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),n))#2 set vobj(this,n)=^(n)
 ;*** End of code by-passed by compiler ***
 Q 
 ;
vRCvalidateDD(this,processMode) ; 
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I (processMode=2) D vRCforceLoad(this)
 I ($D(vobj(this,0))#2) D
 .	 S:'$D(vobj(this,0)) vobj(this,0)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),0)),1:"")
 .	I '($P(vobj(this,0),$C(124),15)=""),'(+$P(vobj(this,0),$C(124),15)=$P(vobj(this,0),$C(124),15))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),15)=$$vRCtrimNumber($P(vobj(this,0),$C(124),15))
 .	S X=$P(vobj(this,0),$C(124),15) I '(X=""),(X'?1.2N),(X'?1"-"1.1N) D vRCvalidateDDerr("DEC",$$^MSG(742,"N"))
 .	I ($L($P(vobj(this,0),$C(124),1))>40) D vRCvalidateDDerr("DES",$$^MSG(1076,40))
 .	I ($L($P(vobj(this,0),$C(124),14))>58) D vRCvalidateDDerr("DFT",$$^MSG(1076,58))
 .	I ($L($P(vobj(this,0),$C(124),12))>40) D vRCvalidateDDerr("IPF",$$^MSG(1076,40))
 .	I '($P(vobj(this,0),$C(124),3)=""),'(+$P(vobj(this,0),$C(124),3)=$P(vobj(this,0),$C(124),3))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),3)=$$vRCtrimNumber($P(vobj(this,0),$C(124),3))
 .	S X=$P(vobj(this,0),$C(124),3) I '(X=""),(X'?1.5N),(X'?1"-"1.4N) D vRCvalidateDDerr("LEN",$$^MSG(742,"N"))
 .	S X=$P(vobj(this,0),$C(124),19) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("LTD",$$^MSG(742,"D"))
 .	I ($L($P(vobj(this,0),$C(124),9))>25) D vRCvalidateDDerr("MAX",$$^MSG(1076,25))
 .	I ($L($P(vobj(this,0),$C(124),8))>25) D vRCvalidateDDerr("MIN",$$^MSG(1076,25))
 .	I ($L($P(vobj(this,0),$C(124),17))>20) D vRCvalidateDDerr("MSK",$$^MSG(1076,20))
 .	S X=$P(vobj(this,0),$C(124),16) I '(X=""),'((","_"C,D,V,W"_",")[(","_X_",")) D vRCvalidateDDerr("MSU",$$^MSG(1485,X))
 .	I ($L($P(vobj(this,0),$C(124),7))>20) D vRCvalidateDDerr("NLV",$$^MSG(1076,20))
 .	I ($L($P(vobj(this,0),$C(124),11))>40) D vRCvalidateDDerr("OPF",$$^MSG(1076,40))
 .	I ($L($P(vobj(this,0),$C(124),10))>60) D vRCvalidateDDerr("PTN",$$^MSG(1076,60))
 .	I ($L($P(vobj(this,0),$C(124),6))>40) D vRCvalidateDDerr("RHD",$$^MSG(1076,40))
 .	I '($P(vobj(this,0),$C(124),4)=""),'(+$P(vobj(this,0),$C(124),4)=$P(vobj(this,0),$C(124),4))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),4)=$$vRCtrimNumber($P(vobj(this,0),$C(124),4))
 .	S X=$P(vobj(this,0),$C(124),4) I '(X=""),(X'?1.3N),(X'?1"-"1.2N) D vRCvalidateDDerr("SIZ",$$^MSG(742,"N"))
 .	I ($L($P(vobj(this,0),$C(124),5))>255) D vRCvalidateDDerr("TBL",$$^MSG(1076,255))
 .	S X=$P(vobj(this,0),$C(124),2) I '(X=""),'($D(^DBCTL("SYS","DVFM",X))#2) D vRCvalidateDDerr("TYP",$$^MSG(1485,X))
 .	I ($L($P(vobj(this,0),$C(124),20))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 .	I ($L($P(vobj(this,0),$C(124),13))>70) D vRCvalidateDDerr("VLD",$$^MSG(1076,70))
 .	Q 
 I ($D(vobj(this,1))#2) D
 .	 S:'$D(vobj(this,1)) vobj(this,1)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),1)),1:"")
 .	I '(($P(vobj(this,1),$C(124),15)=1)!($P(vobj(this,1),$C(124),15)=0)) D vRCvalidateDDerr("PRDEC",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),1)=1)!($P(vobj(this,1),$C(124),1)=0)) D vRCvalidateDDerr("PRDES",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),14)=1)!($P(vobj(this,1),$C(124),14)=0)) D vRCvalidateDDerr("PRDFT",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),12)=1)!($P(vobj(this,1),$C(124),12)=0)) D vRCvalidateDDerr("PRIPF",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),3)=1)!($P(vobj(this,1),$C(124),3)=0)) D vRCvalidateDDerr("PRLEN",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),9)=1)!($P(vobj(this,1),$C(124),9)=0)) D vRCvalidateDDerr("PRMAX",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),8)=1)!($P(vobj(this,1),$C(124),8)=0)) D vRCvalidateDDerr("PRMIN",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),17)=1)!($P(vobj(this,1),$C(124),17)=0)) D vRCvalidateDDerr("PRMSK",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),16)=1)!($P(vobj(this,1),$C(124),16)=0)) D vRCvalidateDDerr("PRMSU",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),7)=1)!($P(vobj(this,1),$C(124),7)=0)) D vRCvalidateDDerr("PRNLV",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),11)=1)!($P(vobj(this,1),$C(124),11)=0)) D vRCvalidateDDerr("PROPF",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),10)=1)!($P(vobj(this,1),$C(124),10)=0)) D vRCvalidateDDerr("PRPTN",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),6)=1)!($P(vobj(this,1),$C(124),6)=0)) D vRCvalidateDDerr("PRRHD",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),4)=1)!($P(vobj(this,1),$C(124),4)=0)) D vRCvalidateDDerr("PRSIZ",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),5)=1)!($P(vobj(this,1),$C(124),5)=0)) D vRCvalidateDDerr("PRTBL",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),2)=1)!($P(vobj(this,1),$C(124),2)=0)) D vRCvalidateDDerr("PRTYP",$$^MSG(742,"L"))
 .	I '(($P(vobj(this,1),$C(124),13)=1)!($P(vobj(this,1),$C(124),13)=0)) D vRCvalidateDDerr("PRVLD",$$^MSG(742,"L"))
 .	Q 
 S X=vobj(this,-3) I '(X=""),'($D(^SCATBL(2,X))#2) D vRCvalidateDDerr("SYSSN",$$^MSG(1485,X))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("DOM",$$^MSG(1476))
 I ($L(vobj(this,-4))>20) D vRCvalidateDDerr("DOM",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDD1(this) ; 
  S:'$D(vobj(this,0)) vobj(this,0)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),0)),1:"")
  S:'$D(vobj(this,1)) vobj(this,1)=$S(vobj(this,-2):$G(^DBCTL("SYS","DOM",vobj(this,-3),vobj(this,-4),1)),1:"")
 N ER S ER=0
 N RM S RM=""
 N errmsg N X
 I ($D(vobj(this,0))#2) D
 .	I ($D(vobj(this,-100,0,"DEC"))&($P($E($G(vobj(this,-100,0,"DEC")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),15))),'($P(vobj(this,0),$C(124),15)=""),'(+$P(vobj(this,0),$C(124),15)=$P(vobj(this,0),$C(124),15))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),15)=$$vRCtrimNumber($P(vobj(this,0),$C(124),15))
 .	I ($D(vobj(this,-100,0,"DEC"))&($P($E($G(vobj(this,-100,0,"DEC")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),15))) S X=$P(vobj(this,0),$C(124),15) I '(X=""),(X'?1.2N),(X'?1"-"1.1N) D vRCvalidateDDerr("DEC",$$^MSG(742,"N"))
 .	I ($D(vobj(this,-100,0,"DES"))&($P($E($G(vobj(this,-100,0,"DES")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),1))) I ($L($P(vobj(this,0),$C(124),1))>40) D vRCvalidateDDerr("DES",$$^MSG(1076,40))
 .	I ($D(vobj(this,-100,0,"DFT"))&($P($E($G(vobj(this,-100,0,"DFT")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),14))) I ($L($P(vobj(this,0),$C(124),14))>58) D vRCvalidateDDerr("DFT",$$^MSG(1076,58))
 .	I ($D(vobj(this,-100,0,"IPF"))&($P($E($G(vobj(this,-100,0,"IPF")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),12))) I ($L($P(vobj(this,0),$C(124),12))>40) D vRCvalidateDDerr("IPF",$$^MSG(1076,40))
 .	I ($D(vobj(this,-100,0,"LEN"))&($P($E($G(vobj(this,-100,0,"LEN")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),3))),'($P(vobj(this,0),$C(124),3)=""),'(+$P(vobj(this,0),$C(124),3)=$P(vobj(this,0),$C(124),3))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),3)=$$vRCtrimNumber($P(vobj(this,0),$C(124),3))
 .	I ($D(vobj(this,-100,0,"LEN"))&($P($E($G(vobj(this,-100,0,"LEN")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),3))) S X=$P(vobj(this,0),$C(124),3) I '(X=""),(X'?1.5N),(X'?1"-"1.4N) D vRCvalidateDDerr("LEN",$$^MSG(742,"N"))
 .	I ($D(vobj(this,-100,0,"LTD"))&($P($E($G(vobj(this,-100,0,"LTD")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),19))) S X=$P(vobj(this,0),$C(124),19) I '(X=""),(X'?1.5N) D vRCvalidateDDerr("LTD",$$^MSG(742,"D"))
 .	I ($D(vobj(this,-100,0,"MAX"))&($P($E($G(vobj(this,-100,0,"MAX")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),9))) I ($L($P(vobj(this,0),$C(124),9))>25) D vRCvalidateDDerr("MAX",$$^MSG(1076,25))
 .	I ($D(vobj(this,-100,0,"MIN"))&($P($E($G(vobj(this,-100,0,"MIN")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),8))) I ($L($P(vobj(this,0),$C(124),8))>25) D vRCvalidateDDerr("MIN",$$^MSG(1076,25))
 .	I ($D(vobj(this,-100,0,"MSK"))&($P($E($G(vobj(this,-100,0,"MSK")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),17))) I ($L($P(vobj(this,0),$C(124),17))>20) D vRCvalidateDDerr("MSK",$$^MSG(1076,20))
 .	I ($D(vobj(this,-100,0,"MSU"))&($P($E($G(vobj(this,-100,0,"MSU")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),16))) S X=$P(vobj(this,0),$C(124),16) I '(X=""),'((","_"C,D,V,W"_",")[(","_X_",")) D vRCvalidateDDerr("MSU",$$^MSG(1485,X))
 .	I ($D(vobj(this,-100,0,"NLV"))&($P($E($G(vobj(this,-100,0,"NLV")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),7))) I ($L($P(vobj(this,0),$C(124),7))>20) D vRCvalidateDDerr("NLV",$$^MSG(1076,20))
 .	I ($D(vobj(this,-100,0,"OPF"))&($P($E($G(vobj(this,-100,0,"OPF")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),11))) I ($L($P(vobj(this,0),$C(124),11))>40) D vRCvalidateDDerr("OPF",$$^MSG(1076,40))
 .	I ($D(vobj(this,-100,0,"PTN"))&($P($E($G(vobj(this,-100,0,"PTN")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),10))) I ($L($P(vobj(this,0),$C(124),10))>60) D vRCvalidateDDerr("PTN",$$^MSG(1076,60))
 .	I ($D(vobj(this,-100,0,"RHD"))&($P($E($G(vobj(this,-100,0,"RHD")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),6))) I ($L($P(vobj(this,0),$C(124),6))>40) D vRCvalidateDDerr("RHD",$$^MSG(1076,40))
 .	I ($D(vobj(this,-100,0,"SIZ"))&($P($E($G(vobj(this,-100,0,"SIZ")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),4))),'($P(vobj(this,0),$C(124),4)=""),'(+$P(vobj(this,0),$C(124),4)=$P(vobj(this,0),$C(124),4))  S vobj(this,-100,0)="" S $P(vobj(this,0),$C(124),4)=$$vRCtrimNumber($P(vobj(this,0),$C(124),4))
 .	I ($D(vobj(this,-100,0,"SIZ"))&($P($E($G(vobj(this,-100,0,"SIZ")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),4))) S X=$P(vobj(this,0),$C(124),4) I '(X=""),(X'?1.3N),(X'?1"-"1.2N) D vRCvalidateDDerr("SIZ",$$^MSG(742,"N"))
 .	I ($D(vobj(this,-100,0,"TBL"))&($P($E($G(vobj(this,-100,0,"TBL")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),5))) I ($L($P(vobj(this,0),$C(124),5))>255) D vRCvalidateDDerr("TBL",$$^MSG(1076,255))
 .	I ($D(vobj(this,-100,0,"TYP"))&($P($E($G(vobj(this,-100,0,"TYP")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),2))) S X=$P(vobj(this,0),$C(124),2) I '(X=""),'($D(^DBCTL("SYS","DVFM",X))#2) D vRCvalidateDDerr("TYP",$$^MSG(1485,X))
 .	I ($D(vobj(this,-100,0,"USER"))&($P($E($G(vobj(this,-100,0,"USER")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),20))) I ($L($P(vobj(this,0),$C(124),20))>20) D vRCvalidateDDerr("USER",$$^MSG(1076,20))
 .	I ($D(vobj(this,-100,0,"VLD"))&($P($E($G(vobj(this,-100,0,"VLD")),5,9999),$C(124))'=$P(vobj(this,0),$C(124),13))) I ($L($P(vobj(this,0),$C(124),13))>70) D vRCvalidateDDerr("VLD",$$^MSG(1076,70))
 .	Q 
 I ($D(vobj(this,1))#2) D
 .	I ($D(vobj(this,-100,1,"PRDEC"))&($P($E($G(vobj(this,-100,1,"PRDEC")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),15))) I '(($P(vobj(this,1),$C(124),15)=1)!($P(vobj(this,1),$C(124),15)=0)) D vRCvalidateDDerr("PRDEC",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRDES"))&($P($E($G(vobj(this,-100,1,"PRDES")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),1))) I '(($P(vobj(this,1),$C(124),1)=1)!($P(vobj(this,1),$C(124),1)=0)) D vRCvalidateDDerr("PRDES",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRDFT"))&($P($E($G(vobj(this,-100,1,"PRDFT")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),14))) I '(($P(vobj(this,1),$C(124),14)=1)!($P(vobj(this,1),$C(124),14)=0)) D vRCvalidateDDerr("PRDFT",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRIPF"))&($P($E($G(vobj(this,-100,1,"PRIPF")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),12))) I '(($P(vobj(this,1),$C(124),12)=1)!($P(vobj(this,1),$C(124),12)=0)) D vRCvalidateDDerr("PRIPF",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRLEN"))&($P($E($G(vobj(this,-100,1,"PRLEN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),3))) I '(($P(vobj(this,1),$C(124),3)=1)!($P(vobj(this,1),$C(124),3)=0)) D vRCvalidateDDerr("PRLEN",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRMAX"))&($P($E($G(vobj(this,-100,1,"PRMAX")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),9))) I '(($P(vobj(this,1),$C(124),9)=1)!($P(vobj(this,1),$C(124),9)=0)) D vRCvalidateDDerr("PRMAX",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRMIN"))&($P($E($G(vobj(this,-100,1,"PRMIN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),8))) I '(($P(vobj(this,1),$C(124),8)=1)!($P(vobj(this,1),$C(124),8)=0)) D vRCvalidateDDerr("PRMIN",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRMSK"))&($P($E($G(vobj(this,-100,1,"PRMSK")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),17))) I '(($P(vobj(this,1),$C(124),17)=1)!($P(vobj(this,1),$C(124),17)=0)) D vRCvalidateDDerr("PRMSK",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRMSU"))&($P($E($G(vobj(this,-100,1,"PRMSU")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),16))) I '(($P(vobj(this,1),$C(124),16)=1)!($P(vobj(this,1),$C(124),16)=0)) D vRCvalidateDDerr("PRMSU",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRNLV"))&($P($E($G(vobj(this,-100,1,"PRNLV")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),7))) I '(($P(vobj(this,1),$C(124),7)=1)!($P(vobj(this,1),$C(124),7)=0)) D vRCvalidateDDerr("PRNLV",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PROPF"))&($P($E($G(vobj(this,-100,1,"PROPF")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),11))) I '(($P(vobj(this,1),$C(124),11)=1)!($P(vobj(this,1),$C(124),11)=0)) D vRCvalidateDDerr("PROPF",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRPTN"))&($P($E($G(vobj(this,-100,1,"PRPTN")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),10))) I '(($P(vobj(this,1),$C(124),10)=1)!($P(vobj(this,1),$C(124),10)=0)) D vRCvalidateDDerr("PRPTN",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRRHD"))&($P($E($G(vobj(this,-100,1,"PRRHD")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),6))) I '(($P(vobj(this,1),$C(124),6)=1)!($P(vobj(this,1),$C(124),6)=0)) D vRCvalidateDDerr("PRRHD",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRSIZ"))&($P($E($G(vobj(this,-100,1,"PRSIZ")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),4))) I '(($P(vobj(this,1),$C(124),4)=1)!($P(vobj(this,1),$C(124),4)=0)) D vRCvalidateDDerr("PRSIZ",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRTBL"))&($P($E($G(vobj(this,-100,1,"PRTBL")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),5))) I '(($P(vobj(this,1),$C(124),5)=1)!($P(vobj(this,1),$C(124),5)=0)) D vRCvalidateDDerr("PRTBL",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRTYP"))&($P($E($G(vobj(this,-100,1,"PRTYP")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),2))) I '(($P(vobj(this,1),$C(124),2)=1)!($P(vobj(this,1),$C(124),2)=0)) D vRCvalidateDDerr("PRTYP",$$^MSG(742,"L"))
 .	I ($D(vobj(this,-100,1,"PRVLD"))&($P($E($G(vobj(this,-100,1,"PRVLD")),5,9999),$C(124))'=$P(vobj(this,1),$C(124),13))) I '(($P(vobj(this,1),$C(124),13)=1)!($P(vobj(this,1),$C(124),13)=0)) D vRCvalidateDDerr("PRVLD",$$^MSG(742,"L"))
 .	Q 
 I ($D(vobj(this,-100,"1*","SYSSN"))&($P($E($G(vobj(this,-100,"1*","SYSSN")),5,9999),$C(124))'=vobj(this,-3))) S X=vobj(this,-3) I '(X=""),'($D(^SCATBL(2,X))#2) D vRCvalidateDDerr("SYSSN",$$^MSG(1485,X))
 I (vobj(this,-4)'=$ZCONVERT(vobj(this,-4),"U")) D vRCvalidateDDerr("DOM",$$^MSG(1476))
 I ($D(vobj(this,-100,"2*","DOM"))&($P($E($G(vobj(this,-100,"2*","DOM")),5,9999),$C(124))'=vobj(this,-4))) I ($L(vobj(this,-4))>20) D vRCvalidateDDerr("DOM",$$^MSG(1076,20))
 Q 
 ;
vRCvalidateDDerr(column,errmsg) ; 
 N ER S ER=0
 N RM S RM=""
 D SETERR^DBSEXECU("DBSDOM","MSG",979,"DBSDOM."_column_" "_errmsg)
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
 N oldKey1 S oldKey1=$S($D(vobj(this,-100,"1*","SYSSN")):$P($E(vobj(this,-100,"1*","SYSSN"),5,9999),$C(124)),1:vobj(this,-3))
 N newKey2 S newKey2=vobj(this,-4)
 N oldKey2 S oldKey2=$S($D(vobj(this,-100,"2*","DOM")):$P($E(vobj(this,-100,"2*","DOM"),5,9999),$C(124)),1:vobj(this,-4))
  N V1,V2 S V1=vobj(this,-3),V2=vobj(this,-4) I ($D(^DBCTL("SYS","DOM",V1,V2))>9) D throwError($$^MSG(2327))
 S newkeys=newKey1_","_newKey2
 S oldkeys=oldKey1_","_oldKey2
  S vobj(this,-3)=oldKey1
  S vobj(this,-4)=oldKey2
 S vRCparams=$$setPar^UCUTILN(vRCparams,"NOINDEX")
 D vRCforceLoad(this)
 I (("/"_vRCparams_"/")["/VALREQ/") D vRCchkReqForInsert(this)
 I (("/"_vRCparams_"/")["/VALDD/") D vRCvalidateDD(this,1)
 D vRCmiscValidations(this,vRCparams,1)
 D vRCupdateDB(this,1,vRCparams,.vRCaudit,.vRCauditIns)
  S vobj(this,-3)=newKey1
  S vobj(this,-4)=newKey2
 N newrec S newrec=$$vReCp1(this)
 S vobj(newrec,-2)=0
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBSDOM(newrec,$$initPar^UCUTILN($$initPar^UCUTILN("/NOVAL/NOCASDEL/NOJOURNAL/NOTRIGBEF/NOTRIGAFT/"))) K vobj(newrec,-100) S vobj(newrec,-2)=1 TC:vTp  
 D
 .	N %O S %O=1
 .	N ER S ER=0
 .	N RM S RM=""
 .	;   #ACCEPT Date=10/24/2008; Pgm=RussellDS; CR=30801; Group=ACCESS
 .	D CASUPD^DBSEXECU("DBSDOM",oldkeys,newkeys)
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
vReCp1(v1) ; RecordDBSDOM.copy: DBSDOM
 ;
 N vNod,vOid
 I $G(vobj(v1,-2)) D
 .	F vNod=0,1 S:'$D(vobj(v1,vNod)) vobj(v1,vNod)=$G(^DBCTL("SYS","DOM",vobj(v1,-3),vobj(v1,-4),vNod))
 S vOid=$$copy^UCGMR(v1)
 Q vOid
