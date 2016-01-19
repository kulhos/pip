 ; 
 ; **** Routine compiled from DATA-QWIK Procedure PBSUTL ****
 ; 
 ; 02/24/2010 18:22 - pip
 ; 
PBSUTL ; Public;PROFILE Client/Server Utilities
 ;
 Q 
 ;
LINK(SVTYP,PGM) ; ZLink  program 'PGM'
 ;
 N SVID
 N CMD S CMD="ZL """_PGM_""""
 ;
 I '$$VALID^%ZRTNS(PGM) WRITE $$^MSG(5424) Q 
 ;
 N rs,vos1,vos2,vos3,vos4 S rs=$$vOpen1()
 I '$G(vos1) Q 
 ;
 F  Q:'$$vFetch1()  D
 . S SVID=rs
 .	D EXEC(SVTYP,SVID,CMD)
 .	Q 
 I $$rdb^UCDBRT() D LINKCP(CMD)
 ;
 Q 
 ;
EXEC(vzsvtyp,vzsvid,code) ; Issue an executable message
 N vTp
 ;
 N vzsvseq
 ;
 N svctrlt,vop1 S svctrlt=$$vRCgetRecord1Opt^RecordSVCTRLT(vzsvtyp,vzsvid,0,.vop1)
 I $G(vop1)=0 D  Q 
 .	S vzerror=1
 .	S vzrmsg=$$^MSG(1467) ;Invalid server ID
 .	Q 
 ;
 S vzsvseq=$O(^SVCTRL(vzsvtyp,vzsvid,""),-1)+1
 I vzsvseq="" S vzsvseq=1
 ;
 N svctrl,vop2,vop3,vop4,vop5 S svctrl="",vop5=0
  S vop4=vzsvtyp
  S vop3=vzsvid
  S vop2=vzsvseq
  S $P(svctrl,$C(124),1)="EXEC "_code
 ;
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^SVCTRL(vop4,vop3,vop2)=$$RTBAR^%ZFUNC(svctrl) S vop5=1 TC:vTp  
 ;
 ; Invoke CTRL Interrupt to server process
 D SIGNAL^IPCMGR($$HEXDEC^%ZHEX($P(svctrlt,$C(124),3)),"CTRL")
 ;
 Q 
 ;
LINKCP(CMD) ; ZLink  program 'PGM' to cursor process
 N vTp
 ;
 N CPID N MSEQ N PID
 ;
 S QNAME=$$TRNLNM^%ZFUNC("SCA_CS_ST_SCA_IBS")
 ;
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen2()
 ;
 F  Q:'$$vFetch2()  D
 .	;
 . S CPID=$P(rs,$C(9),2)
 . S PID=$$HEXDEC^%ZHEX($P(rs,$C(9),3))
 .	;
 .	N rs1,vos6,vos7,vos8,vos9,vos10,vos11 S rs1=$$vOpen3()
 . I '$G(vos6) S MSEQ=1
 .	;
 . F  Q:'$$vFetch3()  S MSEQ=rs1+1
 .	;
 .	N svctrltcp S svctrltcp=$$vcdmNew^RecordSVCTRLTCP() S vobj(svctrltcp,-3)=QNAME S vobj(svctrltcp,-4)=CPID S vobj(svctrltcp,-5)=MSEQ
 .  S $P(vobj(svctrltcp),$C(124),2)=PID
 .  S $P(vobj(svctrltcp),$C(124),1)=CMD
 .	;
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSVCTRLTCP(svctrltcp,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(svctrltcp,-100) S vobj(svctrltcp,-2)=1 TC:vTp  
 .	;
 .	; Invoke CTRL Interrupt to server process
 .	D SIGNAL^IPCMGR(PID,"CTRL")
 . K vobj(+$G(svctrltcp)) Q 
 Q 
 ;
START ; Start PROFILE/IBS server(s)
 ;
 ; Invalid network function
 I $get(%LOGID) S ER=1 S RM=$$^MSG(1409) Q 
 ;
 D PRMT(1)
 Q:VFMQ="Q" 
 ;
 D JOB(SVTYP,SVCNT)
 S ER="W"
 Q 
 ;
JOB(SVTYP,SVCNT) ; External entry point to start servers
 ;
 N JOBNAM N PID N PRCNAM N RMSEQ N SVID N vzsvpgm
 ;
 S SVTYP=$get(SVTYP)
 I SVTYP="" S SVTYP="SCA$IBS"
 ;
 S SVCNT=$get(SVCNT)
 I 'SVCNT S SVCNT=1
 ;
 N ctbl S ctbl=$$vRCgetRecord0Opt^RecordCTBLSVTYP(SVTYP,0,"")
 S vzsvpgm=$P(ctbl,$C(124),4)
 I vzsvpgm="" S vzsvpgm="SVCNCT^PBSSRV"
 ;
 ; Delete entries in control table no longer active (i.e., via stop/id)
 ;
 N vzsexpr N X
 ;
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen4()
 I ''$G(vos1) F  Q:'$$vFetch4()  D
 . S SVID=$P(rs,$C(9),1)
 . S PID=$P(rs,$C(9),2)
 .	;
 .	I $$VALID^%ZPID(PID,1) Q 
 .  ZWI ^SVCTRL(SVTYP,SVID)
 .	D vDbDe1()
 .	D vDbDe2()
 .  ZWI ^SVSTAT(SVTYP,SVID)
 .	Q 
 ;
 S SVID=$O(^SVCTRL(SVTYP,""),-1)
 ;
 F  Q:'SVCNT  D
 .	S SVID=SVID+1
 .	S SVCNT=SVCNT-1
 .	S PRCNAM=SVTYP_"_"_""_"_"_SVID
 .	S JOBNAM=vzsvpgm_"("""_SVTYP_""","_SVID_")"
 .	;
 .	; Different MUMPS platforms support different parameters
 .	S params=$$JOBPARAM^%OSSCRPT(PRCNAM)
 .	S X=$$^%ZJOB(JOBNAM,params,1)
 .	S RMSEQ=$order(RM(""),-1)+1
 .	;
 .	; ~p1 submitted
 .	I X S RM(RMSEQ)=$$^MSG(6800,PRCNAM)
 .	;
 .	; ~p1 not submitted
 .	E  S RM(RMSEQ)=$$^MSG(6799,PRCNAM)
 .	Q 
 Q 
 ;
STOP ; Issue a stop message to PROFILE/IBS server(s)
 ;
 N SVCNT
 ;
 D PRMT(0)
 Q:VFMQ="Q" 
 ;
 S MTMID=$$SCA^%TRNLNM("CS_ST_"_SVTYP)
 ;
 N rs,vos1,vos2,vos3,vos4,vos5,vos6 S rs=$$vOpen5()
 I '$G(vos1) Q 
 F  Q:'$$vFetch5()  D  Q:SVCNT=0 
 .	;
 .	; Signal Interrupt to STOP
 . D SIGNAL^IPCMGR($$HEXDEC^%ZHEX(rs),"STOP")
 .	;
 .	S SVCNT=SVCNT-1
 .	Q 
 ;
 Q 
 ;
PRMT(OPT) ; Prompt for service type and number of servers
 ;
 N %PG
 N OLNTB
 ;
 S %PG=1
 K VFMQ
 D VPG00
 Q 
 ;
VPG00 ; Screen set up
 ;
 N %READ N %TAB
 ;
 ; /DES=Service Type/TYP=U/LEN=20
 S %TAB("SVTYP")=".SVTYP/TBL=[CTBLSVTYP]"
 ;
 ; /DES=Number of Servers/TYP=N/LEN=2
 S %TAB("SVCNT")=".SVCNT/MIN=1"
 ;
 ; Default service type
 I '($D(SVTYP)#2) S SVTYP="SCA$IBS"
 S %READ="@@%FN,,,SVTYP/REQ,SVCNT/REQ"
 D ^UTLREAD I VFMQ="Q" Q 
 I 'OPT Q 
 ;
 N DATA N MTNAME N PGM
 ;
 N svtyp S svtyp=$$vRCgetRecord0Opt^RecordCTBLSVTYP(SVTYP,0,"")
 S MTNAME=$P(svtyp,$C(124),11)
 S PGM=$P(svtyp,$C(124),4)
 ;
 I PGM=""!(PGM="SVCNCT^PBSSRV") D
 .	S ET=$$SVCNCT^%MTAPI(SVTYP,.ID,,MTNAME)
 .	I ET="" S ET=$$SVDSCNCT^%MTAPI(ID)
 .	I ET'="" D
 ..		S ER=1
 ..		S RM=$$ERRDES(ET)
 ..		S VFMQ="Q"
 ..		I RM="" S RM=ET
 ..		Q 
 .	Q 
 Q 
 ;
CSERR(et) ; Client/server message format or transport error
 ;
 N RM
 ;
 D ERRLOG(et,0)
 Q $$ERRMSG($get(RM),et)
 ;
ERRLOG(ET,rms) ; Log error
 ;
 I $get(rms) D
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch1^"_$T(+0)
 .	;
 .	N LOG
 .	;
 .	S LOG=$$FILE^%TRNLNM("PROFILE_SERVER_ERROR.LOG","SCAU$DIR")
 .	I '$$FILE^%ZOPEN(LOG,"WRITE/APPEND",2,1024) Q 
 .	;
 .	USE LOG
 .	WRITE !,"===================================================",!
 .	WRITE $P($H,",",1),!
 .	;
 .	;   #ACCEPT Date=06/22/2008; Pgm=RussellDS; CR=34286; Group=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZSHOW "*"
 .	;*** End of code by-passed by compiler ***
 .	WRITE !,"===================================================",!
 .	CLOSE LOG
 .	USE 0
 .	Q 
 ;
 E  D ^UTLERR
 Q 
 ;
ERRMSG(rm,et,erptr,fldps) ; Return standard error reply message
 ;
  S %MSGID=$get(%MSGID)
 N descr N ercode N param
 N fld
 ;
 I $get(erptr)="" S erptr=1
 S et=$get(et)
 S rm=$get(rm)
 ;
 I et="",rm'="" D
 .	;
 .	S ercode="MSG_"_$piece(%MSGID,"|",1)
 .	;
 .	; Pack parameters, if any
 .	I ($piece(%MSGID,"|",2,$L(%MSGID))="") S param=""
 .	E  D
 ..		;
 ..		N i
 ..		N params
 ..		;
 ..		F i=2:1:$L(%MSGID,"|") S params(i-1)=$piece(%MSGID,"|",i)
 ..		S param=$$V2LV^MSG(.params)
 ..		Q 
 .	;
 .	S descr=rm
 .	Q 
 ;
 E  D
 .	I et="" S et="SV_ERRUNDEF"
 .	S ercode="ER_"_et
 .	I rm'="" S descr=rm
 .	E  S descr=$$ERRDES(et)
 .	Q 
 ;
 S fld(1)="ER" ; Error category
 S fld(2)=erptr ; Sub-record number
 S fld(3)=ercode ; Error code
 S fld(4)=$get(param) ; Parameters
 S fld(5)=descr ; Full description
 S fld(6)=$get(fldps) ; Field position
 ;
 Q $$V2LV^MSG(.fld)
 ;
ERRDES(et) ; Return error description
 ;
 N desc
 ;
 N stbler S stbler=$G(^STBL("ER",et))
 S desc=$P(stbler,$C(124),1)
 I desc="" S desc=et
 Q desc
 ;
param(x) ; Build parameter list from array
 ;
 N i
 N y N z
 ;
 S y=""
 F i=1:1 Q:'$D(x(i))  D
 .	I x(i)?.e1c.e D
 ..		S z="vzpar"_i
 ..		S @z=x(i)
 ..		S y=y_z
 ..		Q 
 .	;
 .	E  S y=y_$$quote(x(i))
 .	S y=y_$char(44)
 .	Q 
 ;
 Q $E(y,1,$L(y)-1)
 ;
quote(x) ; Quote input parameters
 ;
 I ($E(x,1)=".") Q x ; Pass by reference
 I (x=+x) Q x ; Numeric field
 Q $S(x'["""":""""_x_"""",1:$$QADD^%ZS(x,"""")) ; Literal SSV
 ;
VSAV(vzsav) ; Extrinsic function to save specified variables
 ;
 I $get(vzsav)="" Q ""
 ;
 N vzi
 N vzd N vznam N vzstr
 ;
 S vzstr=""
 ;
 I vzsav="*" D
 .	S vznam="%"
 .	I ($D(%)#2) D SAVE("%")
 .	;   #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 .	F  S vznam=$order(@vznam) Q:vznam=""  D SAVE(vznam)
 .	Q 
 ;
 E  F vzi=1:1 D  Q:vznam="" 
 .	S vznam=$piece(vzsav,",",vzi)
 .	I vznam="" Q 
 .	D SAVE(vznam)
 .	Q 
 ;
 Q vzstr
 ;
SAVE(vznam) ; Save 'VZNAM' and descendants
 ;
 Q:$E(vznam,1,2)="vz" 
 ;
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 S vzd=$D(@vznam)
 ;
 I vzd#2 S vzstr=vzstr_$$LV(vznam)
 I vzd=1 Q 
 ;
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 F  S vznam=$query(@vznam) Q:vznam=""  S vzstr=vzstr_$$LV(vznam)
 Q 
 ;
LV(vznam) ; Build vzref=vzval expression
 ;
 N vz S vz=""
 ;
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown;Group=BYPASS
 ;*** Start of code by-passed by compiler
 set vz=$$V2LV^MSG(vznam)_$$V2LV^MSG(@vznam)
 ;*** End of code by-passed by compiler ***
 ;
 Q vz
 ;
VLOD(vzstr) ; Subroutine to load variables saved
 ;
 I $get(vzstr)="" Q 
 ;
 N vzi N vzptr
 N vzfld
 ;
 S vzptr=$$LV2V^MSG(vzstr,.vzfld)
 F vzi=1:2 Q:'($D(vzfld(vzi))#2)  S @vzfld(vzi)=vzfld(vzi+1)
 Q 
 ;
getdata(frm,sel,cid,fsn,vdd,ext,dsc,sqlsp,prot) ; Get account data
 ;
  S ER=0
 ;
 N i N j N n N num N pos
 N dftdes N di N p N par N pro N t N v N x N y N z N zz
 ;
 S num=$L(sel,",")
 S x=$get(ACNDTL("L",cid))
 S y=$get(ACNDTL("V",cid))
 S p=$get(ACNDTL("P",cid))
 S pos=$S(x="":0,1:$L(x,","))+1
 ;
 S n=0
 S z=""
 F i=1:1:num D
 .	S di=$piece(sel,",",i)
 .	I di="" Q 
 .	;
 .	I di["/RH=" D
 ..		S zz=$piece(di,"=",2)
 ..		S di=$piece(di,"/RH",1)
 ..		S dftdes(di)=zz
 ..		Q 
 .	;
 .	; Include only data items not yet fetched
 .	I (","_x_",")'[(","_di_",") S n=n+1 S $piece(z,",",n)=di
 .	;
 .	; Accomodate bracket syntax for file names
 .	I di["[" S zfil=$piece($piece(di,"]",1),"[",2) D
 ..		I (","_frm_",")'[(","_zfil_",") S frm=frm_","_zfil
 ..		Q 
 .	Q 
 ;
 I z'="" D  I ER Q ""
 .	;Update temporary "master" list
 .	S $piece(x,",",pos)=z
 .	S pro=""
 .	;
 .	;Only set on host, client defaults
 .	I '$get(%LOGID) S par="/PROTECTION=1"
 .	E  S par=""
 .	;
 .	;Execute stored procedure
 .	I $get(sqlsp) D EXECSP(frm,cid,.pro) Q:ER 
 .	;
 .	;Get from database
 .	I '$get(sqlsp) D  Q:ER 
 ..		;
 ..		N CID S CID=cid ; Needed for dynamic select
 ..		N row
 ..		;
 ..		;    #ACCEPT Date=12/05/05; Pgm=RussellDS; CR=18400
 ..		N rs,exe,sqlcur,vd,vi,vsql,vsub S rs=$$vOpen0(.exe,.vsql,z,frm,"CID=:CID","","",par,1)
 ..		;
 ..		I '$G(vobj(rs,0)) D  K vobj(+$G(rs)) Q 
 ...			;
 ...			S ER=1
 ...			; Invalid account ~p1
 ...			S RM=$$^MSG(1259,cid)
 ...			Q 
 ..		;
 ..		S row=$$vFetch0(rs) ; Fetch row
 ..		S row=vobj(rs)
 ..		S row=$translate(row,$char(9),$char(28))
 ..		;
 ..		S $piece(y,$char(28),pos)=row
 ..		S pro=$G(vobj(rs,.1))
 ..		K vobj(+$G(rs)) Q 
 .	;
 .	;Append to master list
 .	S p=p_$get(pro)
 .	Q 
 ;
 S z=""
 ;
 ; Construct return value
 F i=1:1:num S di=$piece(sel,",",i) I di'="" D
 .	I di["/RH=" S di=$piece(di,"/RH",1)
 .	;
 .	F j=1:1:$L(x,",") I $piece(x,",",j)=di D  Q 
 ..		S v=$piece(y,$char(28),j)
 ..		S prot=$get(prot)_$E(p,j)
 ..		I $get(ext) D fmt
 ..		S $piece(z,$char(28),i)=v
 ..		Q 
 .	Q 
 ;
 S ACNDTL("L",cid)=x ; Master select list
 S ACNDTL("V",cid)=y ; Master values list
 S ACNDTL("P",cid)=p ; Master protection list
 Q z
 ;
fmt ; Format data values
 ;
 N dec N k
 N des N dinam N distr N files N typ N x
 ;
 ; Load data item info from DBTBL1
 S x=$$DI^DBSDD(di,frm,.vdd)
 S files=frm
 S dinam=di
 ;
 I di["[" D
 .	S files=$piece($piece(di,"]",1),"[",2)
 .	S dinam=$piece(di,"]",2)
 .	Q 
 ;
 F k=1:1:$L(files,",") D  Q:x'="" 
 .	S distr=%LIBS_"."_$piece(files,",",k)_"."_dinam
 .	S x=$get(vdd(distr))
 .	Q:x'="" 
 .	;
 .	S distr="SYSDEV."_$piece(files,",",k)_"."_dinam
 .	S x=$get(vdd(distr))
 .	Q 
 ;
 S des=$piece(x,"|",10)
 S typ=$piece(x,"|",9)
 S dec=$piece(x,"|",14)
 ;
 I "TUF"'[typ S v=$$EXT^%ZM(v,typ,dec)
 ;
 I ($D(dftdes(dinam))#2) S $piece(dsc,"|",i)=dftdes(dinam)
 E  S $piece(dsc,"|",i)=des
 Q 
 ;
EXECSP(frm,cid,sqli) ; Execute the stored procedure
 ;
 N expr N sqlsts N sqldta N sqlcnt
 ;
 S expr="SELECT "_z_" FROM "_frm_" WHERE CID=:CID"
 S expr=$translate(expr,"][",".")
 S par("USING")="CID="_cid
 S par("DQMODE")=1
 ;
 S ER=$$SPCLI^SQLCACHE(expr,.par,.sqlsts,.sqldta,.sqlcnt,.sqli)
 ;
 ;SP failed, try again with RETREC
 I ER S sqlsp=0 Q 
 ;
 S $piece(y,$char(28),pos)=$translate(sqldta,$char(9),$char(28))
 ;
 Q 
 ;
STATS(vzsvstat,vzsrvcls,vztime) ; 
 ;
 ; Accumulate stats
 ;
 ; Incremement reply cnt/elapsed time
 S vzsvstat(vzsrvcls)=$get(vzsvstat(vzsrvcls))
 S $piece(vzsvstat(vzsrvcls),"|",2)=$piece(vzsvstat(vzsrvcls),"|",2)+1
 S $piece(vzsvstat(vzsrvcls),"|",3)=$piece(vzsvstat(vzsrvcls),"|",3)+vztime
 ;
 ; Minimum/maximum response times
 I '$piece(vzsvstat(vzsrvcls),"|",9) S $piece(vzsvstat(vzsrvcls),"|",9)=vztime
 I vztime<$piece(vzsvstat(vzsrvcls),"|",9) S $piece(vzsvstat(vzsrvcls),"|",9)=vztime
 I vztime>$piece(vzsvstat(vzsrvcls),"|",10) S $piece(vzsvstat(vzsrvcls),"|",10)=vztime
 Q 
 ;
ERSTAT(vzsvstat,vzsrvcls,vzrm) ; 
 ;
 N vzsevere
 ;
 ; Severity level
 S vzsevere=$get(%ZTPRIO)
 ;
 I 'vzsevere D
 .	N er N ercat N x
 .	N fld
 .	;
 .	S x=$$LV2V^MSG(vzrm,.fld)
 .	Q:(fld(1)'="ER") 
 .	S x=$get(fld(3))
 .	I x="" Q 
 .	;
 .	S ercat=$piece(x,"_",1)
 .	S er=$piece(x,"_",2,99)
 .	;
 .	I ercat="ER" S ercat="STBLER"
 .	E  I ercat="MSG" S ercat="STBLMSG"
 .	E  Q 
 .	;
 .	; Get level of severity of the error
 .	S vzsevere=$$ERRLOS^%ZFUNC(er,ercat)
 .	Q 
 ;
 I 'vzsevere S vzsevere=1
 S $piece(vzsvstat(vzsrvcls),"|",vzsevere+3)=$piece($get(vzsvstat(vzsrvcls)),"|",vzsevere+3)+1
 Q 
 ;
FILSTAT(vzsvtyp,vzsvid,vzsvstat,vztimer) ; 
 N vTp
 ;
 N svstatz S svstatz=$G(^SVSTAT(vzsvtyp,vzsvid))
 ;
 ; Stats were zeroed out
 I $P(svstatz,$C(124),1) D  Q 
 .	K vzsvstat
 .	 N V1,V2 S V1=vzsvtyp,V2=vzsvid D vDbDe3()
 .	 N V3,V4 S V3=vzsvtyp,V4=vzsvid  ZWI ^SVSTAT(V3,V4)
 .	Q 
 ;
 N time
 N srvcls
 ;
 S time=$$TIM($h)
 I vztimer>time Q 
 S vztimer=time+(vzsttim*60)
 ;
 S srvcls=""
 F  S srvcls=$order(vzsvstat(srvcls)) Q:srvcls=""  D
 .	;
 .	N svstat,vop1,vop2,vop3,vop4 S vop3=vzsvtyp,vop2=vzsvid,vop1=srvcls,svstat=$$vRCgetRecord1Opt^RecordSVSTAT(vzsvtyp,vzsvid,srvcls,0,.vop4)
 .  S $P(svstat,$C(124),1)=$piece(vzsvstat(srvcls),"|",1)
 .  S $P(svstat,$C(124),2)=$piece(vzsvstat(srvcls),"|",2)
 .  S $P(svstat,$C(124),3)=$piece(vzsvstat(srvcls),"|",3)
 .  S $P(svstat,$C(124),4)=$piece(vzsvstat(srvcls),"|",4)
 .  S $P(svstat,$C(124),5)=$piece(vzsvstat(srvcls),"|",5)
 .  S $P(svstat,$C(124),6)=$piece(vzsvstat(srvcls),"|",6)
 .  S $P(svstat,$C(124),7)=$piece(vzsvstat(srvcls),"|",7)
 .  S $P(svstat,$C(124),8)=$piece(vzsvstat(srvcls),"|",8)
 .  S $P(svstat,$C(124),9)=$piece(vzsvstat(srvcls),"|",9)
 .  S $P(svstat,$C(124),10)=$piece(vzsvstat(srvcls),"|",10)
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^SVSTAT(vop3,vop2,vop1)=$$RTBAR^%ZFUNC(svstat) S vop4=1 TC:vTp  
 . Q 
 Q 
 ;
TIM(x) ; Convert extended %CurrentDate to time stamp
 ;
 Q (x*1E5)+$piece(x,",",2)
 ;
CTRL(vzsvtyp,vzsvid,vzcsid,vzactive) ; Process control message(s)
 N vTp
 ;
 N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch2^"_$T(+0)
 ;
 N cmd N msg N mx
 ;
 N rs,vos1,vos2,vos3,vos4,vos5,vos6  N V1,V2 S V1=vzsvtyp,V2=vzsvid S rs=$$vOpen6()
 ;
 F  Q:'$$vFetch6()  D
 . S msg=$P(rs,$C(9),1)
 . S SVSEQ=$P(rs,$C(9),2)
 .	;
 .	 N V3,V4 S V3=vzsvtyp,V4=vzsvid  ZWI ^SVCTRL(V3,V4,SVSEQ)
 .	;
 .	S cmd=$E(msg,1,4)
 .	;
 .	I cmd="ROLE" D  Q 
 ..		N role
 ..		;
 ..		S role=$piece(msg,"=",2)
 ..		I role="PRIMARY" S vzactive=1
 ..		I role="SECONDARY" S vzactive=0
 ..		;
 ..		N svctrlt,vop1,vop2,vop3 S vop2=vzsvtyp,vop1=vzsvid,svctrlt=$$vRCgetRecord0Opt^RecordSVCTRLT(vzsvtyp,vzsvid,0,.vop3)
 ..	  S $P(svctrlt,$C(124),4)=role
 ..	  S $P(svctrlt,$C(124),5)=$h
 ..		;
 ..	 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^SVCTRL(vop2,vop1)=$$RTBAR^%ZFUNC(svctrlt) S vop3=1 TC:vTp  
 ..  Q 
 .	;
 .	I cmd="JRNL" D  Q 
 ..		;
 ..		N ER
 ..		N IO
 ..		;
 ..		S ER=0
 ..		S IO=$piece(msg,"=",2)
 ..		;
 ..		I IO="" Q 
 ..		;
 ..		I IO="OFF" D
 ...			S IO=$get(vzjrnl)
 ...			I IO="" Q 
 ...			D CLOSE^SCAIO
 ...			S vzjrnl=""
 ...			Q 
 ..		;
 ..		E  D
 ...			S vzjrnl=IO
 ...			D OPEN^SCAIO
 ...			I ER S vzjrnl=""
 ...			Q 
 ..		Q 
 .	;
 .	I cmd="STOP" D CTRLSTOP
 .	;
 .	I cmd="PROF" D  Q 
 ..		;
 ..		; M Profiling is only written to an M database and is not
 ..		; supported to write to any other RDBMS
 ..		;    #ACCEPT Date=12/05/05; Pgm=RussellDS;CR=18400
 ..		;*** Start of code by-passed by compiler
 ..		if $P($ZVERSION,"GT.M V",2)<4.2 quit
 ..		if '$P(msg,"=",2) view "TRACE":0 quit
 ..		set seq=$O(^SVPROF(vzsvtyp,vzsvid,""),-1)+1
 ..		view "TRACE":1:"^SVPROF("""_vzsvtyp_""","""_vzsvid_""","""_seq_""")"
 ..		;*** End of code by-passed by compiler ***
 ..		Q 
 .	;
 .	I cmd="LINK" D RESET Q 
 .	;
 .	;   #ACCEPT PGM=Erik Scheetz;DATE=11/21/02;CR=18400
 .	I cmd="EXEC" XECUTE $piece(msg," ",2,9999) Q 
 .	;
 .	I cmd="TRAP" S vztrap=$piece(msg,"=",2) Q 
 .	Q 
 ;
 Q 
 ;
CTRLSTOP ; 
 ;
 S ET=$$SVDSCNCT^%MTAPI(vzcsid)
 I '(ET="") D ERRLOG(ET,'vzactive)
 ;
  N V1,V2 S V1=vzsvtyp,V2=vzsvid D vDbDe4()
  N V3,V4 S V3=vzsvtyp,V4=vzsvid  ZWI ^SVCTRL(V3,V4)
  N V5,V6 S V5=vzsvtyp,V6=vzsvid D vDbDe5()
  N V7,V8 S V7=vzsvtyp,V8=vzsvid  ZWI ^SVSTAT(V7,V8)
 ;
 ; Unregister M Process
 D CLOSE^IPCMGR()
 HALT 
 ;
 Q 
 ;
INIT(%SN) ; Private Initialize and save system variables
 ;
 N %CO N %CRCD N %EMUCRCD N %ED N %IDENT N %LIBS
 N %LOGID N %MCP N %MSK N %ODP N %RESPROC
 N %SVCHNID N %UCLS N %UID N TLO N %VN N %VNC
 ;
 I $get(%SN)="" S %SN="PBS"
 ;
 ; On-line
 S %NET=1
 ;
 D SYSVAR^SCADRV0() ; Init system variables
 S %LOGID=$$LOGID^SCADRV ; Init login information
 ;
 Q $$VSAV("*")
 ;
XKILL ; Exclusive kill
 ;
 N X
 ;
 S X="(vzactive,vzcache,vzclid,vzcpmsg,vzcsid,vzDbCache,vzfaptbl,vzgbldir,vzident,"
 S X=X_"vzjrnl,vzlasttjd,vzlogmsg,vzlogrep,vzmsgpgm,vzmtname,vzpkt,"
 S X=X_"vzpktid,vztprest,vzsav,vzsvchnl,vzsvfap,vzsvid,vzsvsec,vzstart,"
 S X=X_"vzsvstat,vzsttim,vzsvtyp,vztime,vztimer,vztimout,vztrap,"
 S X=X_"%CACHE,%INTRPT,%SVCNTXT,%UCLS)"
 ;
 ;  #ACCEPT Date=12/05/05; Pgm=RussellDS;CR=18400
 ;*** Start of code by-passed by compiler
 kill @X
 ;*** End of code by-passed by compiler ***
 ;
 I ((($P($H,",",2)-$get(%CACHE))>300)!(($P($H,",",2)-$get(%CACHE))<0)) D
 .	;
 .	D vKill1("") K %CACHE,vzcache
 .	S %CACHE=$P($H,",",2)
 .	Q 
 ;
 Q 
 ;
LOG(token,msgid,clmsg,reply,status,srvcls,server,logmsg,logrep,vztime,vztprest) ; TP restart count
 N vTp
 ;
 ; Log message/reply in MSGLOG file
 ;
 I ($get(token)="") Q 
 I ($get(msgid)="") Q 
 ;
 N msglog,vop1,vop2,vop3 S msglog="",vop3=0
 ;
  S vop2=token
  S vop1=msgid
  S $P(msglog,$C(124),1)=$J
  S $P(msglog,$C(124),2)=$H
  S $P(msglog,$C(124),3)=$get(status)
  S $P(msglog,$C(124),4)=$get(srvcls)
  S $P(msglog,$C(124),5)=$get(server)
  S $P(msglog,$C(124),6)=$get(vztime)
  S $P(msglog,$C(124),7)=$get(vztprest)
 ;
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^MSGLOG(vop2,vop1)=$$RTBAR^%ZFUNC(msglog) S vop3=1 TC:vTp  
 ;
 I $get(logmsg) D LOGFILE(clmsg,1)
 I $get(logrep) D LOGFILE(reply,2)
 Q 
 ;
LOGFILE(msg,typ) ; 
 N vTp
 ;
 ; Log the messages/replies
 ;
 N msglogseq S msglogseq=$$vcdmNew^RecordMSGLOGSEQ()
  S vobj(msglogseq,1,1)=""
 ;
  S vobj(msglogseq,-3)=token
  S vobj(msglogseq,-4)=msgid
  S vobj(msglogseq,-5)=typ
  S vobj(msglogseq,1,1)=msg
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordMSGLOGSEQ(msglogseq,"/CASDEL/INDEX/NOJOURNAL/LOG/NOTRIGAFT/NOTRIGBEF/UPDATE/NOVALDD/VALFK/NOVALREQ/NOVALRI/NOVALST/") K vobj(msglogseq,-100) S vobj(msglogseq,-2)=1 TC:vTp  
 ;
 K vobj(+$G(msglogseq)) Q 
 ;
OK2PROC() ; Check if we can process this message
 ;
 ; Attempt to get lock within failover wait time (in seconds)
 ;
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 L +FAILOVER:""
 ;
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 L -FAILOVER
 ;
 ; If the role is now PRIMARY, client message can be processed
 I $$ROLE="PRIMARY" Q 1
 ;
 ; Otherwise, do not process client message
 Q 0
 ;
SHUTDOWN(SVTYP,SVID) ; Controlled shutdown
 N vTp
 ;
 N INIFIL N SCRIPT N X
 ;
 N svctrlt S svctrlt=$$vRCgetRecord0Opt^RecordSVCTRLT(SVTYP,SVID,0,"")
 ;
 I $P(svctrlt,$C(124),6) D  HALT 
 .	;
 .	D vDbDe6()
 .  ZWI ^SVCTRL(SVTYP,SVID)
 .	D vDbDe7()
 .  ZWI ^SVSTAT(SVTYP,SVID)
 .	;
 .	; Unregister M process
 .	D CLOSE^IPCMGR()
 .	Q 
 ;
 ; Inform remaining servers that shutdown is already in process
 N ds,vos1,vos2,vos3,vos4 S ds=$$vOpen7()
 ;
 F  Q:'$$vFetch7()  D
 . N svctrlt,vop1,vop2,vop3 S vop2=$P(ds,$C(9),1),vop1=$P(ds,$C(9),2),svctrlt=$$vRCgetRecord1Opt^RecordSVCTRLT(vop2,vop1,1,.vop3),vop3=1
 .	;
 .  S $P(svctrlt,$C(124),6)=1
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^SVCTRL(vop2,vop1)=$$RTBAR^%ZFUNC(svctrlt) S vop3=1 TC:vTp  
 . Q 
 ;
 ; Define input params
 S SCRIPT=$$SCAU^%TRNLNM("REPL_CONTROLLED")
 S INIFIL=$$SCAU^%TRNLNM("REPL_CONTROLLED_INIT")
 ;
 ; Execute controlled shutdown script
 S X=$$FAILOVER^%OSSCRPT(SCRIPT,INIFIL,"")
 ;
 D vDbDe8()
  ZWI ^SVCTRL(SVTYP,SVID)
 D vDbDe9()
  ZWI ^SVSTAT(SVTYP,SVID)
 ;
 ; Unregister M process
 D CLOSE^IPCMGR()
 ;
 HALT 
 Q 
 ;
FAILOVER ; Failover startup (switch from SECONDARY to PRIMARY role)
 ;
 ; If lock does not succeed, failover startup is already in process.
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 L +FAILOVER:0 E  Q 
 ;
 ; Role is no longer SECONDARY; failover startup script already run.
 I $$ROLE'="SECONDARY" L -FAILOVER Q 
 ;
 N INIFIL N SCRIPT N X
 ;
 S SCRIPT=$$SCAU^%TRNLNM("REPL_FAILOVER")
 S INIFIL=$$SCAU^%TRNLNM("REPL_FAILOVER_INIT")
 ;
 ; Execute failover startup script
 S X=$$FAILOVER^%OSSCRPT(SCRIPT,INIFIL,"")
 ;
 ; Release the FAILOVER lock
 ;  #ACCEPT PGM=Erik Scheetz;DATE=11/22/02;CR=unknown
 L -FAILOVER
 Q 
 ;
CTRLMSG(MSG) ; Send control message to all servers
 N vTp
 ;
 N SVID N SVSEQ
 N SVTYP
 ;
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen8()
 I '$G(vos1) Q 
 ;
 F  Q:'$$vFetch8()  D
 . S SVTYP=$P(rs,$C(9),1)
 . S SVID=$P(rs,$C(9),2)
 .	;
 .	S SVSEQ=$O(^SVCTRL(SVTYP,SVID,""),-1)+1
 .	;
 .	N svctrl,vop1,vop2,vop3,vop4 S svctrl="",vop4=0
 .  S vop3=SVTYP
 .  S vop2=SVID
 .  S vop1=SVSEQ
 .  S $P(svctrl,$C(124),1)=MSG
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^SVCTRL(vop3,vop2,vop1)=$$RTBAR^%ZFUNC(svctrl) S vop4=1 TC:vTp  
 .	;
 .	; Invoke CTRL interrupt to server process
 . D SIGNAL^IPCMGR($$HEXDEC^%ZHEX($P(rs,$C(9),3)),"CTRL")
 . Q 
 Q 
 ;
ROLE() ; Return system role from SCA_STATUS.DAT
 ;
 N IO N RM N X
 ;
 ; Default to PRIMARY if replication is not utilized
 I $$TRNLNM^%ZFUNC("START_REPLICATION")="" Q "PRIMARY"
 ;
 S IO=$$FILE^%TRNLNM("SCA_STATUS.DAT","SCAU_REPL_DIR")
 I '$$FILE^%ZOPEN(IO,"READ",2) Q "UNKNOWN"
 ;
 S X=$$^%ZREAD(IO,.RM)
 CLOSE IO
 USE 0
 I +RM Q "UNKNOWN"
 ;
 I X["PRIMARY" Q "PRIMARY"
 ;
 I X["SECONDARY" Q "SECONDARY"
 ;
 Q "UNKNOWN"
 ;
CHKALL(opt,tim,srvs,mons) ; Check status of servers/monitors
 ;
 N ACTIVE N exit N i
 N srv N typ
 ;
 I +$get(tim)=0 S tim=5
 S exit=$$TIM($h)+tim
 S srvs=$get(srvs)
 S mons=$get(mons)
 ;
 S ACTIVE=0
 F i=1:1 D  Q:ACTIVE!(typ="")!($$TIM($h)>exit) 
 .	S typ=$piece(mons,",",i)
 .	I typ="" Q 
 .	F  S ACTIVE=$$MONSTS(typ) Q:ACTIVE=opt!($$TIM($h)>exit)  HANG 1
 .	Q 
 ;
 ; If any server is ACTIVE, no need to check further
 I ACTIVE Q 1
 ;
 F i=1:1 D  Q:ACTIVE!(typ="")!($$TIM($h)>exit) 
 .	S typ=$piece(srvs,",",i) I typ="" Q 
 .	F  S ACTIVE=$$SRVSTS(typ) Q:ACTIVE=opt!($$TIM($h)>exit)  HANG 1
 .	Q 
 ;
 Q ACTIVE
 ;
MONSTS(MON) ; Check monitor status
 ;
 N ACTIVE
 ;
 S ACTIVE=0
 ;
 I MON="TGLMON" D
 .	N tglpa S tglpa=$$vRCgetRecord0Opt^RecordTGLMONPA("PA",0,"")
 . I ($P(tglpa,$C(124),1)="") Q 
 .	I $$VALID^%ZPID($P(tglpa,$C(124),1),1) S ACTIVE=1
 . Q 
 ;
 Q ACTIVE
 ;
SRVSTS(SVTYP) ; Check server status
 ;
 N ACTIVE
 N LIST
 N PID
 ;
 D ^%ZPID(.LIST)
 S ACTIVE=0
 ;
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen9()
 ;
 F  Q:'$$vFetch9()  D  Q:ACTIVE 
 . S PID=$$HEXDEC^%ZHEX(rs)
 .	I '($D(LIST(PID))#2) Q 
 .	S ACTIVE=1
 .	Q 
 ;
 Q ACTIVE
 ;
DSPSTS(io) ; Display server/monitor status to output file
 ;
 N pid N x
 N list
 ;
 S x=$$FILE^%ZOPEN(io,"WRITE/NEWV")
 I 'x S ER=1 S RM=$piece(x,"|",2) Q 
 D ^%ZPID(.list)
 ;
 USE io WRITE !!,"[Servers]"
 ;
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen10()
 ;
 I ''$G(vos1) F  Q:'$$vFetch10()  D
 . S pid=$$HEXDEC^%ZHEX($P(rs,$C(9),5))
 .	;
 .	USE io
 . WRITE !,$P(rs,$C(9),1),$char(9),$P(rs,$C(9),2),$char(9),pid
 . WRITE $char(9),$P(rs,$C(9),3),$P(rs,$C(9),4)
 . WRITE $char(9),$P(rs,$C(9),6)
 .	;
 .	I '($D(list(pid))#2) WRITE $char(9),"[no such process]"
 .	Q 
 ;
 USE io WRITE !!,"[Monitors]"
 ;
 N tglpa S tglpa=$$vRCgetRecord0Opt^RecordTGLMONPA("PA",0,"")
 ;
 I $P(tglpa,$C(124),1)'="" D
 .	S pid=$$HEXDEC^%ZHEX($P(tglpa,$C(124),1))
 .	USE io WRITE !!,"TGLMON",$char(9),pid
 .	I '($D(list(pid))#2) WRITE $char(9),"[no such process]"
 .	Q 
 ;
 CLOSE io
 USE 0
 Q 
 ;
RESET ; Re-link programs into image (LINK control message)
 ;
 N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch3^"_$T(+0)
 ;
 N CODE N PGM N rtn
 ;
 D IMAGE^%ZRTNS(.rtn)
 K rtn("PBSUTL")
 ;
 S rtn=""
 F  S rtn=$order(rtn(rtn)) Q:rtn=""  D
 .	;
 .	N $ET,$ES,$ZYER S $ZYER="ZE^UCGMR",$ZE="",$EC="",$ET="D:$TL>"_$TL_" rollback^vRuntime("_$TL_") Q:$Q&$ES """" Q:$ES  N voxMrk s voxMrk="_+$O(vobj(""),-1)_" G vCatch4^"_$T(+0)
 .	;
 .	; Replace % character with _ character prior to re-linking
 .	S PGM=$S($E(rtn,1)="%":"_"_$E(rtn,2,1048575),1:rtn)
 .	;
 .	D ^ULODTMPL(PGM,"CODE")
 .	D BUILDRTN^UCGM(.CODE,PGM,.CMPERR)
 .	Q 
 Q 
 ;
%STFHOST() ; Define value of %STFHOST
 ;
 N host1,vop1 S vop1="*",host1=$$vRCgetRecord1Opt^RecordSTFHOST1("*",0,"")
  S host1=$G(^STFHOST1(vop1,"CTL"))
 S %STFHOST=$P(host1,$C(124),1)
 ;
 I %STFHOST D
 .	;
 .	I $get(%STFPID)'="",%STFPID=$$PID^%ZFUNC S %STFHOST=0 Q 
 .	;
 .	N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen11()
 . I '$G(vos1) Q 
 .	F  Q:'$$vFetch11()  Q:%STFHOST'=3  D
 ..  S %STFHOST=rs
 ..		HANG 1
 ..		Q 
 . Q 
 ;
 Q %STFHOST
 ;
PROF(svtyp,svid,seq,IO) ; Utility to copy M-profiling results to ASCII file
 ;
 I $get(IO)="" S IO=$$FILE^%TRNLNM("zmprof_"_svtyp_"_"_svid_".dat","SCAU$SPOOL")
 I '$$FILE^%ZOPEN(IO,"WRITE/NEWV",2,1024) Q 
 ;
 USE IO
 ;
 WRITE "Program|Label|Offset|Cnt|Usr Time|Sys Time"
 ;
 ;  #ACCEPT Date=12/05/05; Pgm=RussellDS;CR=18400
 ;*** Start of code by-passed by compiler
 N lbl,off,pgm,x
 S (lbl,off,pgm)=""
 F  S pgm=$O(^SVPROF(svtyp,svid,seq,pgm)) Q:pgm=""  D
 . F  S lbl=$O(^SVPROF(svtyp,svid,seq,pgm,lbl)) Q:lbl=""  D
 ..  S x=$G(^SVPROF(svtyp,svid,seq,pgm,lbl))
 ..  I x'="" W !,pgm_"|"_lbl_"|*|",$TR(x,":","|")
 ..  F  S off=$O(^SVPROF(svtyp,svid,seq,pgm,lbl,off)) Q:off=""  D
 ...   S x=^SVPROF(svtyp,svid,seq,pgm,lbl,off)
 ...   W !,pgm_"|"_lbl_"|"_off_"|"
 ...   W $TR(x,":","|")
 ;*** End of code by-passed by compiler ***
 ;
 USE 0
 D CLOSE^SCAIO
 Q 
 ;
STOP2 ; Stops an Individual/Interactive Server
 ;
 N SVID
 N %READ N %TAB N SVTYP N VFMQ
 ;
 S SVTYP="SCA$IBS"
 ;
 S %TAB("SVTYP")="[SVCTRLT]SVTYP/TBL=[CTBLSVTYP]"
 S %TAB("SVID")="[SVCTRLT]SVID/TBL=[SVCTRLT]:QU ""[SVCTRLT]SVTYP=<<SVTYP>>"""
 ;
 S %READ="@@%FN,,,SVTYP/REQ,SVID/REQ"
 ;
 D ^UTLREAD
 ;
 I VFMQ="Q" Q 
 ;
 N svt S svt=$$vRCgetRecord0Opt^RecordSVCTRLT(SVTYP,SVID,0,"")
 ;
 D SIGNAL^IPCMGR($$HEXDEC^%ZHEX($P(svt,$C(124),3)),"STOP")
 ;
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "61495^38597^Badrinath Giridharan^37007" ; Signature - LTD^TIME^USER^SIZE
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe1() ; DELETE FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen12()
 F  Q:'$$vFetch12()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVCTRL(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe2() ; DELETE FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen13()
 F  Q:'$$vFetch13()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVSTAT(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vRsRowGC(vNms,vTps) ; Runtime ResultSet.getRow().getColumns()
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 ;
 N vL S vL="" N vN N vT N vO
 F vO=1:1:$S((vNms=""):0,1:$L(vNms,",")) D
 .	S vN=$piece(vNms,",",vO)
 .	S vT=$E(vTps,(vO-1)*2+1)
 .	I "TUF"[vT S vT="String"
 .	E  S vT=$piece("ByteString,Boolean,Date,Memo,Number,Number,Time",",",$F("BLDMN$C",vT)-1)
 .	S $piece(vL,",",v0)=vT_" "_vN
 .	Q 
 Q vL
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe3() ; DELETE FROM SVSTAT WHERE SRVTYP=:V1 AND SRVID=:V2
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen14()
 F  Q:'$$vFetch14()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVSTAT(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe4() ; DELETE FROM SVCTRL WHERE SVTYP=:V1 AND SVID=:V2
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen15()
 F  Q:'$$vFetch15()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVCTRL(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe5() ; DELETE FROM SVSTAT WHERE SRVTYP=:V5 AND SRVID=:V6
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen16()
 F  Q:'$$vFetch16()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVSTAT(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe6() ; DELETE FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen17()
 F  Q:'$$vFetch17()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVCTRL(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe7() ; DELETE FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen18()
 F  Q:'$$vFetch18()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVSTAT(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe8() ; DELETE FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen19()
 F  Q:'$$vFetch19()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVCTRL(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe9() ; DELETE FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5 S vRs=$$vOpen20()
 F  Q:'$$vFetch20()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^SVSTAT(v1,v2,v3)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ;
vKill1(ex1) ; Delete objects %CACHE()
 ;
 N n1 S (n1)=""
 F  S n1=$O(%CACHE(n1)) Q:n1=""  K:'((n1=ex1)) vobj(%CACHE(n1))
 Q
 ;
vOpen0(exe,vsql,vSelect,vFrom,vWhere,vOrderby,vGroupby,vParlist,vOff) ; Dynamic MDB ResultSet
 ;
 N vOid
 N ER,vExpr,mode,RM,vOpen,vTok S ER=0 ;=noOpti
 ;
 S vExpr="SELECT "_vSelect_" FROM "_vFrom
 I vWhere'="" S vExpr=vExpr_" WHERE "_vWhere
 I vOrderby'="" S vExpr=vExpr_" ORDER BY "_vOrderby
 I vGroupby'="" S vExpr=vExpr_" GROUP BY "_vGroupby
 S vExpr=$$UNTOK^%ZS($$SQL^%ZS(vExpr,.vTok),vTok)
 ;
 S sqlcur=$O(vobj(""),-1)+1
 ;
 I $$FLT^SQLCACHE(vExpr,vTok,.vParlist)
 E  S vOpen=$$OPEN^SQLM(.exe,vFrom,vSelect,vWhere,vOrderby,vGroupby,vParlist,,1,,sqlcur) I 'ER D SAV^SQLCACHE(vExpr,.vParlist) s vsql=vOpen
 I ER S $ZE="0,"_$ZPOS_",%PSL-E-SQLFAIL,"_$TR($G(RM),$C(10,44),$C(32,126)),$EC=",U1001,"
 ;
 S vOid=sqlcur
 S vobj(vOid,0)=vsql
 S vobj(vOid,-1)="ResultSet"
 S vobj(vOid,-2)="$$vFetch0^"_$T(+0)
 S vobj(vOid,-3)=$$RsSelList^UCDBRT(vSelect)
 S vobj(vOid,-4)=$G(vsql("D"))
 S vobj(vOid,-5)=0
 Q vOid
 ;
vFetch0(vOid) ; MDB dynamic FETCH
 ;
 ; type public String exe(),sqlcur,vd,vi,vsql()
 ;
 I vsql=0 S vobj(vOid)="" Q 0
 S vsql=$$^SQLF(.exe,.vd,.vi,.sqlcur)
 S vobj(vOid)=vd
 S vobj(vOid,0)=vsql
 S vobj(vOid,.1)=$G(vi)
 Q vsql
 ;
vOpen1() ; SVID FROM SVCTRLT WHERE SVTYP=:SVTYP
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL1a0
 S vos4=""
vL1a4 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL1a0
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
vOpen10() ; SVTYP,SVID,MTMID,MTMSVRID,PID,ROLE FROM SVCTRLT
 ;
 ;
 S vos1=2
 D vL10a1
 Q ""
 ;
vL10a0 S vos1=0 Q
vL10a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=""
vL10a3 S vos3=$O(^SVCTRL(vos3),1) I vos3="" G vL10a0
 S vos4=""
vL10a5 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL10a3
 Q
 ;
vFetch10() ;
 ;
 ;
 I vos1=1 D vL10a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos5=$G(^SVCTRL(vos3,vos4))
 S rs=$S(vos3=vos2:"",1:vos3)_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",1)_$C(9)_$P(vos5,"|",2)_$C(9)_$P(vos5,"|",3)_$C(9)_$P(vos5,"|",4)
 ;
 Q 1
 ;
vOpen11() ; LAST FROM STFHOST1 WHERE CTL>0
 ;
 ;
 S vos1=2
 D vL11a1
 Q ""
 ;
vL11a0 S vos1=0 Q
vL11a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=""
vL11a3 S vos3=$O(^STFHOST1(vos3),1) I vos3="" G vL11a0
 S vos4=$G(^STFHOST1(vos3,"CTL"))
 I '($P(vos4,"|",1)>0) G vL11a3
 Q
 ;
vFetch11() ;
 ;
 ;
 I vos1=1 D vL11a3
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos4=$G(^STFHOST1(vos3,"CTL"))
 S vos5=$G(^STFHOST1(vos3,"LAST"))
 S rs=$P(vos5,"|",1)
 ;
 Q 1
 ;
vOpen12() ; SVTYP,SVID,SVSEQ FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;
 S vos1=2
 D vL12a1
 Q ""
 ;
vL12a0 S vos1=0 Q
vL12a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL12a0
 S vos4=$G(SVID) I vos4="" G vL12a0
 S vos5=""
vL12a5 S vos5=$O(^SVCTRL(vos3,vos4,vos5),1) I vos5="" G vL12a0
 Q
 ;
vFetch12() ;
 ;
 ;
 I vos1=1 D vL12a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen13() ; SRVTYP,SRVID,SRVCLS FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;
 S vos1=2
 D vL13a1
 Q ""
 ;
vL13a0 S vos1=0 Q
vL13a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL13a0
 S vos4=$G(SVID)
 S vos5=""
vL13a5 S vos5=$O(^SVSTAT(vos3,vos4,vos5),1) I vos5="" G vL13a0
 Q
 ;
vFetch13() ;
 ;
 ;
 I vos1=1 D vL13a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen14() ; SRVTYP,SRVID,SRVCLS FROM SVSTAT WHERE SRVTYP=:V1 AND SRVID=:V2
 ;
 ;
 S vos1=2
 D vL14a1
 Q ""
 ;
vL14a0 S vos1=0 Q
vL14a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1) I vos3="" G vL14a0
 S vos4=$G(V2)
 S vos5=""
vL14a5 S vos5=$O(^SVSTAT(vos3,vos4,vos5),1) I vos5="" G vL14a0
 Q
 ;
vFetch14() ;
 ;
 ;
 I vos1=1 D vL14a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen15() ; SVTYP,SVID,SVSEQ FROM SVCTRL WHERE SVTYP=:V1 AND SVID=:V2
 ;
 ;
 S vos1=2
 D vL15a1
 Q ""
 ;
vL15a0 S vos1=0 Q
vL15a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1) I vos3="" G vL15a0
 S vos4=$G(V2) I vos4="" G vL15a0
 S vos5=""
vL15a5 S vos5=$O(^SVCTRL(vos3,vos4,vos5),1) I vos5="" G vL15a0
 Q
 ;
vFetch15() ;
 ;
 ;
 I vos1=1 D vL15a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen16() ; SRVTYP,SRVID,SRVCLS FROM SVSTAT WHERE SRVTYP=:V5 AND SRVID=:V6
 ;
 ;
 S vos1=2
 D vL16a1
 Q ""
 ;
vL16a0 S vos1=0 Q
vL16a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V5) I vos3="" G vL16a0
 S vos4=$G(V6)
 S vos5=""
vL16a5 S vos5=$O(^SVSTAT(vos3,vos4,vos5),1) I vos5="" G vL16a0
 Q
 ;
vFetch16() ;
 ;
 ;
 I vos1=1 D vL16a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen17() ; SVTYP,SVID,SVSEQ FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;
 S vos1=2
 D vL17a1
 Q ""
 ;
vL17a0 S vos1=0 Q
vL17a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL17a0
 S vos4=$G(SVID) I vos4="" G vL17a0
 S vos5=""
vL17a5 S vos5=$O(^SVCTRL(vos3,vos4,vos5),1) I vos5="" G vL17a0
 Q
 ;
vFetch17() ;
 ;
 ;
 I vos1=1 D vL17a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen18() ; SRVTYP,SRVID,SRVCLS FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;
 S vos1=2
 D vL18a1
 Q ""
 ;
vL18a0 S vos1=0 Q
vL18a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL18a0
 S vos4=$G(SVID)
 S vos5=""
vL18a5 S vos5=$O(^SVSTAT(vos3,vos4,vos5),1) I vos5="" G vL18a0
 Q
 ;
vFetch18() ;
 ;
 ;
 I vos1=1 D vL18a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen19() ; SVTYP,SVID,SVSEQ FROM SVCTRL WHERE SVTYP=:SVTYP AND SVID=:SVID
 ;
 ;
 S vos1=2
 D vL19a1
 Q ""
 ;
vL19a0 S vos1=0 Q
vL19a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL19a0
 S vos4=$G(SVID) I vos4="" G vL19a0
 S vos5=""
vL19a5 S vos5=$O(^SVCTRL(vos3,vos4,vos5),1) I vos5="" G vL19a0
 Q
 ;
vFetch19() ;
 ;
 ;
 I vos1=1 D vL19a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen2() ; QNAME,CPID,PID FROM SVCTRLCP WHERE QNAME=:QNAME
 ;
 ;
 S vos1=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos1=0 Q
vL2a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(QNAME) I vos3="" G vL2a0
 S vos4=""
vL2a4 S vos4=$O(^SVCTRLCP(vos3,vos4),1) I vos4="" G vL2a0
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
 S vos5=$G(^SVCTRLCP(vos3,vos4))
 S rs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",1)
 ;
 Q 1
 ;
vOpen20() ; SRVTYP,SRVID,SRVCLS FROM SVSTAT WHERE SRVTYP=:SVTYP AND SRVID=:SVID
 ;
 ;
 S vos1=2
 D vL20a1
 Q ""
 ;
vL20a0 S vos1=0 Q
vL20a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL20a0
 S vos4=$G(SVID)
 S vos5=""
vL20a5 S vos5=$O(^SVSTAT(vos3,vos4,vos5),1) I vos5="" G vL20a0
 Q
 ;
vFetch20() ;
 ;
 ;
 I vos1=1 D vL20a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_vos4_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen3() ; MAX(MSEQ) FROM SVCTRLTCP WHERE QNAME=:QNAME AND CPID=:CPID
 ;
 ;
 S vos6=2
 D vL3a1
 Q ""
 ;
vL3a0 S vos6=0 Q
vL3a1 S vos7=$$BYTECHAR^SQLUTL(254)
 S vos8=$G(QNAME) I vos8="" G vL3a0
 S vos9=$G(CPID)
 S vos10=""
vL3a5 S vos10=$O(^SVCTRLCP(vos8,vos9,vos10),1) I vos10="" G vL3a8
 S vos11=$S($G(vos11)="":$S(vos10=vos7:"",1:vos10),vos11<$S(vos10=vos7:"",1:vos10):$S(vos10=vos7:"",1:vos10),1:vos11)
 G vL3a5
vL3a8 I $G(vos11)="" S vd="" G vL3a0
 Q
 ;
vFetch3() ;
 ;
 ;
 I vos6=1 D vL3a8
 I vos6=2 S vos6=1
 ;
 I vos6=0 S rs1="" Q 0
 ;
 S rs1=$G(vos11)
 S vos6=100
 ;
 Q 1
 ;
vOpen4() ; SVID,PID FROM SVCTRLT WHERE SVTYP=:SVTYP
 ;
 ;
 S vos1=2
 D vL4a1
 Q ""
 ;
vL4a0 S vos1=0 Q
vL4a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL4a0
 S vos4=""
vL4a4 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL4a0
 Q
 ;
vFetch4() ;
 ;
 ;
 I vos1=1 D vL4a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos5=$G(^SVCTRL(vos3,vos4))
 S rs=$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",3)
 ;
 Q 1
 ;
vOpen5() ; PID FROM SVCTRLT WHERE SVTYP=:SVTYP AND MTMID=:MTMID
 ;
 ;
 S vos1=2
 D vL5a1
 Q ""
 ;
vL5a0 S vos1=0 Q
vL5a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL5a0
 S vos4=$G(MTMID) I vos4="",'$D(MTMID) G vL5a0
 S vos5=""
vL5a5 S vos5=$O(^SVCTRL(vos3,vos5),1) I vos5="" G vL5a0
 S vos6=$G(^SVCTRL(vos3,vos5))
 I '($P(vos6,"|",1)=vos4) G vL5a5
 Q
 ;
vFetch5() ;
 ;
 ;
 I vos1=1 D vL5a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos6=$G(^SVCTRL(vos3,vos5))
 S rs=$P(vos6,"|",3)
 ;
 Q 1
 ;
vOpen6() ; SVEXPR,SVSEQ FROM SVCTRL WHERE SVTYP=:V1 AND SVID=:V2
 ;
 ;
 S vos1=2
 D vL6a1
 Q ""
 ;
vL6a0 S vos1=0 Q
vL6a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1) I vos3="" G vL6a0
 S vos4=$G(V2) I vos4="" G vL6a0
 S vos5=""
vL6a5 S vos5=$O(^SVCTRL(vos3,vos4,vos5),1) I vos5="" G vL6a0
 Q
 ;
vFetch6() ;
 ;
 ;
 I vos1=1 D vL6a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos6=$G(^SVCTRL(vos3,vos4,vos5))
 S rs=$P(vos6,"|",1)_$C(9)_$S(vos5=vos2:"",1:vos5)
 ;
 Q 1
 ;
vOpen7() ; SVTYP,SVID FROM SVCTRLT WHERE SVTYP=:SVTYP
 ;
 ;
 S vos1=2
 D vL7a1
 Q ""
 ;
vL7a0 S vos1=0 Q
vL7a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL7a0
 S vos4=""
vL7a4 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL7a0
 Q
 ;
vFetch7() ;
 ;
 ;
 I vos1=1 D vL7a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen8() ; SVTYP,SVID,PID FROM SVCTRLT
 ;
 ;
 S vos1=2
 D vL8a1
 Q ""
 ;
vL8a0 S vos1=0 Q
vL8a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=""
vL8a3 S vos3=$O(^SVCTRL(vos3),1) I vos3="" G vL8a0
 S vos4=""
vL8a5 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL8a3
 Q
 ;
vFetch8() ;
 ;
 ;
 I vos1=1 D vL8a5
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos5=$G(^SVCTRL(vos3,vos4))
 S rs=$S(vos3=vos2:"",1:vos3)_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",3)
 ;
 Q 1
 ;
vOpen9() ; PID FROM SVCTRLT WHERE SVTYP=:SVTYP
 ;
 ;
 S vos1=2
 D vL9a1
 Q ""
 ;
vL9a0 S vos1=0 Q
vL9a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SVTYP) I vos3="" G vL9a0
 S vos4=""
vL9a4 S vos4=$O(^SVCTRL(vos3,vos4),1) I vos4="" G vL9a0
 Q
 ;
vFetch9() ;
 ;
 ;
 I vos1=1 D vL9a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos5=$G(^SVCTRL(vos3,vos4))
 S rs=$P(vos5,"|",3)
 ;
 Q 1
 ;
vCatch4 ; Error trap
 ;
 N vError,$ET,$ES S vError=$ZE,$EC="",$ET="Q",$ZE=""
 D ZX^UCGMR(voxMrk) Q 
 D ZX^UCGMR(voxMrk) Q 
 ;
vCatch3 ; Error trap
 ;
 N vError,$ET,$ES S vError=$ZE,$EC="",$ET="Q",$ZE=""
 N RM
 ;
 S ER=$P(vError,",",3)
 S RM=$P(vError,",",4)
 D ^UTLERR
 D ZX^UCGMR(voxMrk) Q 
 ;
vCatch2 ; Error trap
 ;
 N vError,$ET,$ES S vError=$ZE,$EC="",$ET="Q",$ZE=""
 N ETL
 S ETL=$$ETLOC^%ZT
 D ERRLOG($piece(ETL,",",1),1)
 D ZX^UCGMR(voxMrk) Q 
 ;
vCatch1 ; Error trap
 ;
 N vError,$ET,$ES S vError=$ZE,$EC="",$ET="Q",$ZE=""
 N ET N RM
 S ET=$P(vError,",",3)
 S RM=$P(vError,",",4)
 D ^UTLERR
 D ZX^UCGMR(voxMrk) Q 
