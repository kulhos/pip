 ; 
 ; **** Routine compiled from DATA-QWIK Report DBSSPLST ****
 ; 
 ; 02/24/2010 18:38 - pip
 ; 
R00S056 ; DBSSPLST - Stored Procedure Definition
 ; Copyright(c)2010 Sanchez Computer Associates, Inc.  All Rights Reserved - 02/24/2010 18:38 - pip
 ;
  S ER=0
 N OLNTB
 N %READ N RID N RN N %TAB N VFMQ
 N VIN1 S VIN1="ALL"
 N VIN2 S VIN2="ALL"
 N VIN3 S VIN3="ALL"
 N VIN4 S VIN4="ALL"
 N VIN5 S VIN5="ALL"
 N VIN6 S VIN6="ALL"
 N VIN7 S VIN7="ALL"
 N VIN8 S VIN8="ALL"
 ;
 S RID="DBSSPLST"
 S RN="Stored Procedure Definition"
 I $get(IO)="" S IO=$I
 ;
 D INIT^%ZM()
 ;
 S %TAB("IO")=$$IO^SCATAB
 S %TAB("VIN1")="|255||[DBTBLSP]PID|[DBTBLSP]PID:DISTINCT:NOVAL||D EXT^DBSQRY||T|Stored Procedure Name|||||"
 S %TAB("VIN2")="|255||[DBTBLSP]SPTYPE|||D EXT^DBSQRY||T|Stored Procedure Type|||||"
 S %TAB("VIN3")="|255||[DBTBLSP]HVARS|||D EXT^DBSQRY||T|Host Variable List|||||"
 S %TAB("VIN4")="|255||[DBTBLSP]PARS|||D EXT^DBSQRY||T|Parameters|||||"
 S %TAB("VIN5")="|255||[DBTBLSP]LTD|||D EXT^DBSQRY||T|Date Created|||||"
 S %TAB("VIN6")="|255||[DBTBLSP]TIME|||D EXT^DBSQRY||T|Time Created|||||"
 S %TAB("VIN7")="|255||[DBTBLSP]SQLSTMT|||D EXT^DBSQRY||T|SQL Statement|||||"
 S %TAB("VIN8")="|255||[DBTBLSP]HASHKEY|||D EXT^DBSQRY||T|Hash Key|||||"
 ;
 S %READ="IO/REQ,VIN1#0,VIN2#0,VIN3#0,VIN4#0,VIN5#0,VIN6#0,VIN7#0,VIN8#0,"
 ;
 ; Skip device prompt option
 I $get(VRWOPT("NOOPEN")) S %READ="VIN1#0,VIN2#0,VIN3#0,VIN4#0,VIN5#0,VIN6#0,VIN7#0,VIN8#0,"
 ;
 S VFMQ=""
 I %READ'="" D  Q:$get(VFMQ)="Q" 
 .	S OLNTB=30
 .	S %READ="@RN/CEN#1,,"_%READ
 .	D ^UTLREAD
 .	Q 
 ;
 I '$get(vbatchq) D V0
 Q 
 ;
V0 ; External report entry point
 ;
 N vcrt N VD N VFMQ N vh N vI N vlc N VLC N VNEWHDR N VOFFLG N VPN N VR N VRG N vs N VSEQ N VT
 N VWHERE
 N %TIM N CONAM N RID N RN N VL N VLOF N VRF N VSTATS N vCOL N vHDG N vc1 N vc10 N vc11 N vc12 N vc13 N vc14 N vc15 N vc16 N vc17 N vc18 N vc19 N vc2 N vc3 N vc4 N vc5 N vc6 N vc7 N vc8 N vc9 N vovc1 N vrundate N vsysdate
 ;
 N cuvar S cuvar=$$vRCgetRecord0Opt^RecordCUVAR(0,"")
  S cuvar=$G(^CUVAR("BANNER"))
 ;
 S CONAM="PIP Version 0.2"
 S ER=0 S RID="DBSSPLST" S RN="Stored Procedure Definition"
 S VL=""
 ;
 USE 0 I '$get(VRWOPT("NOOPEN")) D  Q:ER 
 .	I '($get(VRWOPT("IOPAR"))="") S IOPAR=VRWOPT("IOPAR")
 .	E  I (($get(IOTYP)="RMS")!($get(IOTYP)="PNTQ")),('($get(IOPAR)["/OCHSET=")),$$VALID^%ZRTNS("UCIOENCD") D
 ..		; Accept warning if ^UCIOENCD does not exist
 ..		;    #ACCEPT Date=07/26/06; Pgm=RussellDS; CR=22121; Group=ACCESS
 ..		N CHRSET S CHRSET=$$^UCIOENCD("Report","DBSSPLST","V0","*")
 ..		I '(CHRSET="") S IOPAR=IOPAR_"/OCHSET="_CHRSET
 ..		Q 
 .	D OPEN^SCAIO
 .	Q 
 S vcrt=(IOTYP="TRM")
 I 'vcrt S IOSL=60 ; Non-interactive
 E  D  ; Interactive
 .	D TERM^%ZUSE(IO,"WIDTH=133")
 .	;   #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 .	WRITE $$CLEARXY^%TRMVT
 .	;   #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 .	WRITE $$SCR132^%TRMVT ; Switch to 132 col mode
 .	Q 
 ;
 D INIT^%ZM()
 ;
 ; Build WHERE clause to use for dynamic query
 D
 .	N SEQ S SEQ=1
 .	N DQQRY N FROM
 .	I $get(VIN1)="" S VIN1="ALL"
 .	I VIN1'="ALL" S DQQRY(1)="[DBTBLSP]PID "_VIN1 S SEQ=SEQ+1
 .	I $get(VIN2)="" S VIN2="ALL"
 .	I VIN2'="ALL" S DQQRY(SEQ)="[DBTBLSP]SPTYPE "_VIN2 S SEQ=SEQ+1
 .	I $get(VIN3)="" S VIN3="ALL"
 .	I VIN3'="ALL" S DQQRY(SEQ)="[DBTBLSP]HVARS "_VIN3 S SEQ=SEQ+1
 .	I $get(VIN4)="" S VIN4="ALL"
 .	I VIN4'="ALL" S DQQRY(SEQ)="[DBTBLSP]PARS "_VIN4 S SEQ=SEQ+1
 .	I $get(VIN5)="" S VIN5="ALL"
 .	I VIN5'="ALL" S DQQRY(SEQ)="[DBTBLSP]LTD "_VIN5 S SEQ=SEQ+1
 .	I $get(VIN6)="" S VIN6="ALL"
 .	I VIN6'="ALL" S DQQRY(SEQ)="[DBTBLSP]TIME "_VIN6 S SEQ=SEQ+1
 .	I $get(VIN7)="" S VIN7="ALL"
 .	I VIN7'="ALL" S DQQRY(SEQ)="[DBTBLSP]SQLSTMT "_VIN7 S SEQ=SEQ+1
 .	I $get(VIN8)="" S VIN8="ALL"
 .	I VIN8'="ALL" S DQQRY(SEQ)="[DBTBLSP]HASHKEY "_VIN8 S SEQ=SEQ+1
 .	S FROM=$$DQJOIN^SQLCONV("DBTBLSP") Q:ER 
 .	S VWHERE=$$WHERE^SQLCONV(.DQQRY,"")
 .	Q 
 ;
 ; Print Report Banner Page
 I $P(cuvar,$C(124),1),'$get(VRWOPT("NOBANNER")),IOTYP'="TRM",'$get(AUXPTR) D
 .	N VBNRINFO
 .	;
 .	S VBNRINFO("PROMPTS",1)="WC2|"_"Stored Procedure Name"_"|VIN1|"_$get(VIN1)
 .	S VBNRINFO("PROMPTS",2)="WC2|"_"Stored Procedure Type"_"|VIN2|"_$get(VIN2)
 .	S VBNRINFO("PROMPTS",3)="WC2|"_"Host Variable List"_"|VIN3|"_$get(VIN3)
 .	S VBNRINFO("PROMPTS",4)="WC2|"_"Parameters"_"|VIN4|"_$get(VIN4)
 .	S VBNRINFO("PROMPTS",5)="WC2|"_"Date Created"_"|VIN5|"_$get(VIN5)
 .	S VBNRINFO("PROMPTS",6)="WC2|"_"Time Created"_"|VIN6|"_$get(VIN6)
 .	S VBNRINFO("PROMPTS",7)="WC2|"_"SQL Statement"_"|VIN7|"_$get(VIN7)
 .	S VBNRINFO("PROMPTS",8)="WC2|"_"Hash Key"_"|VIN8|"_$get(VIN8)
 .	;
 .	D
 ..		N SEQ
 ..		N VALUE N VAR N X
 ..		S X=VWHERE
 ..		S SEQ=""
 ..		F  S SEQ=$order(VBNRINFO("PROMPTS",SEQ)) Q:SEQ=""  D
 ...			S VAR=$piece(VBNRINFO("PROMPTS",SEQ),"|",3)
 ...			S VALUE=$piece(VBNRINFO("PROMPTS",SEQ),"|",4,99)
 ...			S X=$$replace^DBSRWUTL(X,":"_VAR,"'"_VALUE_"'")
 ...			Q 
 ..		S VBNRINFO("WHERE")=X
 ..		Q 
 .	;
 .	S VBNRINFO("DESC")="Stored Procedure Definition"
 .	S VBNRINFO("PGM")="R00S056"
 .	S VBNRINFO("RID")="DBSSPLST"
 .	S VBNRINFO("TABLES")="DBTBLSP"
 .	;
 .	S VBNRINFO("ORDERBY",1)="[SYSDEV,DBTBLSP]PID"
 .	;
 .	D ^DBSRWBNR(IO,.VBNRINFO) ; Print banner
 .	Q 
 ;
 ; Initialize variables
 S (vc1,vc2,vc3,vc4,vc5,vc6,vc7,vc8,vc9,vc10,vc11,vc12,vc13,vc14,vc15,vc16,vc17,vc18,vc19)=""
 S (VFMQ,vlc,VLC,VOFFLG,VPN,VRG)=0
 S VNEWHDR=1
 S VLOF=""
 S %TIM=$$TIM^%ZM
 S vrundate=$$vdat2str($P($H,",",1),"MM/DD/YEAR") S vsysdate=$S(TJD'="":$ZD(TJD,"MM/DD/YEAR"),1:"")
 ;
 D
 .	N I N J N K
 .	F I=0:1:1 D
 ..		S (vh(I),VD(I))=0 S vs(I)=1 ; Group break flags
 ..		S VT(I)=0 ; Group count
 ..		F J=1:1:0 D
 ...			F K=1:1:3 S VT(I,J,K)="" ; Initialize function stats
 ...			Q 
 ..		Q 
 .	Q 
 ;
  N V1 S V1=$J D vDbDe1() ; Report browser data
 S vh(0)=0
 ;
 ; Run report directly
 D VINILAST
 ;
 ;  #ACCEPT DATE=02/24/2010;PGM=Report Writer Generator;CR=20967
 N rwrs,vos1,vos2,sqlcur,exe,sqlcur,vd,vi,vsql,vsub S rwrs=$$vOpen0(.exe,.vsql,"DBTBLSP.PID,DBTBLSP.PGM,DBTBLSP.PARS,DBTBLSP.USER,DBTBLSP.HASHKEY,DBTBLSP.HVARS,DBTBLSP.LTD,DBTBLSP.TIME,DBTBLSP.SPTYPE,DBTBLSP.SQL1,DBTBLSP.SQL2,DBTBLSP.SQL3,DBTBLSP.SQL4,DBTBLSP.SQL5,DBTBLSP.SQL6,DBTBLSP.SQL7,DBTBLSP.SQL8,DBTBLSP.SQL9,DBTBLSP.SQL10","DBTBLSP",VWHERE,"DBTBLSP.PID","","/DQMODE=1",1)
 ;  #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 I $get(ER) USE 0 WRITE $$MSG^%TRMVT($get(RM),"",1) ; Debug Mode
 I '$G(vos1) D VEXIT(1) Q 
 F  Q:'$$vFetch0()  D  Q:VFMQ 
 .	N V N VI
 . S V=rwrs
 .	S VI=""
 .	D VGETDATA(V,"")
 .	D VPRINT Q:VFMQ 
 .	D VSAVLAST
 .	Q 
 D VEXIT(0)
 ;
 Q 
 ;
VINILAST ; Initialize last access key values
 S vovc1=""
 Q 
 ;
VSAVLAST ; Save last access keys values
 S vovc1=vc1
 Q 
 ;
VGETDATA(V,VI) ; 
 S vc1=$piece(V,$char(9),1) ; DBTBLSP.PID
 S vc2=$piece(V,$char(9),2) ; DBTBLSP.PGM
 S vc3=$piece(V,$char(9),3) ; DBTBLSP.PARS
 S vc4=$piece(V,$char(9),4) ; DBTBLSP.USER
 S vc5=$piece(V,$char(9),5) ; DBTBLSP.HASHKEY
 S vc6=$piece(V,$char(9),6) ; DBTBLSP.HVARS
 S vc7=$piece(V,$char(9),7) ; DBTBLSP.LTD
 S vc8=$piece(V,$char(9),8) ; DBTBLSP.TIME
 S vc9=$piece(V,$char(9),9) ; DBTBLSP.SPTYPE
 S vc10=$piece(V,$char(9),10) ; DBTBLSP.SQL1
 S vc11=$piece(V,$char(9),11) ; DBTBLSP.SQL2
 S vc12=$piece(V,$char(9),12) ; DBTBLSP.SQL3
 S vc13=$piece(V,$char(9),13) ; DBTBLSP.SQL4
 S vc14=$piece(V,$char(9),14) ; DBTBLSP.SQL5
 S vc15=$piece(V,$char(9),15) ; DBTBLSP.SQL6
 S vc16=$piece(V,$char(9),16) ; DBTBLSP.SQL7
 S vc17=$piece(V,$char(9),17) ; DBTBLSP.SQL8
 S vc18=$piece(V,$char(9),18) ; DBTBLSP.SQL9
 S vc19=$piece(V,$char(9),19) ; DBTBLSP.SQL10
 Q 
 ;
VBRSAVE(LINE,DATA) ; Save for report browser
 N vTp
 N tmprptbr,vop1,vop2,vop3,vop4,vop5 S tmprptbr="",vop5=0
  S vop4=$J
  S vop3=LINE
  S vop2=0
  S vop1=0
  S $P(tmprptbr,$C(12),1)=DATA
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" S ^TMPRPTBR(vop4,vop3,vop2,vop1)=tmprptbr S vop5=1 TC:vTp  
 Q 
 ;
VEXIT(NOINFO) ; Exit from report
 N I N PN N vs N z
 N VL S VL=""
 S vs(1)=0
 I 'VFMQ D VRSUM
 I 'VFMQ D
 .	; No information available to display
 .	I NOINFO=1 S VL=$$^MSG(4655) D VOM
 .	I vcrt S VL="" F z=VLC+1:1:IOSL D VOM
 .	;
 .	I '($D(VTBLNAM)#2) D
 ..		S vs(2)=0
 ..		Q 
 .	Q 
 ;
 I 'VFMQ,vcrt S PN=-1 D ^DBSRWBR(2)
 I '$get(VRWOPT("NOCLOSE")) D CLOSE^SCAIO
  N V1 S V1=$J D vDbDe2() ; Report browser data
 ;
 Q 
 ;
VPRINT ; Print section
 N vskp
 ;
 I $get(VRWOPT("NODTL")) S vskp(1)=1 ; Skip detail
 D VBREAK
 ;
 I $get(VH0) S vh(0)=0 S VNEWHDR=1 K VH0 ; Page Break
 I 'vh(0) D VHDG0 Q:VFMQ 
 I '$get(vskp(1)) D VDTL1 Q:VFMQ 
 D VSTAT
 Q 
 ;
VBREAK ; 
 Q:'VT(1) 
 S VH0=1 ; Page break
 N vb1
 S (vb1)=0
 Q 
 ;
VSTAT ; Data field statistics
 ;
 S VT(1)=VT(1)+1
 Q 
 ;
VDTL1 ; Detail
 ;
 I VLC+13>IOSL D VHDG0 Q:VFMQ 
 ;
 S VL="                   "_"---------------------------------------------------------------------------------------------------------------"
 D VOM
 S VL=$E(vc1,1,20)
 S VL=VL_$J("",(21-$L(VL)))_$E(vc2,1,8)
 S VL=VL_$J("",(30-$L(VL)))_$E(vc3,1,25)
 S VL=VL_$J("",(56-$L(VL)))_$E(vc4,1,13)
 S VL=VL_$J("",(71-$L(VL)))_$E(vc5,1,18)
 S VL=VL_$J("",(90-$L(VL)))_$E(vc6,1,20)
 S VL=VL_$J("",(111-$L(VL)))_$J($S(vc7'="":$ZD(vc7,"MM/DD/YEAR"),1:""),10)
 S VL=VL_$J("",(122-$L(VL)))_$J($$TIM^%ZM(vc8),8)
 D VOM
 D VOM
 S VL="                   "_$E(vc9,1,6)
 S VL=VL_$J("",(26-$L(VL)))_$E(vc10,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc11,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc12,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc13,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc14,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc15,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc16,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc17,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc18,1,255)
 I '($translate(VL," ")="") D VOM
 S VL="                          "_$E(vc19,1,255)
 I '($translate(VL," ")="") D VOM
 Q 
 ;
VHDG0 ; Page Header
 N PN N V N VO
 I $get(VRWOPT("NOHDR")) Q  ; Skip page header
 S vh(0)=1 S VRG=0
 I VL'="" D VOM
 I vcrt,VPN>0 D  Q:VFMQ!'VNEWHDR 
 .	N PN N X
 .	S VL=""
 .	F X=VLC+1:1:IOSL D VOM
 .	S PN=VPN
 .	D ^DBSRWBR(2)
 .	S VLC=0
 .	Q:VFMQ 
 .	;   #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 .	I VNEWHDR WRITE $$CLEARXY^%TRMVT
 .	E  S VLC=VLC+6 S VPN=VPN+1
 .	Q 
 ;
 S ER=0 S VPN=VPN+1 S VLC=0
 ;
 S VL=$E($get(CONAM),1,45)
 S VL=VL_$J("",(100-$L(VL)))_"Run Date:"
 S VL=VL_$J("",(110-$L(VL)))_$E(vrundate,1,10)
 S VL=VL_$J("",(123-$L(VL)))_$E(%TIM,1,8)
 D VOM
 S VL=RN_"  ("_RID_")"
 S VL=VL_$J("",(102-$L(VL)))_"System:"
 S VL=VL_$J("",(110-$L(VL)))_$E(vsysdate,1,10)
 S VL=VL_$J("",(122-$L(VL)))_"Page:"
 S VL=VL_$J("",(128-$L(VL)))_$J(VPN,3)
 D VOM
 D VOM
 S VL="                                                                                                                "_"Date      Time"
 D VOM
 S VL=" "_"Procedure ID        Routine  Parameters                User           Hash Key           Host Variables        Created/Modified"
 D VOM
 S VL="------------------------------------------------------------------------------------------------------------------------------------"
 D VOM
 ;
 S VNEWHDR=0
 I vcrt S PN=VPN D ^DBSRWBR(2,1) ; Lock report page heading
 ;
 Q 
 ;
VRSUM ; Report Summary
 N I
 N V N VL
 ;
 S VL=""
 I 'vh(0) D VHDG0 Q:VFMQ 
 I VLC+4>IOSL D VHDG0 Q:VFMQ 
 ;
 D VOM
 S VL="                          "_"---------------------------------"
 D VOM
 S VL="                          "_"Total Stored Procedures: "
 S V=(VT(0)+VT(1)) ; @CNT(0,N,8)
 S VL=VL_""_$J(V,8)
 D VOM
 S VL="                          "_"---------------------------------"
 D VOM
 Q 
 ;
VOM ; Output print line
 ;
 USE IO
 ;
 ; Advance to a new page
 I 'VLC,'vcrt D  ; Non-CRT device (form feed)
 .	;   #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 .	I '$get(AUXPTR) WRITE $char(12),!
 .	;   #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 .	E  WRITE $$PRNTFF^%TRMVT,!
 .	S $Y=1
 .	Q 
 ;
 ;  #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 I vcrt<2 WRITE VL,! ; Output line buffer
 I vcrt S vlc=vlc+1 D VBRSAVE(vlc,VL) ; Save in BROWSER buffer
 S VLC=VLC+1 S VL="" ; Reset line buffer
 Q 
 ;
 ; Pre/post-processors
 ; ----------------
 ;  #OPTION ResultClass 1
vdat2str(vo,mask) ; Date.toString
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 I (vo="") Q ""
 I (mask="") S mask="MM/DD/YEAR"
 N cc N lday N lmon
 I mask="DL"!(mask="DS") D  ; Long or short weekday
 .	;    #ACCEPT PGM=FSCW;DATE=2007-03-30;CR=27800;GROUP=GLOBAL
 .	S cc=$get(^DBCTL("SYS","DVFM")) ; Country code
 .	I (cc="") S cc="US"
 .	;    #ACCEPT PGM=FSCW;DATE=2007-03-30;CR=27800;GROUP=GLOBAL
 .	S lday=$get(^DBCTL("SYS","*DVFM",cc,"D",mask))
 .	S mask="DAY" ; Day of the week
 .	Q 
 I mask="ML"!(mask="MS") D  ; Long or short month
 .	;    #ACCEPT PGM=FSCW;DATE=2007-03-30;CR=27800;GROUP=GLOBAL
 .	S cc=$get(^DBCTL("SYS","DVFM")) ; Country code
 .	I (cc="") S cc="US"
 .	;    #ACCEPT PGM=FSCW;DATE=2007-03-30;CR=27800;GROUP=GLOBAL
 .	S lmon=$get(^DBCTL("SYS","*DVFM",cc,"D",mask))
 .	S mask="MON" ; Month of the year
 .	Q 
 ;  #ACCEPT PGM=FSCW;DATE=2007-03-30;CR=27800;GROUP=BYPASS
 ;*** Start of code by-passed by compiler
 set cc=$ZD(vo,mask,$G(lmon),$G(lday))
 ;*** End of code by-passed by compiler ***
 Q cc
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe1() ; DELETE FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3 N v4
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5,vos6 S vRs=$$vOpen1()
 F  Q:'$$vFetch1()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3) S v4=$P(vRs,$C(9),4)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^TMPRPTBR(v1,v2,v3,v4)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe2() ; DELETE FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3 N v4
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5,vos6 S vRs=$$vOpen2()
 F  Q:'$$vFetch2()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3) S v4=$P(vRs,$C(9),4)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^TMPRPTBR(v1,v2,v3,v4)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ;
vOpen0(exe,vsql,vSelect,vFrom,vWhere,vOrderby,vGroupby,vParlist,vOff) ; Dynamic MDB ResultSet
 ;
 set sqlcur="V0.rwrs"
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
 S vos1=vsql
 Q ""
 ;
vFetch0() ; MDB dynamic FETCH
 ;
 ; type public String exe(),sqlcur,vd,vi,vsql()
 ;
 I vsql=0 S rwrs="" Q 0
 S vsql=$$^SQLF(.exe,.vd,.vi,.sqlcur)
 S rwrs=vd
 S vos1=vsql
 S vos2=$G(vi)
 Q vsql
 ;
vOpen1() ; JOBNO,LINENO,PAGENO,SEQ FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1)
 S vos4=""
vL1a4 S vos4=$O(^TMPRPTBR(vos3,vos4),1) I vos4="" G vL1a0
 S vos5=""
vL1a6 S vos5=$O(^TMPRPTBR(vos3,vos4,vos5),1) I vos5="" G vL1a4
 S vos6=""
vL1a8 S vos6=$O(^TMPRPTBR(vos3,vos4,vos5,vos6),1) I vos6="" G vL1a6
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a8
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$S(vos5=vos2:"",1:vos5)_$C(9)_$S(vos6=vos2:"",1:vos6)
 ;
 Q 1
 ;
vOpen2() ; JOBNO,LINENO,PAGENO,SEQ FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;
 S vos1=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos1=0 Q
vL2a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1)
 S vos4=""
vL2a4 S vos4=$O(^TMPRPTBR(vos3,vos4),1) I vos4="" G vL2a0
 S vos5=""
vL2a6 S vos5=$O(^TMPRPTBR(vos3,vos4,vos5),1) I vos5="" G vL2a4
 S vos6=""
vL2a8 S vos6=$O(^TMPRPTBR(vos3,vos4,vos5,vos6),1) I vos6="" G vL2a6
 Q
 ;
vFetch2() ;
 ;
 ;
 I vos1=1 D vL2a8
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$S(vos5=vos2:"",1:vos5)_$C(9)_$S(vos6=vos2:"",1:vos6)
 ;
 Q 1
