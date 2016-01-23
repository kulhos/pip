 ; 
 ; **** Routine compiled from DATA-QWIK Report DBSQRYLST ****
 ; 
 ; 02/24/2010 18:38 - pip
 ; 
R00S051 ; DBSQRYLST - DATA-QWIK Query Library Listing
 ; Copyright(c)2010 Sanchez Computer Associates, Inc.  All Rights Reserved - 02/24/2010 18:38 - pip
 ;
  S ER=0
 N OLNTB
 N %READ N RID N RN N %TAB N VFMQ
 ;
 S RID="DBSQRYLST"
 S RN="DATA-QWIK Query Library Listing"
 I $get(IO)="" S IO=$I
 ;
 D INIT^%ZM()
 ;
 S %TAB("IO")=$$IO^SCATAB
 ;
 S %READ="IO/REQ,"
 ;
 ; Skip device prompt option
 I $get(VRWOPT("NOOPEN")) S %READ=""
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
 N %TIM N CONAM N QUERY N RID N RN N VL N VLOF N VRF N VSTATS N vCOL N vHDG N vc1 N vc2 N vc3 N vc4 N vc5 N vc6 N vc7 N vc8 N vc9 N vovc1 N vovc2 N vovc3 N vovc4 N vovc5 N vovc6 N vovc7 N vovc8 N vovc9 N vrundate N vsysdate
 ;
 S CONAM="PIP Version 0.2"
 S ER=0 S RID="DBSQRYLST" S RN="DATA-QWIK Query Library Listing"
 S VL=""
 ;
 USE 0 I '$get(VRWOPT("NOOPEN")) D  Q:ER 
 .	I '($get(VRWOPT("IOPAR"))="") S IOPAR=VRWOPT("IOPAR")
 .	E  I (($get(IOTYP)="RMS")!($get(IOTYP)="PNTQ")),('($get(IOPAR)["/OCHSET=")),$$VALID^%ZRTNS("UCIOENCD") D
 ..		; Accept warning if ^UCIOENCD does not exist
 ..		;    #ACCEPT Date=07/26/06; Pgm=RussellDS; CR=22121; Group=ACCESS
 ..		N CHRSET S CHRSET=$$^UCIOENCD("Report","DBSQRYLST","V0","*")
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
 S vCOL="[DBTBL4D]LINE#30#80"
 ;
 ; Initialize variables
 S (vc1,vc2,vc3,vc4,vc5,vc6,vc7,vc8,vc9)=""
 S (VFMQ,vlc,VLC,VOFFLG,VPN,VRG)=0
 S VNEWHDR=1
 S VLOF=""
 S %TIM=$$TIM^%ZM
 S vrundate=$$vdat2str($P($H,",",1),"MM/DD/YEAR") S vsysdate=$S(TJD'="":$ZD(TJD,"MM/DD/YEAR"),1:"")
 ;
 D
 .	N I N J N K
 .	F I=0:1:4 D
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
 N rwrs,vos1,vos2,vos3,vos4,vos5,vos6,vos7,vos8,vos9,vos11,vos10,vos12,vos13,vOid  N V2 S V2=$J S rwrs=$$vOpen1()
 ;  #ACCEPT Date=10/13/2008;Pgm=RussellDS;CR=35741;Group=READ
 I $get(ER) USE 0 WRITE $$MSG^%TRMVT($get(RM),"",1) ; Debug Mode
 I '$G(vos1) D VEXIT(1) Q 
 F  Q:'$$vFetch1()  D  Q:VFMQ 
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
VINILAST ; Initialize old values
  S vovc1=""  S vovc2=""  S vovc3=""  S vovc4=""  S vovc5=""  S vovc6=""  S vovc7=""  S vovc8=""  S vovc9=""
 Q 
 ;
VSAVLAST ; Save old values
 S vovc1=vc1 S vovc2=vc2 S vovc3=vc3 S vovc4=vc4 S vovc5=vc5 S vovc6=vc6 S vovc7=vc7 S vovc8=vc8 S vovc9=vc9
 Q 
 ;
VGETDATA(V,VI) ; 
 S vc1=$piece(V,$char(9),1) ; DBTBL4D.LIBS
 S vc2=$piece(V,$char(9),2) ; DBTBL4D.QID
 S vc3=$piece(V,$char(9),3) ; DBTBL4D.SEQ
 S vc4=$piece(V,$char(9),4) ; DBTBL4.QID
 S vc5=$piece(V,$char(9),5) ; DBTBL4.DESC
 S vc6=$piece(V,$char(9),6) ; DBTBL4.UID
 S vc7=$piece(V,$char(9),7) ; DBTBL4.DATE
 S vc8=$piece(V,$char(9),8) ; DBTBL4.PFID
 S vc9=$piece(V,$char(9),9) ; DBTBL4D.LINE
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
 S vs(1)=0 S vs(2)=0 S vs(3)=0 S vs(4)=0
 I 'VFMQ D VSUM
 I 'vh(0) D VHDG0
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
 I $get(VRWOPT("NODTL")) S vskp(1)=1 S vskp(2)=1 S vskp(3)=1 S vskp(4)=1 ; Skip detail
 D VBREAK
 D VSUM Q:VFMQ 
 ;
 I $get(VH0) S vh(0)=0 S VNEWHDR=1 K VH0 ; Page Break
 I 'vh(0) D VHDG0 Q:VFMQ 
 I '$get(vskp(3)) D VDTL3 Q:VFMQ 
 I '$get(vskp(4)) D VDTL4 Q:VFMQ 
 D VSTAT
 Q 
 ;
VBREAK ; 
 Q:'VT(4) 
 N vb1 N vb2 N vb3 N vb4
 S (vb1,vb2,vb3,vb4)=0
 I 0!(vovc1'=vc1) S vs(3)=0 S vh(3)=0 S VD(1)=0 S vb2=1 S vb3=1 S vb4=1
 I vb3!(vovc2'=vc2) S vs(4)=0 S vh(4)=0 S VD(3)=0 S vb4=1
 Q 
 ;
VSUM ; Report Group Summary
 I 'vs(4) S vs(4)=1 D VSUM4 Q:VFMQ  D stat^DBSRWUTL(4)
 I 'vs(3) S vs(3)=1 D stat^DBSRWUTL(3)
 I 'vs(2) S vs(2)=1 D stat^DBSRWUTL(2)
 Q 
 ;
VSTAT ; Data field statistics
 ;
 S VT(4)=VT(4)+1
 Q 
 ;
VDTL3 ; Detail
 ;
 Q:VD(3)  S VD(3)=1 ; Print flag
 I VLC+3>IOSL D VHDG0 Q:VFMQ 
 ;
 S VL=" "_"Name: "
 S VL=VL_""_$E(vc4,1,8)
 S VL=VL_$J("",(20-$L(VL)))_$E(vc5,1,40)
 S VL=VL_$J("",(85-$L(VL)))_"User ID: "
 S VL=VL_$J("",(94-$L(VL)))_$E(vc6,1,16)
 S VL=VL_$J("",(112-$L(VL)))_"Date: "
 S VL=VL_$J("",(118-$L(VL)))_$J($S(vc7'="":$ZD(vc7,"MM/DD/YEAR"),1:""),10)
 D VOM
 D VOM
 S VL="                    "_"File(s): "
 S VL=VL_""_$E(vc8,1,60)
 D VOM
 Q 
 ;
VDTL4 ; Detail
 ;
 I VLC+1>IOSL D VHDG0 Q:VFMQ 
 ;
 D VP1 Q:VFMQ!$get(verror)  S V=$E(QUERY,1,6) S VL="                      "_V
 S VL=VL_$J("",(29-$L(VL)))_$E(vc9,1,80)
 D VOM
 Q 
 ;
VSUM4 ; Summary
 ;
 N vc1 N vc2 N vc3 N vc4 N vc5 N vc6 N vc7 N vc8 N vc9
 I 'VT(4) Q 
 S vc1=vovc1
 S vc2=vovc2
 S vc3=vovc3
 S vc4=vovc4
 S vc5=vovc5
 S vc6=vovc6
 S vc7=vovc7
 S vc8=vovc8
 S vc9=vovc9
 N VLSAV S VLSAV=""
 I VLC+0>IOSL D VHDG0 Q:VFMQ 
 ;
 S VL="------------------------------------------------------------------------------------------------------------------------------------"
 D VOM
 S VL=$get(VLSAV)
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
 .	E  S VLC=VLC+3 S VPN=VPN+1
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
 S VL=VL_$J("",(128-$L(VL)))_$E($J(VPN,3),1,3)
 D VOM
 S VL="------------------------------------------------------------------------------------------------------------------------------------"
 D VOM
 ;
 S VNEWHDR=0
 I vcrt S PN=VPN D ^DBSRWBR(2,1) ; Lock report page heading
 ;
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
 ;
VP1 ; Column pre-processor - Variable: QUERY
 ;
 I (vc3=1) S QUERY="Query:"
 E  S QUERY=""
 ;
 Q 
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
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe2() ; DELETE FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 N v1 N v2 N v3 N v4
 TS (vobj):transactionid="CS"
 N vRs,vos1,vos2,vos3,vos4,vos5,vos6 S vRs=$$vOpen3()
 F  Q:'$$vFetch3()  D
 . S v1=$P(vRs,$C(9),1) S v2=$P(vRs,$C(9),2) S v3=$P(vRs,$C(9),3) S v4=$P(vRs,$C(9),4)
 .	;     #ACCEPT CR=18163;DATE=2006-01-09;PGM=FSCW;GROUP=BYPASS
 .	;*** Start of code by-passed by compiler
 .	ZWI ^TMPRPTBR(v1,v2,v3,v4)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 Q 
 ;
vOpen1() ; DBTBL4D.LIBS,DBTBL4D.QID,DBTBL4D.SEQ,DBTBL4.QID,DBTBL4.DESC,DBTBL4.UID,DBTBL4.DATE,DBTBL4.PFID,DBTBL4D.LINE FROM DBTBL4D,DBTBL4 WHERE DBTBL4D.QID IN (SELECT ELEMENT FROM TMPDQ WHERE PID=:V2) ORDER BY DBTBL4D.LIBS,DBTBL4D.QID,DBTBL4D.SEQ
 ;
 ;
 S vOid=$G(^DBTMP($J))-1,^($J)=vOid K ^DBTMP($J,vOid)
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$$BYTECHAR^SQLUTL(254)
 S vos4=$G(V2)
 S vos5=""
vL1a6 S vos5=$O(^TEMP(vos4,vos5),1) I vos5="" G vL1a10
 S vd=$S(vos5=vos3:"",1:vos5)
 I vd'="" S ^DBTMP($J,vOid,1,vd)=""
 G vL1a6
vL1a10 S vos6=""
vL1a11 S vos6=$O(^DBTBL(vos6),1) I vos6="" G vL1a0
 S vos7=""
vL1a13 S vos7=$O(^DBTMP($J,vOid,1,vos7),1) I vos7="" G vL1a11
 S vos8=vos6,vos9=vos7
 I '($D(^DBTBL(vos6,4,vos7))) S vos8=vos2,vos9=vos2
 I vos9=vos2 S vos10="",vos11=""
 E  S vos10=$G(^DBTBL(vos8,4,vos9,0))
 E  S vos11=$G(^DBTBL(vos8,4,vos9))
 S vos12=0
vL1a20 S vos12=$O(^DBTBL(vos6,4,vos7,vos12),1) I vos12="" G vL1a13
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a20
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rwrs="" K ^DBTMP($J,vOid) Q 0
 ;
 S vos13=$G(^DBTBL(vos6,4,vos7,vos12))
 I vos9=vos2 S vos10="",vos11=""
 E  S vos10=$G(^DBTBL(vos8,4,vos9,0))
 E  S vos11=$G(^DBTBL(vos8,4,vos9))
 S rwrs=$S(vos6=vos2:"",1:vos6)_$C(9)_vos7_$C(9)_$S(vos12=vos2:"",1:vos12)_$C(9)_$S(vos9=vos2:"",1:vos9)_$C(9)_$P(vos11,"|",1)_$C(9)_$P(vos10,"|",15)_$C(9)_$P(vos10,"|",3)_$C(9)_$P(vos10,"|",1)_$C(9)_$P(vos13,"|",1)
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
 ;
vOpen3() ; JOBNO,LINENO,PAGENO,SEQ FROM TMPRPTBR WHERE JOBNO=:V1
 ;
 ;
 S vos1=2
 D vL3a1
 Q ""
 ;
vL3a0 S vos1=0 Q
vL3a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1)
 S vos4=""
vL3a4 S vos4=$O(^TMPRPTBR(vos3,vos4),1) I vos4="" G vL3a0
 S vos5=""
vL3a6 S vos5=$O(^TMPRPTBR(vos3,vos4,vos5),1) I vos5="" G vL3a4
 S vos6=""
vL3a8 S vos6=$O(^TMPRPTBR(vos3,vos4,vos5,vos6),1) I vos6="" G vL3a6
 Q
 ;
vFetch3() ;
 ;
 ;
 I vos1=1 D vL3a8
 I vos1=2 S vos1=1
 ;
 I vos1=0 S vRs="" Q 0
 ;
 S vRs=vos3_$C(9)_$S(vos4=vos2:"",1:vos4)_$C(9)_$S(vos5=vos2:"",1:vos5)_$C(9)_$S(vos6=vos2:"",1:vos6)
 ;
 Q 1
