 ; 
 ; **** Routine compiled from DATA-QWIK Procedure FORMDQ2 ****
 ; 
 ; 01/27/2016 00:27 - kulhan
 ; 
 ;DO NOT MODIFY  Forms system (OOE) DQ screen driver|FORMDQ2|||||||1
FORMDQ2(SID) ; Screen ID
 ;
 N EDITED N fileflag
 N %TO N gt N THTMAR N TIMEOUT N TOPMAR N XLIMIT N YLIMIT N ZB
 N %LIBS N DBTBL N H N LIBS N MAINPGM N PAGE N RCP N SCP N TAB
 N VA N VIDBF N VIDHL N VIFOF N video N VIDRV N X N zbtxt
 ;
 Q:($get(SID)="") 
 ;
 L 
 ;
 ; Define form system specific commands
 ;
 D INIT^%ZM()
 D ^FORMINIT
 D ^FORMDQ2B(2)
 ;
 S MAINPGM=$T(+0)
 ;
 S PAGE(1)="FORMVAR1|6;35"
 S PAGE(2)="FORMVAR2|6;35"
 ;
 S (%LIBS,LIBS)="SYSDEV"
 ;
 S EDITED=0
 S %TO=$$TO^FORMVAR
 ;
 S H=$$HEADER^FORMINIT(2)
 ;
 ; Avoid warning on lock
 S DBTBL(2,SID)=""
 ; Screen in use ?
 L +DBTBL(2,SID):5
 E  WRITE $$MSG^FORM("Screen in use",1) Q 
 ;
 ; Set up OOE internal structure
 D LOAD(.SID) Q:(SID="") 
 ;
 S X=$$HOME^%TRNLNM("FORM_SCR.SCRIPT")
 I $$FILE^%ZOPEN(X,"READ") D SCRIPT^FORMCMD(X)
 ;
 ;Define OOE window  ( 99 rows by 132 columns )
 D ^FORM(99,132)
 ;
 L -DBTBL(2,SID)
 ;
 Q 
 ;
LOAD(SID) ; Screen ID
 N vpc
 ;
 N quit
 ;
 S quit=0
 ; File the current screen
 I (+$order(M(""))'=+0) D  Q:quit 
 .	;
 .	D QUIT^FORMINIT()
 .	;
 .	I (+ZB'=+0) S quit=1
 .	E  S ZB=13
 .	Q 
 ;
 K A,D,M,P
 ;
 I (SID="") S SID=$$GETSID Q:(SID="") 
 ;
 I ($D(^DBTBL("SYSDEV",2,SID))) D SAVECOPY(SID)
 ;
 S SID="z"_SID
 ;
 N dbtbl2,vop1,vop2,vop3,vop4 S vop1="SYSDEV",vop2=SID,dbtbl2=$$vRCgetRecord1Opt^RecordDBTBL2("SYSDEV",SID,0,"")
  S vop4=$G(^DBTBL(vop1,2,vop2,-5))
  S vop3=$G(^DBTBL(vop1,2,vop2,0))
 ;
 S ST8=$P(vop4,$C(124),1)_"|"_$P(vop4,$C(124),2)_"|"_$P(vop4,$C(124),3)
 S ST8=ST8_"|"_$P(vop4,$C(124),4)_"|"_$P(vop4,$C(124),5)_"|||"_$P(vop4,$C(124),8)
 S ST8=ST8_"|"_$P(vop4,$C(124),9)_"|"_$P(vop4,$C(124),10)_"|"_$P(vop4,$C(124),11)
 S ST8=ST8_"|"_$P(vop4,$C(124),12)_"|"_$P(vop4,$C(124),13)
 ;
 I ($translate(ST8,"|","")="") S ST8="||2||||||32|1;0;2||1"
 E  S $piece(ST8,"|",12)=0 ; Turn off status option
 ;
 ; Restore states display ----------
 D REST8^FORMINIT
 ;
 ; ***** Load form ****************************************************
 S FILES=$P(vop3,$C(124),1)
 S OOEFLG=$P(vop3,$C(124),17)
 S dl=$ZCHAR(0)
 ;
 N ds,vos1,vos2,vos3,vos4 S ds=$$vOpen1()
 ;
 F  Q:'$$vFetch1()  D
 .	;
 .	N L N ox N px N py N ZZVAR
 .	N H N X
 .	;
 . N dbtbl2d,vop5 S vop5=$P(ds,$C(9),3),dbtbl2d=$$vRCgetRecord1Opt^RecordDBTBL2D($P(ds,$C(9),1),$P(ds,$C(9),2),vop5,1,"")
 .	;
 .	; ***** Build the screen array ***************************************
 .	S P(1)=$P(dbtbl2d,$C(124),1)
 .	S P(2)=$P(dbtbl2d,$C(124),2)
 .	S P(3)=$P(dbtbl2d,$C(124),3)
 .	S P(4)=""
 .	S P(5)=$P(dbtbl2d,$C(124),5)
 .	S P(6)=$P(dbtbl2d,$C(124),6)
 .	S P(7)=$P(dbtbl2d,$C(124),7)
 .	S P(8)=$P(dbtbl2d,$C(124),8)
 .	S P(9)=$P(dbtbl2d,$C(124),9)
 .	S P(10)=$P(dbtbl2d,$C(124),10)
 .	S P(11)=$P(dbtbl2d,$C(124),11)
 .	S P(12)=$P(dbtbl2d,$C(124),12)
 .	S P(13)=$P(dbtbl2d,$C(124),13)
 .	S P(14)=$P(dbtbl2d,$C(124),14)
 .	S P(15)=$P(dbtbl2d,$C(124),15)
 .	S P(16)=""
 .	S P(17)=""
 .	S P(18)=$P(dbtbl2d,$C(124),18)
 .	S P(19)=""
 .	S P(20)=""
 .	S P(21)=$P(dbtbl2d,$C(124),21)
 .	S P(22)=$P(dbtbl2d,$C(124),22)
 .	;
 .	I ($E(P(5),1)="@") S $piece(DQP,dl,2)=P(5) ; Orig Name
 .	;
 .	I (P(1)["*") S PROTECT=1 ; Protect flag
 .	E  S PROTECT=0
 .	;
 .	; Compare file definition attributes (integrity check)
 .	D
 ..		;
 ..		N isKey N modified
 ..		N COLUMN N TABLE
 ..		;
 ..		Q:P(5)'?1"["1E.E1"]"1E.E 
 ..		;
 ..		S TABLE=$piece($piece(P(5),"]",1),",",2) Q:(TABLE="") 
 ..		S COLUMN=$piece(P(5),"]",2) Q:(COLUMN="") 
 ..		;
 ..		N tblrec S tblrec=$$getSchTbl^UCXDD(TABLE)
 ..		N colrec S colrec=$$getSchCln^UCXDD(TABLE,COLUMN)
 ..		;
 ..		I ($P(colrec,"|",3)["*") S isKey=1
 ..		E  S isKey=0
 ..		;
 ..		S modified=0
 ..		S P(6)=$$AttrMod(P(6),$P(colrec,"|",21),.isKey,.modified)
 ..		S P(7)=$$AttrMod(P(7),$P(colrec,"|",22),.isKey,.modified)
 ..		S P(10)=$$AttrMod(P(10),$P(colrec,"|",6),.isKey,.modified)
 ..		S P(13)=$$AttrMod(P(13),$P(colrec,"|",27),.isKey,.modified)
 ..		S P(14)=$$AttrMod(P(14),$P(colrec,"|",28),.isKey,.modified)
 ..		S P(15)=$$AttrMod(P(15),$P(colrec,"|",8),.isKey,.modified)
 ..		I ($P(colrec,"|",14)="") S P(21)=$$AttrMod(P(21),$P(tblrec,"|",10),.isKey,.modified)
 ..		; We don't care about position for PSL screens
 ..		I '$P(vop3,$C(124),22) S P(22)=$$AttrMod(P(22),$P(colrec,"|",4),.isKey,.modified)
 ..		;
 ..		I modified WRITE $$MSG^FORM("["_$piece(P(5),",",2)_" Field attributes modified",1)
 ..		Q 
 .	;
 .	; Convert old <<var,pmt,...>> syntax
 .	I ($E(P(11),1,2)="<<"),($E(P(11),$L(P(11))-2+1,1048575)=">>"),'(P(11)["""") D
 ..		;
 ..		N X
 ..		;
 ..		Q:((P(11)["$$")&(P(11)["UTLDOC")) 
 ..		;
 ..		I (P(3)>0) D  Q  ; Already converted
 ...			;
 ...			S P(5)=P(11)
 ...			S P(11)=""
 ...			Q 
 ..		;
 ..		I ((P(11)?.E1"$"1A.E)!(P(11)["$$")) D  Q 
 ...			;
 ...			S P(5)=P(11)
 ...			S P(11)=""
 ...			Q 
 ..		;
 ..		S ZZVAR=1
 ..		;
 ..		S X=$E(P(11),1,$L(P(11))-2)
 ..		S P(3)=+$piece(X,",",4) ; Size
 ..		S P(10)=$piece(X,",",3) ; Format
 ..		I (P(10)="") S P(10)="T"
 ..		;
 ..		S P(2)=P(10)
 ..		S P(5)=$piece(X,",",1)_">>"
 ..		S P(6)=$piece(X,",",9,99)
 ..		S P(7)=""
 ..		S P(11)=$piece(X,",",2)
 ..		S P(13)=$piece(X,",",5)
 ..		S P(14)=$piece(X,",",6)
 ..		S P(15)=$piece(X,",",7)
 ..		S P(21)=""
 ..		S P(22)=$piece(X,",",8)
 ..		I P(22) S P(21)=124
 ..		Q 
 .	;
 .	S py=P(1)\1000
 .	S px=P(1)#1000
 .	S L=$L(P(11))
 .	S ox=px+L
 . S vpc=(py<1) Q:vpc 
 .	;
 .	S X=$piece(P(1),"#",2,999)
 .	I 'OOEFLG D  ; Convert old.X video attributes to new
 ..		;
 ..		N A
 ..		;
 ..		S A=0
 ..		;
 ..		I $F(X,2) S A=A+1 ; Reverse video
 ..		I $F(X,1) S A=A+2 ; Highlight
 ..		I $F(X,3) S A=A+4 ; Underscore
 ..		I $F(X,4) S A=A+8 ; Blinking
 ..		;
 ..		S X=A
 ..		Q 
 .	;
 .	S H=$$HEADER^FORMINIT($piece(X,",",1),$piece(X,",",2),$piece(X,",",3),$piece(X,",",4),$piece(X,",",5),$piece(X,",",6))
 .	;
 .	I '($D(VA(H))#2) D VIDSP^FORMINIT(H) ; Add video option
 .	;
 .	I ($E(P(11),L-1,L)=": ") S P(11)=$E(P(11),1,L-1)
 .	;
 .	I ($E(P(5),1)="@"),(P(5)["@ooe") S P(5)=""
 .	;
 . I (P(11)]]"") S M(py,px)=H_P(11) S vpc=(P(5)="") Q:vpc 
 .	;
 .	I ($E(P(5),1)="@") D
 ..		;
 ..		S ox=px
 ..		S $piece(DQP,dl,1)=P(5)
 ..		Q 
 .	E  D
 ..		;
 ..		N m
 ..		;
 ..		; Strip library
 ..		I ($E(P(5),1,8)="[SYSDEV,") S P(5)="["_$E(P(5),9,1048575)
 ..		;
 ..		S $piece(DQP,dl,1)=P(5) ; Data item
 ..		;
 ..		I (P(18)="") S P(18)=$translate(P(10),"FU","TT")
 ..		;
 ..		S $piece(DQP,dl,3)=+P(3) ; Field length
 ..		S $piece(DQP,dl,4)=P(15) ; Decimal precision
 ..		S $piece(DQP,dl,5)=P(10) ; Data type
 ..		S $piece(DQP,dl,6)=P(6) ; Table lookup
 ..		S $piece(DQP,dl,7)=P(12) ; Required
 ..		S $piece(DQP,dl,8)=PROTECT
 ..		S $piece(DQP,dl,9)=P(7) ; Pattern match
 ..		S $piece(DQP,dl,10)=P(13) ; Minimum value
 ..		S $piece(DQP,dl,11)=P(14) ; Maximum value
 ..		S $piece(DQP,dl,12)=P(4) ; Default value
 ..		S $piece(DQP,dl,15)=P(17) ; Computed operation (not used)
 ..		S $piece(DQP,dl,16)=P(21) ; Delimeter
 ..		S $piece(DQP,dl,17)=P(22) ; Position
 ..		S $piece(DQP,dl,18)=P(18) ; Print edit
 ..		;
 ..		D OBJ(DQP) ; Return m
 ..		;
 ..		I ((ox'=px)!$get(ZZVAR)!'OOEFLG) D
 ...			;
 ...			S H=$$HEADER^FORMINIT(2) ;  Highlight
 ...			K ZZVAR
 ...			Q 
 ..		;
 ..		I '($D(VA(H))#2) D VIDSP^FORMINIT(H)
 ..		;
 ..		S M(py,ox)=H_m
 ..		Q 
 .	;
 .	; Load pre processors & Post processors if necessary
 .	N ds2,vos5,vos6,vos7,vos8,vos9  N V1 S V1=vop5 S ds2=$$vOpen2()
 .	;
 . I ''$G(vos5) D
 ..		;
 ..		N PO N POSEQ N PR N PRSEQ
 ..		;
 ..		S PR=$order(PP(""),-1)+1 ; Pre-processor key
 ..		S PO=PR+1 ; Post-processor key
 ..		S (POSEQ,PRSEQ)=1
 ..		;
 ..		F  Q:'$$vFetch2()  D
 ...			;
 ...   N dbtbl2pp,vop6 S vop6=$P(ds2,$C(9),4),dbtbl2pp=$$vRCgetRecord1Opt^RecordDBTBL2PP($P(ds2,$C(9),1),$P(ds2,$C(9),2),$P(ds2,$C(9),3),vop6,1,"")
 ...			;
 ...			; Pre-processor
 ...			I (vop6'>20) D
 ....				;
 ....				S PP(PR,PRSEQ)=$P(dbtbl2pp,$C(12),1)
 ....				S PRSEQ=PRSEQ+1
 ....				Q 
 ...			; Post-processor
 ...			E  D
 ....				;
 ....				S PP(PO,POSEQ)=$P(dbtbl2pp,$C(12),1)
 ....				S POSEQ=POSEQ+1
 ....				Q 
 ...   Q 
 ..		;
 ..		I (PRSEQ=1) S PR=""
 ..		I (POSEQ=1) S PO=""
 ..		;
 ..		S $piece(DQP,dl,13)=PR
 ..		S $piece(DQP,dl,14)=PO
 ..		Q 
 .	;
 .	S D(py,ox)=DQP
 .	S DQP=""
 . Q 
 ;
 S EDITED=0
 ;
 ; ***** Display ******************************************************
 D PUTRGN^FORMFUN()
 ;
 Q 
 ;
AttrMod(SCRATTR,TBLATTR,isKey,modified) ; Modified the field flag
 ;
 I (SCRATTR'=TBLATTR) D
 .	;
 .	S SCRATTR=TBLATTR
 .	I 'isKey S modified=1
 .	Q 
 ;
 Q SCRATTR
 ;
FILE(ORIGSID,NEWSID) ; New screen name (may be the same as ORIGSID)
 N vTp
 ;
 N N N REC N XPX N XPY
 N DBTBL20 N F N PGM
 ;
 TS (vobj):transactionid="CS"
 ;
 D
 .	N newscr,vop1,vop2,vop3 S vop1="SYSDEV",vop2=NEWSID,newscr=$$vRCgetRecord1Opt^RecordDBTBL2("SYSDEV",NEWSID,0,"")
 .	 S vop3=$G(^DBTBL(vop1,2,vop2,0))
 .	;
 .	S PGM=$P(vop3,$C(124),2)
 . Q 
 ;
 ; Load the original screen
 N origscr S origscr=$$vRCgetRecord1^RecordDBTBL2("SYSDEV",ORIGSID,0)
 ;
 ; Load screen-level processor code
 N ds,vos1,vos2,vos3,vos4 S ds=$$vOpen3()
 ;
 F  Q:'$$vFetch3()  D
 .	;
 . N dbtbl2pp,vop4 S vop4=$P(ds,$C(9),4),dbtbl2pp=$$vRCgetRecord1Opt^RecordDBTBL2PP($P(ds,$C(9),1),$P(ds,$C(9),2),$P(ds,$C(9),3),vop4,1,"")
 .	;
 .	S DBTBL20(vop4)=$P(dbtbl2pp,$C(12),1)
 . Q 
 ;
 N newscr S newscr=$$vReCp1(origscr)
 ;
 D DELSID^DBSDS(NEWSID)
 ;
 S vobj(newscr,-2)=0
  S vobj(newscr,-4)=NEWSID
 ;
 ; Save screen attributes
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),1)=YORIGIN
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),2)=XORIGIN
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),3)=+RULER
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),4)=$ZASCII(VIDEO,1)_","_$ZASCII(VIDEO,2)_","_$ZASCII(VIDEO,3)_","_$ZASCII(VIDEO,4)_","_$ZASCII(VIDEO,5)_","_$ZASCII(VIDEO,6)
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),5)=BUFFERS
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),8)=PY
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),9)=PX
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),10)=FORMHDG
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),11)=LASTFID
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),12)=STATUS
  S vobj(newscr,-100,"v5")="" S $P(vobj(newscr,"v5"),$C(124),13)=RHTMAR
 ;
  S vobj(newscr,-100,0)="" S $P(vobj(newscr,0),$C(124),2)=PGM
  S vobj(newscr,-100,0)="" S $P(vobj(newscr,0),$C(124),17)=1
  S vobj(newscr,-100,0)="" S $P(vobj(newscr,0),$C(124),3)=$P($H,",",1)
 ;
 I ($P(vobj(newscr,0),$C(124),8)="")  S vobj(newscr,-100,0)="" S $P(vobj(newscr,0),$C(124),8)=1
 I ($P(vobj(newscr,0),$C(124),9)="")  S vobj(newscr,-100,0)="" S $P(vobj(newscr,0),$C(124),9)="???"
  S vobj(newscr,-100,"0*")="" S $P(vobj(newscr),$C(124),1)=$P(vobj(newscr,0),$C(124),9) ; For backward compatibility
 ;
 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL2(newscr,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(newscr,-100) S vobj(newscr,-2)=1 TC:vTp  
 ;
 I ($P(vobj(newscr,0),$C(124),9)="???") D CTL^FORMDQ2C(NEWSID)
 ;
 ; Save screen-level processor code
 S N=""
 F  S N=$order(DBTBL20(N)) Q:(N="")  D
 .	;
 .	N dbtbl2pp S dbtbl2pp=$$vcdmNew^RecordDBTBL2PP() S vobj(dbtbl2pp,-3)="SYSDEV" S vobj(dbtbl2pp,-4)=NEWSID S vobj(dbtbl2pp,-5)=0 S vobj(dbtbl2pp,-6)=N
 .	;
 .  S $P(vobj(dbtbl2pp),$C(12),1)=DBTBL20(N)
 .	;
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL2PP(dbtbl2pp,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl2pp,-100) S vobj(dbtbl2pp,-2)=1 TC:vTp  
 .	K vobj(+$G(dbtbl2pp)) Q 
 ;
 ; Save screen fields
 S (F,REC)=0
 S (XPX,XPY)=""
 F  S XPY=$order(M(XPY)) Q:(XPY="")  D
 .	;
 .	F  S XPX=$order(M(XPY,XPX)) Q:(XPX="")  D
 ..		;
 ..		N I
 ..		N H N X
 ..		;
 ..		S REC=REC+1
 ..		N dbtbl2d S dbtbl2d=$$vcdmNew^RecordDBTBL2D() S vobj(dbtbl2d,-3)="SYSDEV" S vobj(dbtbl2d,-4)=NEWSID S vobj(dbtbl2d,-5)=REC
 ..		;
 ..		S D=$translate($get(D(XPY,XPX)),$char(124),$char(128))
 ..		S M=M(XPY,XPX)
 ..		;
 ..		; Header
 ..		S H=""
 ..		F I=1:1:6 S $piece(H,",",I)=$ZASCII(M,I)
 ..		S $piece(H,",",7)=0
 ..		;
 ..		S M=$translate($E(M,7,1048575),$char(124),$char(128))
 ..		;
 ..		I '(D="") D
 ...			;
 ...			I '($E(D,1)="@") S M=""
 ...			;
 ...			F I=1:1:20 S F(I)=$piece(D,dl,I)
 ...			Q 
 ..		;
 ..		; X = XPY*1000+XPX_PROTECT_"{"_Video
 ..		S X=(XPY*1000)+XPX
 ..		I ($piece(D,dl,8)>0) S X=X_"*"
 ..		S X=X_"#"_H
 ..		;
 ..	  S $P(vobj(dbtbl2d),$C(124),1)=X
 ..		;
 ..		; Text data  format= T  item name = @  size = field length
 ..		I (D="")!($E(D,1)="@") D
 ...			;
 ...			; Create literal tag for V3.x compiler
 ...			I (D="")  S $P(vobj(dbtbl2d),$C(124),5)="@ooe"_REC
 ...			; Keep original data item name @xxx
 ...			E   S $P(vobj(dbtbl2d),$C(124),5)=$piece(D,dl,1)
 ...			;
 ...		  S $P(vobj(dbtbl2d),$C(124),10)="T"
 ...		  S $P(vobj(dbtbl2d),$C(124),11)=M
 ...		  S $P(vobj(dbtbl2d),$C(124),2)="*"
 ...			;
 ...			I ($P(vobj(dbtbl2d),$C(124),1)["#")  S $P(vobj(dbtbl2d),$C(124),1)=$piece($P(vobj(dbtbl2d),$C(124),1),"#",1)_"*#"_$piece($P(vobj(dbtbl2d),$C(124),1),"#",2,99)
 ...			E   S $P(vobj(dbtbl2d),$C(124),1)=$piece($P(vobj(dbtbl2d),$C(124),1),"{",1)_"*{"_$piece($P(vobj(dbtbl2d),$C(124),1),"{",2,99)
 ...			Q 
 ..		E  D
 ...			;
 ...			; Post processor
 ...			I $piece(D,dl,14) D
 ....				;
 ....			  S $P(vobj(dbtbl2d),$C(124),8)=1
 ....				;
 ....				D FILEPP(NEWSID,REC,20,$piece(D,dl,14))
 ....				Q 
 ...			; Pre processor
 ...			I $piece(D,dl,13) D
 ....				;
 ....			  S $P(vobj(dbtbl2d),$C(124),9)=1
 ....				;
 ....				D FILEPP(NEWSID,REC,0,$piece(D,dl,13))
 ....				Q 
 ...			;
 ...			I ($E(F(1),1,2)="<<"),($E(F(1),$L(F(1))-2+1,1048575)=">>") D
 ....				;
 ....				; Data item entry syntax <<var,pmt,type,len,min,max,dec,pos,tbl>>
 ....				S M=F(1)
 ....				;
 ....				I (F(2)=""),(F(1)?1"<<"1A.AN1">>") D
 .....					;
 .....					S F(1)="@"_$E($piece(F(1),">>",1),3,99)
 .....					S F(2)="*"
 .....					Q 
 ....				; Keep orig Name
 ....				E  I ($E(F(2),1)="@") D
 .....					;
 .....					S F(1)=F(2)
 .....					S F(2)="*"
 .....					Q 
 ....				E  D
 .....					;
 .....					S F(1)="@ooe"_(REC+1)
 .....					S F(2)="*"
 .....					Q 
 ....				Q 
 ...			;
 ...		  S $P(vobj(dbtbl2d),$C(124),2)=F(2) ;  Node number
 ...		  S $P(vobj(dbtbl2d),$C(124),3)=F(3)
 ...			;
 ...			I ($E(F(1),1)="["),'(F(1)[",") S F(1)="[SYSDEV,"_$E($piece(F(1),"]",1),2,99)_"]"_$piece(F(1),"]",2)
 ...			;
 ...		  S $P(vobj(dbtbl2d),$C(124),5)=F(1) ; Data item
 ...		  S $P(vobj(dbtbl2d),$C(124),6)=F(6) ; Table lookup
 ...		  S $P(vobj(dbtbl2d),$C(124),7)=F(9) ; Pattern match
 ...		  S $P(vobj(dbtbl2d),$C(124),10)=F(5) ; Data type
 ...		  S $P(vobj(dbtbl2d),$C(124),11)=M ; Prompt
 ...		  S $P(vobj(dbtbl2d),$C(124),12)=F(7) ; Required
 ...		  S $P(vobj(dbtbl2d),$C(124),13)=F(10) ; Minimum value
 ...		  S $P(vobj(dbtbl2d),$C(124),14)=F(11) ; Maximum value
 ...		  S $P(vobj(dbtbl2d),$C(124),15)=F(4) ; Decimal precision
 ...			;
 ...			; Print format (v4.0)
 ...			I (F(18)'=F(5))  S $P(vobj(dbtbl2d),$C(124),18)=F(18)
 ...			;
 ...		  S $P(vobj(dbtbl2d),$C(124),21)=F(16) ; Delimiter
 ...		  S $P(vobj(dbtbl2d),$C(124),22)=F(17) ; Position
 ...			Q 
 ..		;
 ..	 S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL2D(dbtbl2d,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl2d,-100) S vobj(dbtbl2d,-2)=1 TC:vTp  
 ..		K vobj(+$G(dbtbl2d)) Q 
 .	Q 
 ;
  TC:$TL 
 ;
 K vobj(+$G(newscr)),vobj(+$G(origscr)) Q 
 ;
FILEPP(SID,SEQ,OPT,KEY) ; Pointer to PP array
 N vTp
 ;
 N N N Y N Z
 ;
 S (Y,Z)=1
 ;
 I ($order(PP(KEY,""),-1)>20) S Z=.001
 ;
 S N=""
 F  S N=$order(PP(KEY,N)) Q:(N="")  D
 .	;
 .	N PSEQ
 .	;
 .	S PSEQ=OPT+Y
 .	;
 .	N dbtbl2pp S dbtbl2pp=$$vcdmNew^RecordDBTBL2PP() S vobj(dbtbl2pp,-3)="SYSDEV" S vobj(dbtbl2pp,-4)=SID S vobj(dbtbl2pp,-5)=SEQ S vobj(dbtbl2pp,-6)=PSEQ
 .	;
 .  S $P(vobj(dbtbl2pp),$C(12),1)=PP(KEY,N)
 .	;
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordDBTBL2PP(dbtbl2pp,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(dbtbl2pp,-100) S vobj(dbtbl2pp,-2)=1 TC:vTp  
 .	;
 .	S Y=Y+Z
 .	K vobj(+$G(dbtbl2pp)) Q 
 ;
 Q 
 ;
SAVE(ZMSG) ; 
 ;
 N MENUID
 N NEWSID N OPTION N ORIGSID
 ;
 I ($D(M)=0) Q 1 ; Nothing to save
 ;
 I ($D(P)>0) D SELOFF^FORMSEL(1)
 ;
 I (+$get(ANCHOR)>0) D ANCHOF^FORMSEL
 ;
 S OPTION(1)=$E(SID,2,1048575)
 ;
 ; menu # 105 = Save as ~,Save as other
 ;        106 = Save as ~,Save as other|Exit from editor
 ;
 S MENUID=105
 I '($get(ZMSG)="") S MENUID=106
 ;
 S OPTION=$$^DBSMBAR(MENUID,"","","",.OPTION)
 I (OPTION'>0) Q 0
 ;
 ;set ORIGSID = SID
 S ORIGSID=$E(SID,2,1048575) ; Point back to the original name
 S NEWSID=ORIGSID
 ;
 I (OPTION=2) D  Q:(NEWSID="") 0
 .	;
 .	N isDone
 .	;
 .	S isDone=0
 .	F  D  Q:isDone 
 ..		;
 ..		S NEWSID=$$^FORMREAD(NEWSID,12,"Name: ","U")
 ..		;
 ..		I (NEWSID="") S isDone=1
 ..		E  I NEWSID=ORIGSID S isDone=1
 ..		E  I ($D(^DBTBL("SYSDEV",2,NEWSID))) D
 ...			;
 ...			WRITE $$MSG^FORM("Screen already exists",1)
 ...			HANG 1
 ...			Q 
 ..		E  S isDone=1
 ..		Q 
 .	Q 
 ;
 ; Remove old DATA ITEM index information
 I ($D(^DBTBL("SYSDEV",2,NEWSID))) D INDEX(NEWSID,"D")
 ;
 D FILE(SID,NEWSID)
 ;
 WRITE $$MSG^FORM(NEWSID_" Saved")
 ;
 ; Create new DATA ITEM x-ref file ^DBSINDX
 D INDEX(NEWSID,"A")
 ;
 S EDITED=0
 ;
 Q 1
 ;
INDEX(ID,DQFUN) ; 
 ;
 N DBOPT
 ;
 S DBOPT=2
 ;
 D ^DBSUTL3
 ;
 Q 
 ;
OBJ(X) ; 
 ;
 N DEC N I N LEN
 N DSP N T N TY N VAR N XE
 ;
 S (TY,XE)=""
 S VAR=$piece(X,dl,1)
 S DSP=$piece(X,dl,18)
 S LEN=$piece(X,dl,3)
 S DEC=$piece(X,dl,4)
 ;
 F I=1:1 S T=$piece(DSP,",",I) Q:(T="")  D
 .	;
 .	I (T="$") D
 ..		;
 ..		S TY="N"
 ..		S DEC=2
 ..		Q 
 .	E  I (T="E") D
 ..		;
 ..		S TY="N"
 ..		S DEC=2
 ..		;
 ..		I '(XE="") S XE=XE_" "
 ..		S XE=XE_"S m=$FN(m,"","")"
 ..		Q 
 .	E  D
 ..		;
 ..		I ($E(T,1)="R") D
 ...			;
 ...			S TY="N"
 ...			I ($E(T,2)="D") S DEC=+$E(T,3,9)
 ...			Q 
 ..		I ("NDLFUTC"[T) S TY=T ; Type is defined in layout
 ..		I (T="ZS") S TY="N"
 ..		I ($E(T,1)="J") S TY="T"
 ..		Q 
 .	Q 
 ;
 I ((TY="$")!(TY="N")) S m=9
 E  I (TY="D") S m="mm/dd/yyyy"
 E  I (TY="C") S m="hh:mm AM"
 E  S m="x"
 ;
 S $piece(m,m,LEN+1)=""
 ;
 I (DEC>0),(TY="N") S m=$E(m,1,LEN-DEC-1)_"."_$E(m,1,DEC)
 ;
 ;  #ACCEPT Date=05/23/06; Pgm=RussellDS; CR=20967
 I '(XE="") XECUTE XE
 ;
 I (LEN>0) D
 .	;
 .	I (TY="N") S m=$E(m,$L(m)-LEN+1,1048575)
 .	E  S m=$E(m,1,LEN)
 .	Q 
 ;
 Q 
 ;
GETSID() ; 
 ;
 N %NOPRMT N %READ N %TAB N SID N VFMQ
 ;
 S SID=""
 S %TAB("SID")=".SID1/TBL=[DBTBL]/XPP=D GETSID1^FORMDQ2"
 ;
 S %READ="SID"
 ;
 D ^UTLREAD I (VFMQ="Q") S SID=""
 ;
 Q $ZCONVERT(SID,"U")
 ;
GETSID1 ; 
 ;
 Q:(X="") 
 ;
 ; Check to see if linked screen
 N dbtbl2,vop1,vop2,vop3 S vop1="SYSDEV",vop2=X,dbtbl2=$$vRCgetRecord0Opt^RecordDBTBL2("SYSDEV",X,0,"")
  S vop3=$G(^DBTBL(vop1,2,vop2,-1))
 ;
 I '($P(vop3,$C(124),1)="") D  Q 
 .	;
 .	S ER=1
 .	S RM="Linked screen"
 .	Q 
 ;
 I (X'?1U.UN) D  Q 
 .	;
 .	S ER=1
 .	S RM="Alphanumeric format only"
 .	Q 
 ;
 S I(3)=""
 ;
 Q 
 ;
SAVECOPY(SID) ; Screen ID
 ;
 N ZSID
 ;
 S ZSID=SID
 I '($E(ZSID,1)="z") S ZSID="z"_SID
 ;
 I ($D(^DBTBL("SYSDEV",2,ZSID))) D  Q:(ZSID="") 
 .	;
 .	I $$YN^DBSMBAR("","Use backup copy of "_SID_"?") S ZSID=""
 .	;
 .	E  D DELSID^DBSDS(ZSID)
 .	Q 
 ;
 D COPYSID^DBSDS(SID,ZSID,0)
 ;
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^18993" ; Signature - LTD^TIME^USER^SIZE
 ;
vOpen1() ; LIBS,SID,SEQ FROM DBTBL2D WHERE LIBS='SYSDEV' AND SID=:SID ORDER BY SEQ ASC
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(SID) I vos3="" G vL1a0
 S vos4=0
vL1a4 S vos4=$O(^DBTBL("SYSDEV",2,vos3,vos4),1) I vos4="" G vL1a0
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds="SYSDEV"_$C(9)_vos3_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vOpen2() ; LIBS,SID,SEQ,PSEQ FROM DBTBL2PP WHERE LIBS='SYSDEV' AND SID=:SID AND SEQ=:V1 ORDER BY PSEQ ASC
 ;
 ;
 S vos5=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos5=0 Q
vL2a1 S vos6=$$BYTECHAR^SQLUTL(254)
 S vos7=$G(SID) I vos7="" G vL2a0
 S vos8=$G(V1)
 S vos8=+vos8
 S vos9=""
vL2a6 S vos9=$O(^DBTBL("SYSDEV",2,vos7,vos8,vos9),1) I vos9="" G vL2a0
 Q
 ;
vFetch2() ;
 ;
 ;
 I vos5=1 D vL2a6
 I vos5=2 S vos5=1
 ;
 I vos5=0 S ds2="" Q 0
 ;
 S ds2="SYSDEV"_$C(9)_vos7_$C(9)_vos8_$C(9)_$S(vos9=vos6:"",1:vos9)
 ;
 Q 1
 ;
vOpen3() ; LIBS,SID,SEQ,PSEQ FROM DBTBL2PP WHERE LIBS='SYSDEV' AND SID=:ORIGSID AND SEQ = 0
 ;
 ;
 S vos1=2
 D vL3a1
 Q ""
 ;
vL3a0 S vos1=0 Q
vL3a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(ORIGSID) I vos3="" G vL3a0
 S vos4=""
vL3a4 S vos4=$O(^DBTBL("SYSDEV",2,vos3,0,vos4),1) I vos4="" G vL3a0
 Q
 ;
vFetch3() ;
 ;
 ;
 I vos1=1 D vL3a4
 I vos1=2 S vos1=1
 ;
 I vos1=0 S ds="" Q 0
 ;
 S ds="SYSDEV"_$C(9)_vos3_$C(9)_0_$C(9)_$S(vos4=vos2:"",1:vos4)
 ;
 Q 1
 ;
vReCp1(v1) ; RecordDBTBL2.copy: DBTBL2
 ;
 N vNod,vOid
 I $G(vobj(v1,-2)) D
 . S:'$D(vobj(v1,"v1")) vobj(v1,"v1")=$G(^DBTBL(vobj(v1,-3),2,vobj(v1,-4),-1))
 . S:'$D(vobj(v1,"v5")) vobj(v1,"v5")=$G(^DBTBL(vobj(v1,-3),2,vobj(v1,-4),-5))
 .	F vNod=0 S:'$D(vobj(v1,vNod)) vobj(v1,vNod)=$G(^DBTBL(vobj(v1,-3),2,vobj(v1,-4),vNod))
 S vOid=$$copy^UCGMR(v1)
 Q vOid
