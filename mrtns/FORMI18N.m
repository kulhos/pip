 ; 
 ; **** Routine compiled from DATA-QWIK Procedure FORMI18N ****
 ; 
 ; 01/27/2016 00:53 - kulhan
 ; 
 ;DO NOT MODIFY  FORMI18N|FORMI18N|||||||1
FORMI18N(topy,phrtyp,fldval1,fldval2,mldfv2) ; FORMI18N
 ;
 D START
 ;
 Q 
 ;
START ; 
 ;
 N savDEL N status
 N EXTANT N ORIGIN N vtblrtn
 ;
 ; Have to save it and use later
 S savDEL=$get(DELETE)
 ;
 ; Don't init again if called by OOE
 I ($get(I18NOOE)="") D FRMINIT
 ;
 S vtblrtn="N X D VW^FORMI18N" S status=$get(STATUS)
 ;
 S topy=$get(topy)
 I topy="" S topy=1
 S ORIGIN=topy_";"_LFTMAR S EXTANT=5_";"_(RHTMAR-LFTMAR-1)
 ;
 D MAIN
 ;
 Q 
 ;
MAIN ; Run main part of program
 ;
 D VRUN
 D FINISH
 ;
 Q 
 ;
FINISH ; 
 ;
 ; Restore DELETE again
 S DELETE=savDEL
 ;
 ; Clean up the frame
 I ($get(I18NOOE)=""),('%i18nmtra) D VREPRNT^@PGM
 ;
 Q 
 ;
VRUN ; Main part control
 ;
 N px N py N %MAX N %PG N %PAGE
 N DATA N VO N %fkey N %TAB N zbterm
 ;
 ; Clear Region for OBJECT definition
 D CLRRGN^FORMFUN
 ;
 ; Remove status line
 I status D OFF^FORMSTAT
 ;
 D SCRNPRE
 ;
 D VBX D VPR D VDA D ^DBSPNT() D VTAB
 ;
 Q 
 ;
VPR ; Build the prompts
 ;
 S VO="1||13"
 S VO(0)="0;0|"_vWID
 S VO(1)=$ZCHAR(topy)_$ZCHAR(17)_$ZCHAR(46)_$ZCHAR(2)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_"01T  Master Language Dictionary Translation Utility  "
 ;
 Q 
 ;
VDA ; Build screen data
 ;
 S fldval2=$get(fldval2)
 ;
 S VO="5|2|13"
 S VO(2)=$ZCHAR((topy+1))_$ZCHAR(5)_$ZCHAR(20)_$ZCHAR(2)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_"01T"_$E($get(flddes1),1,20)
 S VO(3)=$ZCHAR((topy+2))_$ZCHAR(vSTART)_$ZCHAR(vINP)_$ZCHAR(2)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_"01T"_$E($get(fldval1),1,vINP)
 S VO(4)=$ZCHAR((topy+3))_$ZCHAR(5)_$ZCHAR(20)_$ZCHAR(2)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_"01T"_$E($get(flddes2),1,20)
 S VO(5)=$ZCHAR((topy+4))_$ZCHAR(vSTART)_$ZCHAR(vINP)_$ZCHAR(2)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_$ZCHAR(0)_"00T"_$E($get(fldval2),1,vINP)
 ;
 Q 
 ;
VTAB ; Build the %TAB(array)
 ;
 N OLNTB N REQ N VPB N VPT
 N DFID N DLIB N PGM
 ;
 S %MAX=4 S VPT=1 S VPB=7 S PGM=$T(+0) S DLIB="SYSDEV" S DFID="I18NMLD" S REQ=1
 S OLNTB=24001
 ;
 D VTAB1
 ;
 Q 
 ;
VTAB1 ; 
 ;
 D VTBL
 ;
 D ^DBSCRT8
 I status,('($get(I18NOOE)="")) D ON^FORMSTAT
 S ZB=13
 ;
 Q 
 ;
VTBL ; Create %TAB(array)
 ;
 ; 1 2 3  4 5   6   7-9 10-11
 ; DY,DX,SZ PT REQ TYPE DEL POS |NODE|ITEM NAME|TBL|FMT|PP|PRE|MIN|MAX|DEC
 ;
 S %TAB(1)=$ZCHAR((topy+0))_$ZCHAR(4)_$ZCHAR(20)_"20T|*flddes1|[*]@flddes1"
 S %TAB(2)=$ZCHAR((topy+1))_$ZCHAR(vSTART)_$ZCHAR(vINP)_"20T|*fldval1|[*]@fldval1"
 S %TAB(3)=$ZCHAR((topy+2))_$ZCHAR(4)_$ZCHAR(20)_"20T|*flddes2|[*]@flddes2"
 S %TAB(4)=$ZCHAR((topy+3))_$ZCHAR((vSTART-1))_$ZCHAR(vINP)_"00T|*fldval2|[*]@fldval2|||D VP2^FORMI18N|D VP1^FORMI18N"
 ;
 Q 
 ;
SCRNPRE ; Screen Pre-Processor
 ;
 I (phrtyp=11),(fldval2="") S fldval2=fldval1
 S vWID=1 S vINP=132 S vSTART=1
 I $L(fldval1)<80 S vINP=80
 I $L(fldval1)<70 S vINP=70 S vWID=0 S vSTART=7
 ;
 Q 
 ;
VSPP ; Screen Post-Processor
 ;
 I ($D(phrtab(fldval2))#2) S %O=2
 ;
 Q 
 ;
VDEPRE ; Data Entry Pre-proc
 ;
 D VPOS
 ;
 Q 
 ;
VPOS ; User defined post processor's
 ;
 D VP1
 ;
 Q 
 ;
VP1 ; Preproc for fldval2
 ;
 I 'maxphr D CHANGE^DBSMACRO("TBL","")
 E  D CHANGE^DBSMACRO("TBL","phrtab(")
 ;
 Q 
 ;
VP2 ; Postproc for fldval2
 ;
 N i
 ;
 I 'srcphr Q 
 ;
 ; Locking if admin set the flag
 I $get(uclflgMLD) D  Q 
 .	S RM=$$^MSG(5914)
 .	Q 
 E  D CHANGE^DBSMACRO("TBL","")
 ;
 S mldfv2=X
 ;
 I phrtyp=2 D
 .	N bwd N done N fwd
 .	;
 .	; translator did the work (or no space)
 .	I mldfv2["@"!(mldfv2'[" ") Q 
 .	; start at middle of message
 .	S (fwd,bwd)=$L(mldfv2)\2
 .	S done=0
 .	F  Q:done  D
 ..		; found space going fwd
 ..		I $E(mldfv2,fwd)=" " S done=fwd
 ..		; move pointers
 ..		S fwd=fwd+1 S bwd=bwd-1
 ..		; found space going backwd
 ..		I bwd>0,$E(mldfv2,bwd)=" " S done=bwd
 ..		I done S mldfv2=$E(mldfv2,1,done-1)_"@"_$E(mldfv2,done+1,999)
 ..		Q 
 .	Q 
 ;
 ; Phrase type 3 Screen or Report prompts with :
 I phrtyp=3 D  Q:ER 
 .	I ($E(X,$L(X))'=":") D SETERR^DBSEXECU("I18NMLD","MSG",5915) Q 
 .	S mldfv2=$translate(X,":","")
 .	Q 
 ;
 ; Special Handeling for Phrase type 6 = ^STBLPROMPT
 I phrtyp=6 D
 .	N e
 .	;
 .	; Must have the same number of variables as original phrase
 .	I $L(fldval1,"~")'=$L(X,"~") D SETERR^DBSEXECU("I18NMLD","MSG",5916) Q 
 .	F i=2:1:$L(X,"~") D
 ..		I $E($piece(X,"~",i),1,2)'?1A1N D SETERR^DBSEXECU("I18NMLD","MSG",5916) Q 
 ..		S e(i-1)="~"_$E($piece(X,"~",i),1,2)
 ..		Q 
 .	F i=1:1:9 D
 ..		I '($D(e(i))#2) Q 
 ..		I $ZCONVERT(fldval1,"U")'[$ZCONVERT(e(i),"U") S i=10 D SETERR^DBSEXECU("I18NMLD","MSG",5916) Q 
 ..		Q 
 .	Q 
 ;
 ; Phrase type 11 - Mixed text and <<functions>>
 I phrtyp=11 D
 .	N edit
 .	S edit=$$CHECK(fldval1,X)
 .	I edit=0 D SETERR^DBSEXECU("I18NMLD","MSG",5917)
 .	Q 
 ;
 Q 
 ;
CHECK(src,trg) ; Routine called by post processor to edit ptype 11
 ; making sure that source <<>> appears in target
 ;
 N same
 N nte
 ;
 D getnte(src,1)
 D getnte(trg,2)
 S same=1 S nte=""
 ;
 N rs,vos1,vos2,vos3,vos4  N V1 S V1=$J S rs=$$vOpen1()
 F  Q:'$$vFetch1()  D
 .	I same=0 Q 
 . S nte=rs
 .	N tmprpt2,vop1 S tmprpt2=$$vRCgetRecord1Opt^RecordTMPRPT2(0,.vop1)
 .	I '$G(vop1) S same=0
 .	 N V2,V3 S V2=$J,V3=nte  K 
 . Q 
 ;
 N rs1,vos5,vos6,vos7,vos8  N V2 S V2=$J S rs1=$$vOpen2()
 I same,(''$G(vos5)) S same=0
 ;
 Q same
 ;
getnte(p,x) ; 
 N vTp
 ;
 N i
 N nte
 ;
  N V1,V2 S V1=$j,V2=x  K 
 ;
 F i=2:1:$L(p,"<<") D
 .	S nte=$piece($piece(p,"<<",i),">>")
 .	N tmprpt2 S tmprpt2=$$vRCgetRecord1^RecordTMPRPT2(0)
 .	 S vobj(tmprpt2,1,1)="" N von S von="" F  S von=$O((1,von)) quit:von=""  S vobj(tmprpt2,1,1)=vobj(tmprpt2,1,1)_(1,von)
 .  S vobj(tmprpt2,1,1)=""
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vReSav1(tmprpt2) K vobj(tmprpt2,-100) S vobj(tmprpt2,-2)=1 TC:vTp  
 .	K vobj(+$G(tmprpt2)) Q 
 ;
 Q 
 ;
FRMINIT ; Initialise global var's used by ^FORMxxx procedures
 ;
 N KBI
 N BEL N fileflag N GLDKEY N JRNL N N N NUL N TAB N zbtxt
 ;
 I $get(ST8)="" S ST8="||2||||||32|1;0;2||1"
 E  S $piece(ST8,"|",12)=0
 ;
 D
 .	I $get(%UID)="" S %UID=1 S %UCLS="SCA"
 .	;
 .	I '($D(YLIMIT)#2) S YLIMIT=255
 .	I '($D(XLIMIT)#2) S XLIMIT=255
 .	;
 .	S LFTMAR=1 S TOPMAR=1
 .	S BOTMAR=23 S RHTMAR=80
 .	;
 .	S zbtxt="x"
 .	;
 .	S N="" S fileflag="" S GLDKEY="" S JRNL=""
 .	S NUL=$char(0) S BEL=$char(7) S TAB=$char(9) S KBI=0
 .	;
 .	S DELETE=127
 .	S TIMEOUT=60
 .	;
 .	D VIDINIT^FORMINIT
 .	D TRMSET^FORMINIT
 .	Q 
 ;
 D REST8^FORMINIT D ST8^FORMINIT
 ;
 ; Save screen attributes
 S DBTBL2(-5)=ST8
 ;
 Q 
 ;
VBX ; 
 ;
 D BOX^%TRMVT(ORIGIN,EXTANT)
 ;
 Q 
 ;
VW ; 
 ;
 D PUTRGN^FORMFUN(ORIGIN,EXTANT)
 D VREPRNT
 ;
 Q 
 ;
VREPRNT ; 
 ;
 D VBX D VPR D VDA D ^DBSPNT()
 ;
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^9018" ; Signature - LTD^TIME^USER^SIZE
 ;
vOpen1() ; KEY2 FROM TMPRPT2 WHERE PID=:V1 AND KEY1=1
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(V1) I vos3="",'$D(V1) G vL1a0
 Q
 ;
vFetch1() ;
 ;
 ;
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos1=100
 S vos4=()
 S rs=vos4
 S vos1=0
 ;
 Q 1
 ;
vOpen2() ; KEY2 FROM TMPRPT2 WHERE PID=:V2 AND KEY1=2
 ;
 ;
 S vos5=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos5=0 Q
vL2a1 S vos6=$$BYTECHAR^SQLUTL(254)
 S vos7=$G(V2) I vos7="",'$D(V2) G vL2a0
 Q
 ;
vFetch2() ;
 ;
 ;
 ;
 I vos5=0 S rs1="" Q 0
 ;
 S vos5=100
 S vos8=()
 S rs1=vos8
 S vos5=0
 ;
 Q 1
 ;
vReSav1(tmprpt2) ; RecordTMPRPT2 saveNoFiler()
 ;
 Q
