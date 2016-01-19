 ; 
 ; **** Routine compiled from DATA-QWIK Procedure SCADRV5 ****
 ; 
 ; 02/24/2010 18:22 - pip
 ; 
 ;
 D INIT
 Q 
 ;
INIT ; 
 N %TAB N %READ N VFMQ N %NOPRMT
 N I
 ;
 K OLNTB
 S %O=1
 S %TAB("MENU")=".MENU1/TBL=[SCAMENU0]"
 S %READ="@@%FN,,,MENU/REQ" S %NOPRMT="F"
 D ^UTLREAD
 I VFMQ="Q" Q 
 ;
 S I=1
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen1()
 F  Q:'$$vFetch1()  D
 . S MNU(I)=$P(rs,$C(9),2)_"|"_$P(rs,$C(9),3)
 .	S I=I+1
 .	Q 
 ;
 D EXT
 Q 
 ;
EXT ; Entry point from ^SCADRV8
 D DSPHDR
 D OPT
 Q 
 ;
OPT ; 
 ;
 D DSPSEQ
 K OP
 I ($order(MNU(""))="") D OP1 Q 
 S OP=$$^DBSMBAR(33)
 I OP=1 D ADD D OPT Q 
 I OP=2 D MOD D OPT Q 
 I OP=3 D DEL D OPT Q 
 I OP=4 D FILE
 Q 
 ;
OP1 ; 
 N MASK
 ;
 S (MASK(2),MASK(3))=""
 S OP=$$^DBSMBAR(33,"",.MASK) Q:OP=3 
 I OP=1 D ADD D OPT Q 
 ;
 ; 4th option in mbar 33 is file
 I OP=4 D FILE
 Q 
 ;
ADD ; 
 S X=""
 ;
 I '$D(MNU) S SEQ=0 D A1 Q 
 ;
 ; Insert After Sequence:
 WRITE $$BTM^%TRMVT,$$^MSG("1239")
 S SEQ=$$TERM^%ZREAD I (SEQ="")!(%fkey="ESC") Q 
 ;
 ; Invalid sequence
 I SEQ'=0,'$D(MNU(SEQ)) S RM=$$^MSG("1466") D ERR D ADD Q 
 ;
 F  Q:'(X="")  D A1
 Q 
 ;
A1 ; 
 ;
 ; Function Name / Menu #:
 WRITE $$BTM^%TRMVT,$$^MSG("1137")
 S X=$$TERM^%ZREAD
 ;
 D A2
 Q 
 ;
A2 ; 
 N vpc
 N I N N
 N DES N SC
 ;
 S DES=""
 I %fkey="ESC" Q 
 I %fkey="FND"!(%fkey="SEL") D DSPFUN D A1 Q 
 ;
 ; Press SELECT for a list of valid options
 I %fkey="HLP" S RM=$$^MSG("2218") D ERR Q 
 ;
 N rs,vos1,vos2,vos3,vos4 S rs=$$vOpen2()
 ;
 N scatbl,vop1 S scatbl=$$vRCgetRecord1Opt^RecordSCATBL(X,0,.vop1)
 ;
 ; Invalid function/menu
 I '(X?1N.N),'$G(vos1),('$G(vop1)) S RM=$$^MSG("1362") D ERR D A1 Q 
 ;
 N rs1,vos5,vos6,vos7,vos8 S rs1=$$vOpen3()
 I $$vFetch3() S DES=rs1
 ;
 ; Description:  ~p1
 WRITE $$BTM^%TRMVT_$$^MSG(8233,DES),$$CUB^%TRMVT($L(DES))
 S DES=$$TERM^%ZREAD(DES,36,1) S vpc=(%fkey="ESC") Q:vpc 
 S SC("SEQ")=DES_"|"_X
 ;
 S N=SEQ
 F  S N=$order(MNU(N)) Q:(N="")  S X(N)=MNU(N)
 S MNU(SEQ+1)=SC("SEQ") S I=SEQ+2
 ;
 F I=I:1 S N=$order(X(N)) Q:(N="")  S MNU(I)=X(N)
 Q 
 ;
MOD ; 
 ;
 ; Enter Sequence to Modify
 WRITE $$BTM^%TRMVT,$$^MSG("953")
 S SEQ=$$TERM^%ZREAD I (SEQ="")!(%fkey="ESC") Q 
 ;
 ; Invalid sequence
 I '$D(MNU(SEQ)) S RM=$$^MSG("1466") D ERR D MOD Q 
 ;
 S X=$piece(MNU(SEQ),"|",1)
 ;
 ; Description:  ~p1
 WRITE $$BTM^%TRMVT_$$^MSG(8233,X),$$CUB^%TRMVT($L(X))
 S X=$$TERM^%ZREAD(X,36,1) I (%fkey'="ESC"),$L(X) S $piece(MNU(SEQ),"|",1)=X
 Q 
 ;
DEL ; 
 N I N N
 ;
 ; Enter Sequence to Delete
 WRITE $$BTM^%TRMVT,$$^MSG("952")
 S SEQ=$$TERM^%ZREAD I SEQ=""!(%fkey="ESC") Q 
 ;
 ; Invalid sequence
 I '$D(MNU(SEQ)) S RM=$$^MSG("1466") D ERR D DEL Q 
 ;
 ; Are you sure?
 Q:'$$YN^DBSMBAR("",$$^MSG("307")) 
 ;
 S (N,I)=SEQ
 F I=I:1 S N=$order(MNU(N)) Q:(N="")  S MNU(I)=MNU(N)
 K MNU(I)
 I $D(MNU) Q 
 D DSPSEQ
 ;
 ; Delete empty menu
 I $$YN^DBSMBAR("",$$^MSG("795")) D
 .	;
 .	; Ensure deletion sent to FEPs
 .	D vDbDe1()
 .	D FILE
 .	Q 
 Q 
 ;
FILE ; 
 N vTp
 N I N N
 N L N M N NEWARR
 ;
 S N=""
 N rs,vos1,vos2,vos3,vos4,vos5 S rs=$$vOpen4()
 F  Q:'$$vFetch4()  D
 . S N=$P(rs,$C(9),1)
 . S OLDARR(N)=$P(rs,$C(9),2)_"|"_$P(rs,$C(9),3)
 .	Q 
 S N=0
 F I=1:1 S N=$order(MNU(N)) Q:(N="")  S NEWARR(I)=MNU(N)
 ;
 S (L,M)=""
 F  S M=$order(OLDARR(M)) Q:(M="")  D
 .	D vDbDe2()
 .	Q 
 ;
 F  S L=$order(NEWARR(L)) Q:(L="")  D
 .	N scamenu S scamenu=$$vcdmNew^RecordSCAMENU()
 .  S vobj(scamenu,-3)=MENU
 .  S vobj(scamenu,-4)=L
 .  S $P(vobj(scamenu),$C(124),1)=$piece(NEWARR(L),"|",1)
 .  S $P(vobj(scamenu),$C(124),2)=$piece(NEWARR(L),"|",2)
 . S vTp=($TL=0) TS:vTp (vobj):transactionid="CS" D vSave^RecordSCAMENU(scamenu,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/") K vobj(scamenu,-100) S vobj(scamenu,-2)=1 TC:vTp  
 .	K vobj(+$G(scamenu)) Q 
 ;
 K MNU
 Q 
 ;
ERR ; 
 ;
 WRITE $$MSG^%TRMVT(RM,1,1,0,23)
 Q 
 ;
DSPHDR ; Display menu header
 N DX N DY N N N Z
 ;
 ;  Menu:
 S X=$$^MSG("4851")_MENU
 ;
 N rs,vos1,vos2,vos3,vos4 S rs=$$vOpen5()
 I $$vFetch5() S Y=rs
 ;
 S X=X_$J("",((80-$L(Y))\2)-$L(X))_Y S X=X_$J("",80-$L(X))
 ;
 WRITE $$CLEAR^%TRMVT_$$VIDREV^%TRMVT_X_$$VIDOFF^%TRMVT
 ;
 Q 
 ;
DSPSEQ ; Display menu functions and sub-menus
 ;
 S Z=$order(MNU(""),-1) S Z=(Z\2)+(Z#2)
 ;
 WRITE $$CLR^%TRMVT(2,Z+2)
 ;
 D NX
 Q 
 ;
NX ; 
 N N N DX
 N X
 ;
 S DX=0 S N=""
 ;
 F  S N=$order(MNU(N)) Q:(N="")  D
 .	S X=N_$J("",5-$L(N))_$piece(MNU(N),"|",1)
 .	S X=$E(X,1,40)
 .	S DY=N+1
 .	I N>Z S DX=40 S DY=N-Z+1
 .	WRITE $$CUP^%TRMVT(DX,DY)_X
 .	Q 
 ;
 S OLNTB=(Z+1)*1000
 ;
 Q 
 ;
DSPFUN ; Function SELECT
 ;
 S X=$$^DBSTBL("[SCATBL]",X,"T","","",.TOP,.BTM,"[SCATBLDOC]")
 Q 
 ;
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "60187^24547^Sanjay Chhrabria^5584" ; Signature - LTD^TIME^USER^SIZE
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe1() ; DELETE FROM SCAMENU0 WHERE MENU=:MENU
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 TS (vobj):transactionid="CS"
 N vRec S vRec=$$vRCgetRecord1^RecordSCAMENU0(MENU,0)
 I $G(vobj(vRec,-2))=1 S vobj(vRec,-2)=3 D
 .	;     #ACCEPT Date=07/09/2008; Pgm=RussellDS; CR=30801; Group=BYPASS
 .	;*** Start of code by-passed by compiler
 .	D vSave^RecordSCAMENU0(vRec,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/",0)
 .	;*** End of code by-passed by compiler ***
 .	Q 
  TC:$TL 
 K vobj(+$G(vRec)) Q 
 ; ----------------
 ;  #OPTION ResultClass 1
vDbDe2() ; DELETE FROM SCAMENU WHERE MNUMB=:MENU and SNUMB=:M
 ;
 ;  #OPTIMIZE FUNCTIONS OFF
 TS (vobj):transactionid="CS"
 N vDs,vos1,vos2,vos3,vos4 S vDs=$$vOpen6()
 F  Q:'$$vFetch6()  D
 . N vRec S vRec=$$vRCgetRecord1^RecordSCAMENU($P(vDs,$C(9),1),$P(vDs,$C(9),2),1)
 .	S vobj(vRec,-2)=3
 .	;     #ACCEPT Date=07/09/2008; Pgm=RussellDS; CR=30801; Group=BYPASS
 .	;*** Start of code by-passed by compiler
 .	D vSave^RecordSCAMENU(vRec,"/CASDEL/INDEX/JOURNAL/LOG/TRIGAFT/TRIGBEF/UPDATE/VALDD/VALFK/VALREQ/VALRI/VALST/",0)
 .	;*** End of code by-passed by compiler ***
 .	K vobj(+$G(vRec)) Q 
  TC:$TL 
 Q 
 ;
vOpen1() ; SNUMB,MDESC,FUNMENU FROM SCAMENU WHERE MNUMB=:MENU ORDER BY SNUMB
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(MENU)
 S vos4=""
vL1a4 S vos4=$O(^SCATBL(0,vos3,vos4),1) I vos4="" G vL1a0
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
 S vos5=$G(^SCATBL(0,vos3,vos4))
 S rs=$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",1)_$C(9)_$P(vos5,"|",2)
 ;
 Q 1
 ;
vOpen2() ; SNUMB FROM SCAMENU WHERE MNUMB=:X
 ;
 ;
 S vos1=2
 D vL2a1
 Q ""
 ;
vL2a0 S vos1=0 Q
vL2a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(X)
 S vos4=""
vL2a4 S vos4=$O(^SCATBL(0,vos3,vos4),1) I vos4="" G vL2a0
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
vOpen3() ; DESC FROM SCATBL WHERE FN=:X
 ;
 ;
 S vos5=2
 D vL3a1
 Q ""
 ;
vL3a0 S vos5=0 Q
vL3a1 S vos6=$$BYTECHAR^SQLUTL(254)
 S vos7=$G(X) I vos7="" G vL3a0
 I '($D(^SCATBL(1,vos7))#2) G vL3a0
 Q
 ;
vFetch3() ;
 ;
 ;
 ;
 I vos5=0 S rs1="" Q 0
 ;
 S vos5=100
 S vos8=$G(^SCATBL(1,vos7))
 S rs1=$P(vos8,"|",1)
 S vos5=0
 ;
 Q 1
 ;
vOpen4() ; SNUMB,MDESC,FUNMENU FROM SCAMENU WHERE MNUMB=:MENU ORDER BY SNUMB
 ;
 ;
 S vos1=2
 D vL4a1
 Q ""
 ;
vL4a0 S vos1=0 Q
vL4a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(MENU)
 S vos4=""
vL4a4 S vos4=$O(^SCATBL(0,vos3,vos4),1) I vos4="" G vL4a0
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
 S vos5=$G(^SCATBL(0,vos3,vos4))
 S rs=$S(vos4=vos2:"",1:vos4)_$C(9)_$P(vos5,"|",1)_$C(9)_$P(vos5,"|",2)
 ;
 Q 1
 ;
vOpen5() ; DES FROM SCAMENU0 WHERE MENU=:MENU
 ;
 ;
 S vos1=2
 D vL5a1
 Q ""
 ;
vL5a0 S vos1=0 Q
vL5a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(MENU)
 I '($D(^SCATBL(0,vos3))#2) G vL5a0
 Q
 ;
vFetch5() ;
 ;
 ;
 ;
 I vos1=0 S rs="" Q 0
 ;
 S vos1=100
 S vos4=$G(^SCATBL(0,vos3))
 S rs=$P(vos4,"|",1)
 S vos1=0
 ;
 Q 1
 ;
vOpen6() ; MNUMB,SNUMB FROM SCAMENU WHERE MNUMB=:MENU AND SNUMB=:M
 ;
 ;
 S vos1=2
 D vL6a1
 Q ""
 ;
vL6a0 S vos1=0 Q
vL6a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=$G(MENU)
 S vos4=$G(M)
 I '($D(^SCATBL(0,vos3,vos4))#2) G vL6a0
 Q
 ;
vFetch6() ;
 ;
 ;
 ;
 I vos1=0 S vDs="" Q 0
 ;
 S vos1=100
 S vDs=vos3_$C(9)_vos4
 S vos1=0
 ;
 Q 1
