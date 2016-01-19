 ; 
 ; **** Routine compiled from DATA-QWIK Screen DBTBL5Q ****
 ; 
 ; 02/24/2010 18:33 - pip
 ; 
V00S074(%O,fDBTBL5Q) ; -  - SID= <DBTBL5Q> QWIK Report Definition
 ;;Copyright(c)2010 Sanchez Computer Associates, Inc.  All Rights Reserved - 02/24/2010 18:33 - pip
 ;
 N KEYS N KVAR N VFSN N VO N VODFT N VPGM N vPSL N VSID N VSNAME
 ;
 ; %O (0-Create  1-Modify  2-Inquiry  3-Delete  4-Print  5-Blank screen)
 ;
 S:'($D(%O)#2) %O=5
 I (%O=5) D
 .	I '($D(fDBTBL5Q)#2)  K vobj(+$G(fDBTBL5Q)) S fDBTBL5Q=$$vcdmNew^RecordDBTBL5Q()
 .	Q 
 S KVAR="kill %TAB,VFSN,VO,VPTBL,vtab" S VSID="DBTBL5Q" S VPGM=$T(+0) S VSNAME="QWIK Report Definition"
 S VFSN("DBTBL5Q")="zfDBTBL5Q"
 S vPSL=1
 S KEYS(1)=vobj(fDBTBL5Q,-3)
 S KEYS(2)=vobj(fDBTBL5Q,-4)
 ;
 ; ==================== Display blank screen         (%O=5)
 ;
 I %O=5 D VPR(.fDBTBL5Q) D VDA1(.fDBTBL5Q) D ^DBSPNT() Q 
 ;
 S ER=0 D VSCRPRE(.fDBTBL5Q) I ER Q  ; Screen Pre-Processor
 ;
 I '%O D VNEW(.fDBTBL5Q) D VPR(.fDBTBL5Q) D VDA1(.fDBTBL5Q)
 I %O D VLOD(.fDBTBL5Q) Q:$get(ER)  D VPR(.fDBTBL5Q) D VDA1(.fDBTBL5Q)
 ;
 ; ====================  Display Form
 D ^DBSPNT()
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=XECUTE
 I %O=2!(%O=3) D ^DBSCRT8A XECUTE:'$D(%PAGE) KVAR Q  ; Inquiry/Delete
 ; ====================  Set up data entry control table
 ;
 I %O<2 D VTAB(.fDBTBL5Q)
 Q 
 ;
VNEW(fDBTBL5Q) ; Initialize arrays if %O=0
 ;
 D VLOD(.fDBTBL5Q)
 D VDEF(.fDBTBL5Q)
 D VLOD(.fDBTBL5Q)
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
 Q 
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 ;
VDEF(fDBTBL5Q) ; 
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 Q:(vobj(fDBTBL5Q,-3)="")!(vobj(fDBTBL5Q,-4)="") 
 Q:%O  S ER=0 I (vobj(fDBTBL5Q,-3)="")!(vobj(fDBTBL5Q,-4)="") S ER=1 S RM=$$^MSG(1767,"LIBS,QRID") Q 
  N V1,V2 S V1=vobj(fDBTBL5Q,-3),V2=vobj(fDBTBL5Q,-4) I ($D(^DBTBL(V1,6,V2))) S ER=1 S RM=$$^MSG(2327) Q 
 I $P(vobj(fDBTBL5Q,0),$C(124),12)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"BANNER")) vobj(fDBTBL5Q,-100,0,"BANNER")="L012"_$P(vobj(fDBTBL5Q,0),$C(124),12),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),12)=1
 I $P(vobj(fDBTBL5Q,0),$C(124),14)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"CSCMP")) vobj(fDBTBL5Q,-100,0,"CSCMP")="L014"_$P(vobj(fDBTBL5Q,0),$C(124),14),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),14)=1
 I $P(vobj(fDBTBL5Q,0),$C(124),4)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"DTL")) vobj(fDBTBL5Q,-100,0,"DTL")="L004"_$P(vobj(fDBTBL5Q,0),$C(124),4),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),4)=1
 I $P(vobj(fDBTBL5Q,0),$C(124),3)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"LTD")) vobj(fDBTBL5Q,-100,0,"LTD")="D003"_$P(vobj(fDBTBL5Q,0),$C(124),3),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),3)=+$piece($H,",",1)
 I $P(vobj(fDBTBL5Q,0),$C(124),13)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"MSQL")) vobj(fDBTBL5Q,-100,0,"MSQL")="L013"_$P(vobj(fDBTBL5Q,0),$C(124),13),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),13)=0
 I $P(vobj(fDBTBL5Q,0),$C(124),5)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"RSIZE")) vobj(fDBTBL5Q,-100,0,"RSIZE")="N005"_$P(vobj(fDBTBL5Q,0),$C(124),5),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),5)=80
 I $P(vobj(fDBTBL5Q,0),$C(124),8)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"STAT")) vobj(fDBTBL5Q,-100,0,"STAT")="L008"_$P(vobj(fDBTBL5Q,0),$C(124),8),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),8)=0
 I $P(vobj(fDBTBL5Q,0),$C(124),15)=""  S:'$D(vobj(fDBTBL5Q,-100,0,"UID")) vobj(fDBTBL5Q,-100,0,"UID")="T015"_$P(vobj(fDBTBL5Q,0),$C(124),15),vobj(fDBTBL5Q,-100,0)="" S $P(vobj(fDBTBL5Q,0),$C(124),15)=$$USERNAM^%ZFUNC
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
 Q 
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
VNEWDQ(fDBTBL5Q) ; Original VNEW section
 ;
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
 Q 
VLOD(fDBTBL5Q) ; User defined access section
 ;
 S %REPEAT=8
 Q:%O=0 
 I $get(VFMQ)?1N Q 
 Q 
 ;  #ACCEPT date=11/05/03;pgm=Screen compiler;CR=UNKNOWN;GROUP=SYNTAX
VLODDQ(fDBTBL5Q) ; Original VLOD section
 ;
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 Q 
 ;
VPR(fDBTBL5Q) ; Display screen prompts
 S VO="43||13|0"
 S VO(0)="|0"
 S VO(1)=$C(1,1,80,0,0,0,0,0,0,0)_"11Tlqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqk"
 S VO(2)=$C(2,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(3)=$C(2,4,15,0,0,0,0,0,0,0)_"01TQWIK Report ID:"
 S VO(4)=$C(2,33,5,0,0,0,0,0,0,0)_"01TUser:"
 S VO(5)=$C(2,56,13,0,0,0,0,0,0,0)_"01TLast Updated:"
 S VO(6)=$C(2,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(7)=$C(3,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(8)=$C(3,6,13,1,0,0,0,0,0,0)_"01T Description:"
 S VO(9)=$C(3,61,8,0,52,0,0,0,0,0)_"01TRoutine:"
 S VO(10)=$C(3,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(11)=$C(4,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(12)=$C(4,3,16,1,0,0,0,0,0,0)_"01T Access File(s):"
 S VO(13)=$C(4,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(14)=$C(5,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(15)=$C(5,3,16,0,0,0,0,0,0,0)_"01TReport Order by:"
 S VO(16)=$C(5,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(17)=$C(6,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(18)=$C(6,9,10,0,0,0,0,0,0,0)_"01T Break on:"
 S VO(19)=$C(6,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(20)=$C(7,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(21)=$C(7,35,12,1,0,0,0,0,0,0)_"01T Data Items "
 S VO(22)=$C(7,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(23)=$C(8,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(24)=$C(8,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(25)=$C(9,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(26)=$C(9,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(27)=$C(10,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(28)=$C(10,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(29)=$C(12,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(30)=$C(12,2,19,0,0,0,0,0,0,0)_"01TData Export Option:"
 S VO(31)=$C(12,35,14,0,0,0,0,0,0,0)_"01T Print Detail:"
 S VO(32)=$C(12,60,13,0,0,0,0,0,0,0)_"01T Banner Page:"
 S VO(33)=$C(12,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(34)=$C(13,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(35)=$C(13,9,12,0,0,0,0,0,0,0)_"01T Statistics:"
 S VO(36)=$C(13,54,19,0,0,0,0,0,0,0)_"01T  SQL Query Syntax:"
 S VO(37)=$C(13,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(38)=$C(14,1,30,0,0,0,0,0,0,0)_"11Txqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
 S VO(39)=$C(14,32,19,1,0,0,0,0,0,0)_"01T Query Definitions "
 S VO(40)=$C(14,52,28,0,52,0,0,0,0,0)_"11Tqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
 S VO(41)=$C(14,80,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(42)=$C(15,1,1,0,0,0,0,0,0,0)_"11Tx"
 S VO(43)=$C(15,80,1,0,0,0,0,0,0,0)_"11Tx"
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 Q 
 ;
VDA1(fDBTBL5Q) ; Display screen data
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
  S:'$D(vobj(fDBTBL5Q,12)) vobj(fDBTBL5Q,12)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),12)),1:"")
  S:'$D(vobj(fDBTBL5Q,13)) vobj(fDBTBL5Q,13)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),13)),1:"")
  S:'$D(vobj(fDBTBL5Q,14)) vobj(fDBTBL5Q,14)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),14)),1:"")
  S:'$D(vobj(fDBTBL5Q,11)) vobj(fDBTBL5Q,11)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),11)),1:"")
  S:'$D(vobj(fDBTBL5Q,1)) vobj(fDBTBL5Q,1)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),1)),1:"")
  S:'$D(vobj(fDBTBL5Q,2)) vobj(fDBTBL5Q,2)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),2)),1:"")
  S:'$D(vobj(fDBTBL5Q,3)) vobj(fDBTBL5Q,3)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),3)),1:"")
  S:'$D(vobj(fDBTBL5Q,4)) vobj(fDBTBL5Q,4)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),4)),1:"")
  S:'$D(vobj(fDBTBL5Q,5)) vobj(fDBTBL5Q,5)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),5)),1:"")
  S:'$D(vobj(fDBTBL5Q,6)) vobj(fDBTBL5Q,6)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),6)),1:"")
  S:'$D(vobj(fDBTBL5Q,7)) vobj(fDBTBL5Q,7)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),7)),1:"")
 N V
 ;
 S VO="66|44|13|0"
 S VO(44)=$C(2,20,12,2,0,0,0,0,0,0)_"01T"_$E(vobj(fDBTBL5Q,-4),1,12)
 S VO(45)=$C(2,39,16,2,0,0,0,0,0,0)_"01T"_$E($P(vobj(fDBTBL5Q,0),$C(124),15),1,16)
 S VO(46)=$C(2,70,10,2,0,0,0,0,0,0)_"01D"_$$vdat2str($P(vobj(fDBTBL5Q,0),$C(124),3),"MM/DD/YEAR")
 S VO(47)=$C(3,20,40,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q),$C(124),1),1,40)
 S VO(48)=$C(3,70,8,2,0,0,0,0,0,0)_"01T"_$E($P(vobj(fDBTBL5Q,0),$C(124),2),1,8)
 S VO(49)=$C(4,20,60,2,0,0,0,0,0,0)_"00U"_$E($P(vobj(fDBTBL5Q,0),$C(124),1),1,60)
 S VO(50)=$C(5,20,61,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,0),$C(124),10),1,100)
 S VO(51)=$C(6,20,60,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,0),$C(124),11),1,60)
 S VO(52)=$C(8,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,12),$C(124),1),1,78)
 S VO(53)=$C(9,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,13),$C(124),1),1,78)
 S VO(54)=$C(10,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,14),$C(124),1),1,78)
 S VO(55)=$C(12,22,12,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,11),$C(124),1),1,12)
 S VO(56)=$C(12,50,1,2,0,0,0,0,0,0)_"00L"_$S($P(vobj(fDBTBL5Q,0),$C(124),4):"Y",1:"N")
 S VO(57)=$C(12,74,1,2,0,0,0,0,0,0)_"00L"_$S($P(vobj(fDBTBL5Q,0),$C(124),12):"Y",1:"N")
 S VO(58)=$C(13,22,1,2,0,0,0,0,0,0)_"00L"_$S($P(vobj(fDBTBL5Q,0),$C(124),8):"Y",1:"N")
 S VO(59)=$C(13,74,1,2,0,0,0,0,0,0)_"00L"_$S($P(vobj(fDBTBL5Q,0),$C(124),13):"Y",1:"N")
 S VO(60)=$C(15,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,1),$C(124),1),1,78)
 S VO(61)=$C(16,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,2),$C(124),1),1,78)
 S VO(62)=$C(17,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,3),$C(124),1),1,78)
 S VO(63)=$C(18,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,4),$C(124),1),1,78)
 S VO(64)=$C(19,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,5),$C(124),1),1,78)
 S VO(65)=$C(20,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,6),$C(124),1),1,78)
 S VO(66)=$C(21,2,78,2,0,0,0,0,0,0)_"00T"_$E($P(vobj(fDBTBL5Q,7),$C(124),1),1,78)
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 Q 
 ;
VTAB(fDBTBL5Q) ; 
 ;
 K VSCRPP,REQ,%TAB,%MOD,%MODOFF,%MODGRP,%REPREQ,vtab
 S %MAX=23 S VPT=1 S VPB=21 S PGM=$T(+0) S DLIB="SYSDEV" S DFID="DBTBL5Q"
 S OLNTB=21002
 ;
 S VFSN("DBTBL5Q")="zfDBTBL5Q"
 ;
 ;
 S %TAB(1)=$C(1,19,12)_"21U12402|1|[DBTBL5Q]QRID"
 S %TAB(2)=$C(1,38,16)_"21T12415|1|[DBTBL5Q]UID|||||||||20"
 S %TAB(3)=$C(1,69,10)_"21D12403|1|[DBTBL5Q]LTD"
 S %TAB(4)=$C(2,19,40)_"01T12401|1|[DBTBL5Q]DESC||||do VP1^V00S074(.fDBTBL5Q)"
 S %TAB(5)=$C(2,69,8)_"20U12402|1|[DBTBL5Q]PGM"
 S %TAB(6)=$C(3,19,60)_"01U12401|1|[DBTBL5Q]PFID|[DBTBL1]:LIST||do VP2^V00S074(.fDBTBL5Q)"
 S %TAB(7)=$C(4,19,61)_"00T12410|1|[DBTBL5Q]ORDERBY|||do VP3^V00S074(.fDBTBL5Q)|do VP4^V00S074(.fDBTBL5Q)|||||100"
 S %TAB(8)=$C(5,19,60)_"00T12411|1|[DBTBL5Q]BREAKON|||do VP5^V00S074(.fDBTBL5Q)"
 S %TAB(9)=$C(7,1,78)_"01T12401|1|[DBTBL5Q]FLD1|||do VP6^V00S074(.fDBTBL5Q)|do VP7^V00S074(.fDBTBL5Q)"
 S %TAB(10)=$C(8,1,78)_"00T12401|1|[DBTBL5Q]FLD2|||do VP8^V00S074(.fDBTBL5Q)|do VP9^V00S074(.fDBTBL5Q)"
 S %TAB(11)=$C(9,1,78)_"00T12401|1|[DBTBL5Q]FLD3|||do VP10^V00S074(.fDBTBL5Q)|do VP11^V00S074(.fDBTBL5Q)"
 S %TAB(12)=$C(11,21,12)_"00T12401|1|[DBTBL5Q]TRANS|[STBLTFMT]||do VP12^V00S074(.fDBTBL5Q)"
 S %TAB(13)=$C(11,49,1)_"00L12404|1|[DBTBL5Q]DTL"
 S %TAB(14)=$C(11,73,1)_"00L12412|1|[DBTBL5Q]BANNER"
 S %TAB(15)=$C(12,21,1)_"00L12408|1|[DBTBL5Q]STAT|||do VP13^V00S074(.fDBTBL5Q)"
 S %TAB(16)=$C(12,73,1)_"00L12413|1|[DBTBL5Q]MSQL|||do VP14^V00S074(.fDBTBL5Q)"
 S %TAB(17)=$C(14,1,78)_"00T12401|1|[DBTBL5Q]QID1|||do VP15^V00S074(.fDBTBL5Q)"
 S %TAB(18)=$C(15,1,78)_"00T12401|1|[DBTBL5Q]QID2|||do VP16^V00S074(.fDBTBL5Q)"
 S %TAB(19)=$C(16,1,78)_"00T12401|1|[DBTBL5Q]QID3|||do VP17^V00S074(.fDBTBL5Q)"
 S %TAB(20)=$C(17,1,78)_"00T12401|1|[DBTBL5Q]QID4|||do VP18^V00S074(.fDBTBL5Q)"
 S %TAB(21)=$C(18,1,78)_"00T12401|1|[DBTBL5Q]QID5|||do VP19^V00S074(.fDBTBL5Q)"
 S %TAB(22)=$C(19,1,78)_"00T12401|1|[DBTBL5Q]QID6|||do VP20^V00S074(.fDBTBL5Q)"
 S %TAB(23)=$C(20,1,78)_"00T12401|1|[DBTBL5Q]QID7|||do VP21^V00S074(.fDBTBL5Q)"
 D VTBL(.fDBTBL5Q) D VDEPRE(.fDBTBL5Q) I $get(ER) Q 
 D ^DBSCRT8 ; data entry
 Q 
 ;
VREQ ; Create REQ() array
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 Q 
 ;
VTBL(fDBTBL5Q) ; Create %TAB(array)
 ; 1 2 3  4 5   6   7-9 10-11
 ; DY,DX,SZ PT REQ TYPE DEL POS |NODE|ITEM NAME|TBL|FMT|PP|PRE|MIN|MAX|DEC
 ;
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 Q 
 ;
VDEPRE(fDBTBL5Q) ; Data Entry Pre-processor
 ;
 I (ZQRUN="") Q 
 ;
 ; Run mode
 ;
 ; ====== Protect ACCESS FILES and DATA ITEMS prompts
 ;
 D PROTECT^DBSMACRO("DBTBL5Q.DESC")
 D PROTECT^DBSMACRO("DBTBL5Q.PFID")
 D PROTECT^DBSMACRO("DBTBL5Q.ORDERBY")
 D PROTECT^DBSMACRO("DBTBL5Q.BREAKON")
 D PROTECT^DBSMACRO("DBTBL5Q.FLD1")
 D PROTECT^DBSMACRO("DBTBL5Q.FLD2")
 D PROTECT^DBSMACRO("DBTBL5Q.FLD3")
 D PROTECT^DBSMACRO("DBTBL5Q.FLD4")
 D PROTECT^DBSMACRO("DBTBL5Q.FLD5")
 D PROTECT^DBSMACRO("DBTBL5Q.STAT")
 D PROTECT^DBSMACRO("DBTBL5Q.TRANS")
 D PROTECT^DBSMACRO("DBTBL5Q.DTL")
 D PROTECT^DBSMACRO("DBTBL5Q.BANNER")
 D PROTECT^DBSMACRO("DBTBL5Q.CNTREC")
 Q 
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 ;
 ;user-defined post procs
 ;
VP1(fDBTBL5Q) ; 
 N ZMSG
 I '(QRID="") Q 
 ;
 S ZMSG=$$^MSG(7289)
 D DEFAULT^DBSMACRO("DBTBL5Q.DESC",ZMSG,"1","0","0")
 ;
 Q 
 ;
VP2(fDBTBL5Q) ; 
 N I
 ;
 S FILES=X
 S DLIB="SYSDEV"
 S DFID=$piece(X,",",1)
 S X=""
 ;
 D ^DBSDI
 Q:ER 
 ;
 S ZQFID=FID
 S X=FILES
 ;
 D ^DBSFVER
 ;
 I '$get(ER) D
 .	S FID=$piece(FILES,",",1)
 .	S PFID=$piece(FILES,",",1)
 .	Q 
 ;
 F  Q:X'?.E1","  S X=$E(X,1,$L(X)-1)
 ;
 I $get(ER) Q 
 ;
 I %O Q 
 ;
 Q 
 ;
VP3(fDBTBL5Q) ; 
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 ;
 N J N Z
 N FL N Y N ZA N ZB N ZZORD
 ;
 S I(3)=""
 ;
 I $get(X)="" Q 
 ;
 I X=V Q 
 ;
 I $L(X,",")>7 D  Q 
 .	S ER=1
 .	S RM=$$^MSG(1695)
 .	Q 
 ;
 S ER=0
 S Z=$L(X,",")
 ;
 F J=1:1:Z I $piece(X,",",J)="" S ER=1 Q 
 ;
 I $get(ER) S RM=$$^MSG(1475) Q 
 ;
 S FL=$piece($P(vobj(fDBTBL5Q,0),$C(124),1),",",1)
 ;
 N db1,vop1,vop2,vop3 S vop1="SYSDEV",vop2=FL,db1=$$vRCgetRecord0Opt^RecordDBTBL1("SYSDEV",FL,0,"")
  S vop3=$G(^DBTBL(vop1,1,vop2,16))
 ;
 I $P(vop3,$C(124),1)=X D  Q 
 .	S ER=1
 .	S RM=$$^MSG(3049,X)
 .	Q 
 ;
 D ORDERBY^DBSEXEQ6($P(vobj(fDBTBL5Q,0),$C(124),1),X,.ZZORD)
 ;
 Q:ER 
 ;
 S X=""
 F J=1:1 D  Q:Y="" 
 .	S Y=$piece(ZZORD,"|",J)
 .	Q:Y="" 
 .	S ZB=$piece(Y,"]",2)
 .	S Y="["_$piece(Y,",",2)
 .	I ($D(ZA(ZB))#2) S ER=1 S RM=$$^MSG(871,Y) S Y="" Q 
 .	S ZA(ZB)=ZB
 .	;
 .	I '((ZB?1A.AN)!(Z?1"%".AN)) Q  ; Skip dummy key
 .	;
 .	I ($D(VORDER(ZB))#2) S X=X_Y_"/DESC"_","
 .	E  S X=X_Y_","
 .	Q 
 ;
 Q:ER 
 ;
 S X=$E(X,1,$L(X)-1)
 S V="*****************************************"
 ;
 Q 
VP4(fDBTBL5Q) ; 
 S I(3)="@SELDI^DBSFUN(FILES,vSTR):LIST"
 Q 
VP5(fDBTBL5Q) ; 
 N i N j
 N DI N ZORD N ZORDDI
 ;
 Q:(X="") 
 ;
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 S ZORD=$P(vobj(fDBTBL5Q,0),$C(124),10)
 ;
 ; No ORDER BY specified, default from table's keys
 I (ZORD="") D ZZORD(.fDBTBL5Q,.ZORD)
 ;
 ; Check to see if only column name was entered
 F i=1:1:$L(X,",") D
 .	S DI=$piece(X,",",i)
 .	;
 .	; Already in [TABLE]COLUMN format
 .	I (DI["[") Q 
 .	;
 .	; Not in [TABLE]COLUMN format
 .	F j=1:1:$L(ZORD,",") D
 ..		S ZORDDI=$piece(ZORD,",",j)
 ..		I $piece(ZORDDI,"]",2)=DI S DI=ZORDDI
 ..		Q 
 .	; Replace original entry with [TABLE]COLUMN format
 .	S $piece(X,",",i)=DI
 .	Q 
 ;
 ; Select name from list: ~p1
 I '((","_ZORD_",")[(","_X_",")) D SETERR^DBSEXECU("DBTBL5Q","MSG",2467,ZORD) Q:ER 
 ;
 Q 
 ;
 ; Default to access keys from the primary file
 ;
ZZORD(fDBTBL5Q,ZORD) ; 
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 ;
 N i N len
 N KEYS N PFID
 ;
 S PFID=$P(vobj(fDBTBL5Q,0),$C(124),1)
 S PFID=$piece(PFID,",",1)
 N db1,vop1,vop2,vop3 S vop1="SYSDEV",vop2=PFID,db1=$$vRCgetRecord0Opt^RecordDBTBL1("SYSDEV",PFID,0,"")
  S vop3=$G(^DBTBL(vop1,1,vop2,16))
 ;
 S KEYS=$P(vop3,$C(124),1)
 S ZORD=""
 ;
 S len=$L(KEYS,",")
 ;
 F i=1:1:len D
 .	I $$isLit^UCGM($piece(KEYS,",",i)) Q 
 .	S ZORD=ZORD_"["_PFID_"]"_$piece(KEYS,",",i)_","
 .	Q 
 ;
 ; Remove trailing comma
 S ZORD=$E(ZORD,1,$L(ZORD)-1)
 ;
 Q 
VP6(fDBTBL5Q) ; 
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 ;
 N Z2
 N BK N Z N Z1
 ;
 S Z=$P(vobj(fDBTBL5Q,0),$C(124),11)
 I X'="",Z'="" D
 .	S Z1=$piece(Z,"]",2)
 .	S BK=$piece(Z1,"/",1)
 .	F Z2=1:1 S Z1=$piece(X,",",Z2) Q:Z1=""  D
 ..		I Z1["]" S Z1=$piece(Z1,"]",2)
 ..		I Z1=$get(BK) S BK=""
 ..		Q 
 .	I '($get(BK)="") S ER=1 S RM=$$^MSG(1757,Z)
 .	Q 
 ;
 Q:ER 
 ;
 I '(X=""),(FILES[","),($D(zztblfid)#2),'(zztblfid=""),'(X[","),'($E(X,1)="[") S X="["_zztblfid_"]"_X
 ;
 D PPDI^DBSEXEQ(.X,FID,FILES,.I,.RM,.ER)
 ;
 Q:ER 
 ;
 I X?1"@".E D DELETE^DBSMACRO("DBTBL5Q.TRANS")
 I X?1"@".E D GOTO^DBSMACRO("DBTBL5Q.MSQL")
 ;
 Q 
VP7(fDBTBL5Q) ; 
 S I(3)="@SELDI^DBSFUN(FILES,vSTR):LIST"
 Q 
VP8(fDBTBL5Q) ; 
 ;
 I '(X=""),(FILES[","),($D(zztblfid)#2),'(zztblfid=""),'(X[","),'($E(X,1)="[") S X="["_zztblfid_"]"_X
 ;
 D PPDI^DBSEXEQ(.X,FID,FILES,.I,.RM,.ER)
 Q 
VP9(fDBTBL5Q) ; 
 S I(3)="@SELDI^DBSFUN(FILES,vSTR):LIST"
 Q 
VP10(fDBTBL5Q) ; 
 ;
 I '(X=""),(FILES[","),($D(zztblfid)#2),'(zztblfid=""),'(X[","),'($E(X,1)="[") S X="["_zztblfid_"]"_X
 ;
 D PPDI^DBSEXEQ(.X,FID,FILES,.I,.RM,.ER)
 Q 
VP11(fDBTBL5Q) ; 
 S I(3)="@SELDI^DBSFUN(FILES,vSTR):LIST"
 Q 
VP12(fDBTBL5Q) ; 
 I X="" Q 
 ;
 I '($D(^DBCTL("SYS","TFMT",X))#2) Q 
 ;
 D DEFAULT^DBSMACRO("DBTBL5Q.BANNER",0,"1","0","0")
 ;
 ; SELECT PRINT DETAIL OPTION
 D GOTO^DBSMACRO("DBTBL5Q.CNTREC")
 ;
 Q 
VP13(fDBTBL5Q) ; 
 ;
 ; RUN mode ?
 I $get(ZQRUN) S UX=1 Q 
 ;
 ; Format page only
 I 'X D  Q 
 .	S %PAGE=2
 .	S %PG=1
 .	Q 
 ;
 ; Access stat screen
 S %PAGE=3
 S %PG=1
 ;
 Q 
VP14(fDBTBL5Q) ; 
  S:'$D(vobj(fDBTBL5Q,1)) vobj(fDBTBL5Q,1)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),1)),1:"")
  S:'$D(vobj(fDBTBL5Q,2)) vobj(fDBTBL5Q,2)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),2)),1:"")
  S:'$D(vobj(fDBTBL5Q,3)) vobj(fDBTBL5Q,3)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),3)),1:"")
  S:'$D(vobj(fDBTBL5Q,4)) vobj(fDBTBL5Q,4)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),4)),1:"")
  S:'$D(vobj(fDBTBL5Q,5)) vobj(fDBTBL5Q,5)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),5)),1:"")
  S:'$D(vobj(fDBTBL5Q,6)) vobj(fDBTBL5Q,6)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),6)),1:"")
  S:'$D(vobj(fDBTBL5Q,7)) vobj(fDBTBL5Q,7)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),7)),1:"")
  S:'$D(vobj(fDBTBL5Q,8)) vobj(fDBTBL5Q,8)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),8)),1:"")
  S:'$D(vobj(fDBTBL5Q,0)) vobj(fDBTBL5Q,0)=$S(vobj(fDBTBL5Q,-2):$G(^DBTBL(vobj(fDBTBL5Q,-3),6,vobj(fDBTBL5Q,-4),0)),1:"")
 ; Convert DATA-QWIK query syntax into PROFILE/SQL syntax
 ;
 N i
 N pfid N qry N WHERE
 ;
 I 'V,X
 E  Q 
 ;
 ; Changed from DQ to SQL synatx
 ;
 S qry(1)=$P(vobj(fDBTBL5Q,1),$C(124),1)
 S qry(2)=$P(vobj(fDBTBL5Q,2),$C(124),1)
 S qry(3)=$P(vobj(fDBTBL5Q,3),$C(124),1)
 S qry(4)=$P(vobj(fDBTBL5Q,4),$C(124),1)
 S qry(5)=$P(vobj(fDBTBL5Q,5),$C(124),1)
 S qry(6)=$P(vobj(fDBTBL5Q,6),$C(124),1)
 S qry(7)=$P(vobj(fDBTBL5Q,7),$C(124),1)
 S qry(8)=$P(vobj(fDBTBL5Q,8),$C(124),1)
 ;
 S pfid=$P(vobj(fDBTBL5Q,0),$C(124),1) ; Primary file
 ;
 S WHERE=$$WHERE^SQLCONV(.qry,pfid) ; SQL WHERE statement
 I WHERE="" Q 
 ;
 F i=1:1:8 S qry(i)="" ; Delete old syntax
 ;
 D CONVLN^SQLCONV(WHERE,78,.qry) ; New syntax
 ;
  S vobj(fDBTBL5Q,-100,1)="" S $P(vobj(fDBTBL5Q,1),$C(124),1)=qry(1)
  S vobj(fDBTBL5Q,-100,2)="" S $P(vobj(fDBTBL5Q,2),$C(124),1)=qry(2)
  S vobj(fDBTBL5Q,-100,3)="" S $P(vobj(fDBTBL5Q,3),$C(124),1)=qry(3)
  S vobj(fDBTBL5Q,-100,4)="" S $P(vobj(fDBTBL5Q,4),$C(124),1)=qry(4)
  S vobj(fDBTBL5Q,-100,5)="" S $P(vobj(fDBTBL5Q,5),$C(124),1)=qry(5)
  S vobj(fDBTBL5Q,-100,6)="" S $P(vobj(fDBTBL5Q,6),$C(124),1)=qry(6)
  S vobj(fDBTBL5Q,-100,7)="" S $P(vobj(fDBTBL5Q,7),$C(124),1)=qry(7)
  S vobj(fDBTBL5Q,-100,8)="" S $P(vobj(fDBTBL5Q,8),$C(124),1)=qry(8)
 ;
 D DISPLAY^DBSMACRO("ALL")
 D GOTO^DBSMACRO("END")
 ;
 Q 
VP15(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP16(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP17(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP18(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP19(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP20(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
VP21(fDBTBL5Q) ; 
 N FILES N PFID N ZX
 ;
 S FILES=""
 S PFID=""
 ;
 D PPQ^DBSEXEQ(fDBTBL5Q,.FILES,.PFID,.X,.RM,.ER)
 ;
 Q 
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 ;
VRV(V,L) ; 
 Q V_$J("",L-$L(V))
VREPRNT ; 
 D VPR(.fDBTBL5Q)
 D VDA1(.fDBTBL5Q)
 D ^DBSPNT()
 Q 
 ;
VW(fDBTBL5Q) ; 
 D VDA1(.fDBTBL5Q)
 D ^DBSPNT(10)
 Q 
 ;
VDAPNT(fDBTBL5Q) ; 
 D VDA1(.fDBTBL5Q)
 D ^DBSPNT(0,2)
 Q 
 ;
VDA ; 
 D VDA1(.fDBTBL5Q)
 Q 
 ;
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 ;
vSET(sn,di,X) ; 
 I sn="DBTBL5Q" D vSET1(.fDBTBL5Q,di,X)
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
 Q 
vSET1(fDBTBL5Q,di,X) ; 
  D propSet^DBSDYNRA(fDBTBL5Q,di,X,1,0)
 ;  #ACCEPT Date=11/5/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
 Q 
 ;
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
vREAD(fid,di) ; 
 I fid="DBTBL5Q" Q $$vREAD1(.fDBTBL5Q,di)
 Q ""
vREAD1(fDBTBL5Q,di) ; 
 Q $$propGet^DBSDYNRA(fDBTBL5Q,di)
 ;  #ACCEPT DATE=11/05/03; PGM=Screen Compiler;CR=UNKNOWN;GROUP=DEAD
 ;
VSCRPRE(fDBTBL5Q) ; Screen Pre-Processor
 N %TAB,vtab ; Disable .MACRO. references to %TAB()
 ;
 S LIBS="SYSDEV"
 Q 
 ;  #ACCEPT date=11/05/03;PGM=Screen Compiler;CR=UNKNOWN;GROUP=SYNTAX
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
