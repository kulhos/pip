 ; 
 ; **** Routine compiled from DATA-QWIK Procedure DBSRWBNR ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  DATA-QWIK Report Banner Print|DBSRWBNR|||||||1
DBSRWBNR(IO,BNRINFO) ; 
 ;
 N N
 N ALLEQLS N DESC N LINE N RID N TABLOC
 ;
 S DESC=$get(BNRINFO("DESC"))
 ;
 S ALLEQLS=""
 S $piece(ALLEQLS,"=",$L(DESC)+1)="" ; ===== line
 S TABLOC=(75-$L(DESC))\2 ; Tab location
 S LINE=""
 S $piece(LINE,"-",71)="" ; ----- line
 ;
 ; Description
 USE IO WRITE !,?TABLOC,ALLEQLS,!,?TABLOC,DESC,!,?TABLOC,ALLEQLS,!!!
 ;
 ; Documentation
 I $order(BNRINFO("DOC","")) D
 .	N I N N
 .	;
 .	S N=""
 .	WRITE !?5,LINE
 .	F I=1:1 S N=$order(BNRINFO("DOC",N)) Q:(N="")  WRITE !?5,BNRINFO("DOC",N)
 .	WRITE !?5,LINE,!
 .	I I>21 WRITE $char(12),! ; New page
 .	Q 
 ;
 WRITE !,"              User: ",%UserID
 WRITE ?45,"Run: ",%CurrentDate,"  ",$$TIM^%ZM,!
 I $get(%SystemDate) WRITE ?42,"System: ",%SystemDate,!
 WRITE !
 ;
 S RID=BNRINFO("RID")
 I $E(RID,1,5)="QWIK_" D  ; QWIK report
 .	WRITE "       QWIK Report: "
 .	WRITE $E(RID,6,1048575)
 .	Q 
 E  WRITE "            Report: ",RID
 ;
 WRITE ?41,"Program: ",$get(BNRINFO("PGM"))
 ;
 ; File(s):
 WRITE !!,$J($$^MSG(3479),20),$get(BNRINFO("TABLES"))
 ;
 WRITE !!,"          Order By: "
 D
 .	N N S N=""
 .	N X
 .	;
 .	F  S N=$order(BNRINFO("ORDERBY",N)) Q:(N="")  D
 ..		S X=BNRINFO("ORDERBY",N)
 ..		WRITE ?20,$$DES^DBSDD(X),!
 ..		Q 
 .	Q 
 ;
 I $D(BNRINFO("PROMPTS")) D
 .	N N S N=""
 .	N X
 .	;
 .	WRITE !!,"             Input: "
 .	F  S N=$order(BNRINFO("PROMPTS",N)) Q:(N="")  D
 ..		S X=BNRINFO("PROMPTS",N)
 ..		WRITE ?20,$piece(X,"|",2)," "
 ..		WRITE $piece(X,"|",4,99),!
 ..		Q 
 .	Q 
 ;
 ; Break WHERE clause into 60 character chunks to pring
 WRITE !!,?5,$E(LINE,1,31)," WHERE ",$E(LINE,1,32),!!
 I '($get(BNRINFO("WHERE"))="") D
 .	N I
 .	N X
 .	;
 .	S X=BNRINFO("WHERE")
 .	F  D  Q:(X="") 
 ..		I $L(X)<60 D
 ...			WRITE ?10,X,!
 ...			S X=""
 ...			Q 
 ..		E  D
 ...			F I=60:-1:1 Q:$E(X,I)=" " 
 ...			WRITE ?10,$E(X,1,I),!
 ...			S X=$E(X,I+1,1048575)
 ...			Q 
 ..		Q 
 .	Q 
 E  WRITE ?32,"No WHERE clause",!
 ;
 WRITE !,?5,LINE,!!
 ;
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^3007" ; Signature - LTD^TIME^USER^SIZE
