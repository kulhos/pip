 ; 
 ; **** Routine compiled from DATA-QWIK Procedure DBARCHIVE1 ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  DBARCHIVE2 Builder|DBARCHIVE1|||||||1
DBARCHIVE1 ; 
 ;
 N pslcode
 ;
 D addcode(.pslcode,"private DBARCHIVE2(String ARCHDIR, Number ARCHNUM, Date THRUDATE, String ARCHTBL, String KEYVALS())"_$char(9)_"// Call RecordTABLE.archive() to archive data")
 D addcode(.pslcode," // Last compiled:  "_%CurrentDate_" "_$$TIM^%ZM_" - "_%UserName)
 D addcode(.pslcode,"")
 D addcode(.pslcode," // THIS IS A COMPILED ROUTINE.  Compiled by procedure DBARCHIVE1")
 D addcode(.pslcode,"")
 D addcode(.pslcode," // See DBARCHIVE1 for argument definitions")
 D addcode(.pslcode,"")
 D addcode(.pslcode," type Number retVal = 0")
 D addcode(.pslcode,"")
 ;
 N rs,vos1,vos2,vos3 S rs=$$vOpen1()
 ;
 F  Q:'$$vFetch1()  D
 .	;
 .	N i N KEYCNT
 .	N ARCHTBL N code
 .	;
 . S ARCHTBL=rs
 .	;
 .	N tblDes S tblDes=$$getPslTbl^UCXDD(ARCHTBL,0)
 .	;
 .	S KEYCNT=$$getArchiveKey^DBARCHIVE(tblDes,0)
 .	;
 .	D addcode(.pslcode," if (ARCHTBL = """_ARCHTBL_""") do {")
 .	;
 .	D addcode(.pslcode,"  type static Record"_ARCHTBL)
 .	;
 .	; If archive key is first key, KEYVALS will not be passed as it
 .	; is not used
 .	S code="set retVal = Record"_ARCHTBL_".archive(ARCHDIR, ARCHNUM, THRUDATE"
 .	F i=1:1:KEYCNT-1 S code=code_", KEYVALS("_i_")"
 .	S code=code_")"
 .	;
 .	D addcode(.pslcode,"  "_code)
 .	D addcode(.pslcode," }")
 .	Q 
 ;
 D addcode(.pslcode,"")
 D addcode(.pslcode," quit retVal")
 ;
 ; Build compiled routine
 D cmpA2F^UCGM(.pslcode,"DBARCHIVE2")
 ;
 Q 
 ;
addcode(pslcode,code) ; Code to insert into array
 ;
 N LINENO
 ;
 I ($E(code,1)=" ") S code=$char(9)_$E(code,2,1048575)
 ;
 S LINENO=$order(pslcode(""),-1)+1 ; Add to end
 S pslcode(LINENO)=code
 ;
 Q 
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^2573" ; Signature - LTD^TIME^USER^SIZE
 ;
vOpen1() ; ARCHTBL FROM DBUTARCHIVE ORDER BY ARCHTBL ASC
 ;
 ;
 S vos1=2
 D vL1a1
 Q ""
 ;
vL1a0 S vos1=0 Q
vL1a1 S vos2=$$BYTECHAR^SQLUTL(254)
 S vos3=""
vL1a3 S vos3=$O(^UTBL("DBARCHIVE",vos3),1) I vos3="" G vL1a0
 Q
 ;
vFetch1() ;
 ;
 ;
 I vos1=1 D vL1a3
 I vos1=2 S vos1=1
 ;
 I vos1=0 S rs="" Q 0
 ;
 S rs=$S(vos3=vos2:"",1:vos3)
 ;
 Q 1
