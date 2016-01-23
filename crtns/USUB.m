 ; 
 ; **** Routine compiled from DATA-QWIK Procedure USUB ****
 ; 
 ;  0.000000000000000000000000 - 
 ; 
 ;DO NOT MODIFY  Sub Field Get/Put Utility|USUB|||||||1
USUB ; 
 Q 
 ;
GET(V,sft,sfd1,sfd2,sfp) ; Get a value (substring) from a string
 ;
 I $get(sft)="",$get(sfd1)="",$get(sfd2)="" Q $E(V,sfp)
 I '$get(sfp) S sfp=1
 ;
 I sft="" Q $P(V,sfd1,sfp)
 ;
 I $get(sfd2)="" S sfd2=sfd1
 ;
 Q $P($P($P(sfd1_V,(sfd1_sft_sfd2),2),sfd1,1),sfd2,sfp)
 ;
PUT(V,X,sft,sfd1,sfd2,sfp) ; Poke a value X into a string V
 ;
 N field
 ;
 I '$get(sfp) S sfp=1
 ;
 I sft="" S $P(V,sfd1,sfp)=X Q $$STRIP(V,sfd1)
 ;
 I $get(sfd2)="" S sfd2=sfd1
 I V="" S $P(V,sfd2,sfp)=X Q sft_sfd2_V
 ;
 S field=$P($P(sfd1_V,(sfd1_sft_sfd2),2),sfd1,1)
 I field'="" D
 .	N z
 .	S z=sfd1_sft_sfd2_field
 .	S V=$P(sfd1_V,z,1)_$P(sfd1_V,z,2)
 .	I $E(V,1)=sfd1 S V=$E(V,2,1048575)
 .	Q 
 S $P(field,sfd2,sfp)=X
 ;
 I V="" Q sft_sfd2_field
 ;
 Q V_sfd1_sft_sfd2_field
 ;
STRIP(V,D) ; Strip trailing D's from V
 ;
 F  Q:$E(V,$L(V))'=D  S V=$E(V,1,$L(V)-1)
 ;
 Q V
 ;
PARSE(expr,value,mode) ; Change $$GET^USUB format into SET $P format
 ;
 N cnt,del,exp,pos,z,zexp
 ;
 S cnt=$L(expr,",")
 S pos=$P(expr,",",cnt)+0
 S del=$P(expr,",",cnt-2)
 S exp=$P(expr,",",1,cnt-4)
 S exp=$P(exp,"(",2,99)
 S z=$P(exp,",",1)
 S zexp=$P(z,"(",1)_"($G("_$P(z,"(",2,99)_")"_","_$P(exp,",",2,99)
 ;
 ; I18N=OFF
 ; create
 Q " S v="_zexp_",$P(v,"_del_","_pos_")="_value_","_exp_"=v"
 ; I18N=ON
 ;  #OPTION ResultClass ON
vSIG() ; 
 Q "^^^1659" ; Signature - LTD^TIME^USER^SIZE
