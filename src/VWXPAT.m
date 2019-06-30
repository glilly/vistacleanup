VWXPAT ;gpl - patient data utilities ;2019-06-15  3:27 PM
 ;;0.1;VW PATIENT DATA UTILS;;Feb 07, 2019;Build 34
 ;
 ; Authored by George P. Lilly 2019
 ;
 q
 ;
 ; DO NOT USE THIS ROUTINE IF THE SYSTEM CONTAINS REAL PATIENT DATA
 ; DELETE THIS ROUTINE FROM THE VISTA SYSTEM IF THERE IS REAL PATIENT DATA
 ; YOU MAY MESS UP YOUR VISTA SYSTEM BY USING THIS ROUTINE
 ; USER BEWARE
 ;
PFILES(ZIX,PRINT) ; IF PRINT=1 the pointers found in ^DD to 2 will be printed 
 N ZPT,ZIHS
 S ZPT=$NA(^DD(2,0,"PT"))
 S ZIHS=$NA(^DD(9000001,0,"PT"))
 M ZIX=@ZPT
 M ZIX=@ZIHS
 S ZIX(63)="" ; LABDATA
 S ZIX(63.9999)="" ; LABDATA
 S ZIX(41.9)="" ; CENSUS
 n in s in="^DD"
 n patfn
 f patfn=2,90000001 d  ;
 . n i s i=$q(@in@(""))
 . f  s i=$q(@i) q:i=""  d  ;
 . . q:$ql(i)>3
 . . q:$qs(i,3)'=0
 . . q:+$qs(i,1)=0
 . . n zp s zp=$p(@i,"^",2)
 . . n zpn s zpn="P"_patfn
 . . q:zp'[zpn
 . . q:zp=""
 . . n zp1 s zp1=$p(zp,"P"_patfn,2)
 . . q:$e(zp1,1)=0
 . . q:+$e(zp1,1)>0
 . . i $g(PRINT)=1 w !,i," ",@i
 . . n zf s zf=$qs(i,1)
 . . i $g(^DIC(zf,0,"GL"))="" d  ;
 . . . w !,"NO GL node: ",zf," ",$o(^DD(zf,0,"NM",""))
 . . . n zup,zgl,zpre,zf1
 . . . s zf1=zf
 . . . s zgl=""
 . . . s zpre=""
 . . . f  s zup=$g(^DD(zf1,0,"UP")) q:zup=""  q:zgl'=""  d  ;
 . . . . s zf1=zup
 . . . . s zgl=$g(^DIC(zf1,0,"GL"))
 . . . . s zpre="Parent's ("_zf1_") "_zpre
 . . . i zgl'="" w !,"  ",zpre," GL: ",zgl,!,"     ",@(zgl_"0)")
 . . q:+$d(ZIX(zf))
 . . w !,"Not found in PT: ",i," ",@i
 q
 ;
BUILD ; LOOK AT WHAT PATIENT DATA EXISTS - BACK UP TO ^BAK
 N ZIX
 D PFILES(.ZIX)
 K ^BAK
 N ZI S ZI=0
 F  S ZI=$O(ZIX(ZI)) Q:+ZI=0  D  ;
 . Q:'$D(^DIC(ZI))
 . N ZGL S ZGL=$G(^DIC(ZI,0,"GL"))
 . I ZGL="" D  Q  ;
 . . W !,"SKIPPING - NO GL: ",ZI
 . N ZZRO1 S ZZRO1=ZGL_"0)"
 . N ZZRO S ZZRO=$G(@ZZRO1)
 . N ZZRO2 S ZZRO2=ZGL
 . I $E(ZZRO2,$L(ZZRO2))="(" S ZZRO2=$E(ZZRO2,1,$L(ZZRO2)-1)
 . I $E(ZZRO2,$L(ZZRO2))="," S ZZRO2=$E(ZZRO2,1,$L(ZZRO2)-1)_")"
 . ;Q:$O(@ZZRO2@(0))=""
 . W !,"FILE: ",ZI," GLOBAL: ",ZGL," ZERO NODE: ",ZZRO
 . M ^BAK(ZI,ZZRO2)=@ZZRO2
 Q
 ;
BAK(OUT) ; EXIT FROM SYNVPR
 ; USAGE: mfilter=BAK^VWXPAT in the url
 K ^GPL("mfilter")
 M ^GPL("mfilter")=@OUT
 Q:'$D(@OUT)
 N ZZI S ZZI=0
 F  S ZZI=$O(@OUT@(ZZI)) Q:+ZZI=0  D  ;
 . N LN S LN=$G(@OUT@(ZZI))
 . Q:$E(LN,1,7)'="|  |--^"
 . ;N GL S GL=$P($P(LN," ",1),"|  |--",2)
 . N GL S GL=$P($P(LN,"|  |--",2)," ",1)
 . N TX S TX=$P(LN,"|  |--",2)
 . N FN S FN=+$P(@OUT@(ZZI-1),"|--",2)
 . S @OUT@(ZZI)="|  |--<a href=/gtree/BAK("_FN_","""_GL_""")?level=1&mfilter=BAK2^VWXPAT >"_TX_"</a>"
 Q
 ;
BAK2(OUT) ; second exit from SYNVPR
 ; puts fmx links into record lines
 Q:'$D(@OUT)
 N FILE S FILE=""
 N ZZI S ZZI=0
 F  S ZZI=$O(@OUT@(ZZI)) Q:+ZZI=0  D  ;
 . N LN S LN=$G(@OUT@(ZZI))
 . I $E(LN,1,4)="^BAK" D  Q  ;
 . . S FILE=+$P($P(LN,"^BAK(",2),",",1)
 . Q:$E(LN,1,4)="|--0"
 . Q:$E(LN,1,3)'="|--"
 . N TX S TX=$P(LN,"|--",2)
 . N IEN S IEN=+$P(TX," ",1)
 . Q:IEN=0
 . ;Q:FILE=""
 . S @OUT@(ZZI)="|--<a href=/fmx?file="_FILE_"&ien="_IEN_" >"_TX_"</a>"
 . ;S @OUT@(ZZI)="|--<a href=/gtree/BAK("_FN_","""_GL_""")?level=1 >"_TX_"</a>"
 q
 ;
wsPATFIL(rtn,filter) ; web service for /patientfiles
 s filter("level")=2
 s filter("mfilter")="BAK^VWXPAT"
 s filter("root")="BAK"
 d wsGtree^SYNVPR(.rtn,.filter)
 q
 ;
wsPATFN(rtn,filter) ; web service for the list of patient file numbers
 n zi s zi=0
 n cnt s cnt=0
 n gn s gn=$na(^BAK)
 f  s zi=$o(@gn@(zi)) q:+zi=0  d  ;
 . s cnt=cnt+1
 . s rtn(cnt)=zi
 S rtn="<!DOCTYPE HTML><html><head></head><body><pre>"
 S rtn($O(rtn(""),-1)+1)="</pre></body></html>"
 D ADDCRLF^%webutils(.rtn)
 S HTTPRSP("mime")="text/html"
 q
 ;
BAKFLS ; writes to screen all the file numbers in ^BAK
 ;
 n zi s zi=0
 f  s zi=$o(^BAK(zi)) q:+zi=0  d  ;
 . w !,zi
 q
 ;
listm(out,in)   ; out is passed by name in is passed by reference
 n i s i=$q(@in@(""))
 f  s i=$q(@i) q:i=""  d  ;
 . ;d oneout^SYNVPR(out,i_"="_@i)
 . ;i $qs(i,3)=0 i @i["P2" w !,i," ",@i
 . ;i $qs(i,3)=0 w !,i," ",@i
 . q:$ql(i)>3
 . q:$qs(i,3)'=0
 . q:+$qs(i,1)=0
 . n zp s zp=$p(@i,"^",2)
 . q:zp'["P2"
 . q:zp=""
 . n zp1 s zp1=$p(zp,"P2",2)
 . q:$e(zp1,1)=0
 . q:+$e(zp1,1)>0
 . w !,i," ",@i
 q
 ;
