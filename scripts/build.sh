#!/bin/sh
ENV=${1}

. ${ENV}/gtmenv > /dev/null
[ -f ${gtmgbldir} ] && rm -vf ${gtmgbldir}

$gtm_dist/mumps -r ^GDE <<-EOF
@${ENV}/scripts/pip.gde
exit
EOF

[ -f ${PIP_DB} ] && rm -vf ${PIP_DB}
$gtm_dist/mupip create
$gtm_dist/mumps -r %XCMD 'set ^CUVAR(2)=+$h,$p(^SCAU(1,1),"|",7)=$h+1000'

cnt=0
for file in ${DIR}/data/*.G
 do
       #echo "Loading file ${file}"
       $gtm_dist/mumps -r %XCMD "do READFILE^TBXINST(.code,\"${file}\") for i=1:1 quit:'\$d(code(i))  set @code(i)"
	cnt=$(($cnt+1))
done

echo "\n$cnt Globals loaded."

cnt=0
for file in ${DIR}/dataqwik/table/*/*.TBL
 do
       #echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXTBL(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt tables loaded."

cnt=0
for file in ${DIR}/dataqwik/table/*/*.COL
 do
       #echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXCOL(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt columns loaded."

cnt=0
for file in ${DIR}/dataqwik/trigger/*.TRIG
 do
       #echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXTRIG(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt triggers loaded."

cnt=0
for file in ${DIR}/dataqwik/index/*.IDX
 do
       #echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXIDX(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt indexes loaded."

cnt=0
for file in ${DIR}/dataqwik/foreign_key/*.FKY
 do
       #echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXFKEY(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt foreign keys loaded."

cnt=0
for file in ${DIR}/data/*.DAT
 do
       # echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXDATA(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt data files loaded."

cnt=0
for file in ${DIR}/dataqwik/procedure/*.PROC
 do
       # echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXPROC(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt procedures loaded."

cnt=0
for file in ${DIR}/dataqwik/screen/*.SCR
 do
       # echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXSCRN(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt screens loaded."

cnt=0
for file in ${DIR}/dataqwik/pre_post_lib/*.PPL
 do
       # echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXPPL(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt processors loaded."


cnt=0
for file in ${DIR}/dataqwik/report/*.RPT
 do
       # echo "Loading file ${file}"
	fname=$(basename ${file})
       $gtm_dist/mumps -r %XCMD \
	"do READFILE^TBXINST(.code,\"${file}\") s x=\$\$LOAD^TBXRPT(.code,\"${fname}\",3,\"$(whoami)\",+\$H)"
	cnt=$(($cnt+1))
done
echo "\n$cnt reports loaded."

echo "Recompiling procedures"
$gtm_dist/mumps -r %XCMD "do BUILDALL^DBSPROC"

echo "Recompiling screens"
$gtm_dist/mumps -r %XCMD "do BUILDALL^DBSDSMC"

echo "Recompiling filers"
$gtm_dist/mumps -r %XCMD "do BUILDALL^DBSFILB"

