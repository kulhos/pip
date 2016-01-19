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
$gtm_dist/mupip load -for=zwr /pip/gbls/pip02.zwr
$gtm_dist/mumps -r %XCMD 'set ^CUVAR(2)=+$h,$p(^SCAU(1,1),"|",7)=$h+1000'
