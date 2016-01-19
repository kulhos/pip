#!/bin/sh
ENV=${1}

. ${ENV}/gtmenv > /dev/null
[ -f ${gtmgbldir} ] && rm -f ${gtmgbldir}

$gtm_dist/mumps -r ^GDE <<-EOF
@${ENV}/scripts/pip.gde
exit
EOF

[ -f ${PIP_DB} ] && rm -f ${PIP_DB}
$gtm_dist/mupip create
$gtm_dist/mupip load -for=zwr /pip/gbls/pip02.zwr
