#!/bin/sh
ENV=${1}

. ${ENV}/gtmenv > /dev/null
[ -f ${gtmgbldir} ] && rm -f ${gtmgbldir}
$gtm_dist/mumps -r ^GDE <<-EOF
@${ENV}/scripts/pip.gde
exit
EOF

$gtm_dist/mupip create
