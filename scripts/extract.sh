#!/bin/sh

bipdir=${1}
extrdir=${2}

[ "${extrdir}" = "" ] && extrdir=${bipdir}

if [ ! -f ${bipdir}/gtmenv ]
 then
	echo "${bipdir}/gtmenv file not found"
	exit 1
fi

. ${bipdir}/gtmenv

$gtm_dist/mumps -r %XCMD "D TYPEINIT^TBXINST(.TYPES),EXTDATA^TBXEXTR(\"${bipdir}\"),EXTDQW^TBXEXTR(\"${bipdir}\")"

