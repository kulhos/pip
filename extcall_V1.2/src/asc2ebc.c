/*
*	asc2ebc.c 
*
*	Copyright(c)2000 Sanchez Computer Associates, Inc.
*	All Rights Reserved
*
*	UNIX:	Harsha Lakshmikantha 01 March 2000
*
*	DESC:	External Call from MUMPS to translate between ASCII and EBCDIC 
*
* $Id: $
*
* $Log:	asc2ebc.c,v $
 * Revision 1.2  00/03/03  10:45:52  10:45:52  lyh ()
 * Replaced value for ` (grave character)
 * 
 * Revision 1.1  00/03/03  10:28:19  10:28:19  lyh ()
 * Initial revision
 * 
*
* $Revision: 1.2 $
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extcall.h"


static char asc_ebc_table[]={
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
            0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
            0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D, 0x4D,
            0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
            0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
            0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
            0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8,
            0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
            0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
            0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,
            0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88,
            0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
            0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
            0xA8, 0xA9, 0xC0, 0x6A, 0xD0, 0xA1, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B,
            0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B, 0x4B } ;


void
asc2ebc(int count,STR_DESCRIPTOR *str_d,STR_DESCRIPTOR *str_d2,SLONG *rc)
{
	int i;
	char *str = (char *)NULL;
	char *ptr = (char *)NULL;

	if((str_d->str == (char *)NULL) || (str_d->length <= 0))
	{
		/*
		*	if Null String was passed
		*/
		str_d->length=0;
		*rc = MUMPS_FAILURE;
		return;
	}

	if((str = (char *)malloc(str_d->length)) == (char *)NULL)
	{
		*rc = MUMPS_FAILURE;
		return;
	}
	(void)memset(str,'\0',str_d->length);
	ptr = str;

	for(i=0;i<str_d->length;i++)
	{
		*ptr++ = asc_ebc_table[str_d->str[i]];
	}

	(void)memcpy(str_d2->str,str,str_d->length);
	str_d2->length = str_d->length;

	(void)free(str);

   	*rc = MUMPS_SUCCESS;
	return;
}

/* void
asc2ebc(int count,STR_DESCRIPTOR *str_d,SLONG *rc)
{
	int i;
	char *str = (char *)NULL;
	char *ptr = (char *)NULL;

	if((str_d->str == (char *)NULL) || (str_d->length <= 0))
	{ */
		/*
		*	if Null String was passed
		*/
	/*	str_d->length=0;
		*rc = MUMPS_FAILURE;
		return;
	}

	if((str = (char *)malloc(str_d->length)) == (char *)NULL)
	{
		*rc = MUMPS_FAILURE;
		return;
	}
	(void)memset(str,'\0',str_d->length);
	ptr = str;

	for(i=0;i<str_d->length;i++)
	{
		*ptr++ = asc_ebc_table[str_d->str[i]];
	}

	(void)memcpy(str_d->str,str,str_d->length);

	(void)free(str);

   	*rc = MUMPS_SUCCESS;
	return;
} */
