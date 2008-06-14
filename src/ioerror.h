/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/


#ifndef	IOERROR_H_INCLUDED
#define	IOERROR_H_INCLUDED

#include "core.h"
#include "object.h"

void raise_io_error(VM* vm, const char* who, int operation, const char* message, int err, scm_obj_t port, scm_obj_t filename);
void raise_io_codec_error(VM* vm, const char* who, int operation, const char* message, scm_obj_t port, scm_obj_t ch);

#endif
