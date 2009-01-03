/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef BAG_H_INCLUDED
#define BAG_H_INCLUDED

#include "core.h"
#include "object.h"

sharedbag_slot_t* lookup_sharedbag(scm_sharedbag_t bag, const char* key, int len);

#endif
