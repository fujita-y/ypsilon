/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "ucs4.h"

bool
ucs4_intraline_whitespace(int c) // http://www.unicode.org/Public/UNIDATA/PropList.txt
{
    if (c == 0x0020) return true;                   //; White_Space # Zs       SPACE
    if (c == 0x0009) return true;                   //; White_Space # Cc   [5] <control-0009>
    if (c <= 0x007F) return false;
    if (c >= 0x2000 && c <= 0x200a) return true;    //; White_Space # Zs  [11] EN QUAD..HAIR SPACE
    switch (c) {
        case 0x00A0:    //; White_Space # Zs       NO-BREAK SPACE
        case 0x1680:    //; White_Space # Zs       OGHAM SPACE MARK
        case 0x180E:    //; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
        case 0x202F:    //; White_Space # Zs       NARROW NO-BREAK SPACE
        case 0x205F:    //; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
        case 0x3000:    //; White_Space # Zs       IDEOGRAPHIC SPACE
            return true;
    }
    return false;
}
