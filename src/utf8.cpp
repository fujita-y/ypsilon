/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "heap.h"
#include "utf8.h"

int
ascii_cstring_pred(const char* s)
{
	uint8_t c;
	while ((c = *s++) != 0) {
		if (c > 0x7f) return 0;
	}
	return 1;
}

int
cnvt_ucs4_to_utf8(uint32_t ucs4, uint8_t utf8[4])
{
    if (ucs4 >= 0xd800 && ucs4 <= 0xdfff) {
        fatal("cnvt_ucs4_to_utf8() excluded range: %x", ucs4);
    }
	if (ucs4 < 0x80) {
		utf8[0] = ucs4;
		return 1;
	}
	if (ucs4 < 0x800) {
		utf8[0] = ((ucs4 >>  6) & 0x1f) | 0xc0;
		utf8[1] = ((ucs4      ) & 0x3f) | 0x80;
		return 2;
	}
	if (ucs4 < 0x10000) {
		utf8[0] = ((ucs4 >> 12) & 0x0f) | 0xe0;
		utf8[1] = ((ucs4 >>  6) & 0x3f) | 0x80;
		utf8[2] = ((ucs4      ) & 0x3f) | 0x80;
		return 3;
	}
	if (ucs4 < 0x200000) {
		utf8[0] = ((ucs4 >> 18) & 0x07) | 0xf0;
		utf8[1] = ((ucs4 >> 12) & 0x3f) | 0x80;
		utf8[2] = ((ucs4 >>  6) & 0x3f) | 0x80;
		utf8[3] = ((ucs4      ) & 0x3f) | 0x80;
		return 4;
	}
	fatal("cnvt_ucs4_to_utf8() out of range: %x", ucs4);
}

int
cnvt_utf8_to_ucs4(const uint8_t utf8[4], uint32_t* ucs4)
{
    int sv;
	if (utf8[0] < 0x80) {
		sv = utf8[0];
        if (sv >= 0x80) return -1;                          // invalid sequence
        *ucs4 = sv;
        return 1;
    } else if (utf8[0] < 0xc0) {
        return -1;                                          // invalid sequence
	} else if (utf8[0] < 0xe0) {
        sv = ((utf8[0] & 0x1f) << 6) + (utf8[1] & 0x3f);
        if (sv >= 0x800) return -1;                         // invalid sequence
        *ucs4 = sv;
        return 2;
	} else if (utf8[0] < 0xf0) {
        sv = ((utf8[0] & 0x0f) << 12) + ((utf8[1] & 0x3f) << 6) + (utf8[2] & 0x3f);
        if ((sv >= 0xD800) & (sv <= 0xDFFF)) return -1;     // SURROGATE AREA
        if (sv >= 0x10000) return -1;                       // invalid sequence
        // if (sv >= 0xFFFE) return -1;                     // NONCHARACTERS
        *ucs4 = sv;
        return 3;
	} else  if (utf8[0] < 0xf8) {
        sv = ((utf8[0] & 0x07) << 18) + ((utf8[1] & 0x3f) << 12) + ((utf8[2] & 0x3f) << 6) + (utf8[3] & 0x3f);
        if (sv > 0x10FFFF) return -1;                       // non-assignment
        *ucs4 = sv;
        return 4;
    }
    return -1;
}

bool
string_eq_pred(scm_obj_t obj1, scm_obj_t obj2)
{
	if (STRINGP(obj1)) {
		if (STRINGP(obj2)) {
			scm_string_t string1 = (scm_string_t)obj1;
			scm_string_t string2 = (scm_string_t)obj2;
			int size1 = HDR_STRING_SIZE(string1->hdr);
			int size2 = HDR_STRING_SIZE(string2->hdr);
			return (size1 == size2) && (memcmp(string1->name, string2->name, size1) == 0);
		}
	}
	return false;
}

int
string_compare(scm_obj_t obj1, scm_obj_t obj2)
{
	if (STRINGP(obj1)) {
		if (STRINGP(obj2)) {
			scm_string_t string1 = (scm_string_t)obj1;
			scm_string_t string2 = (scm_string_t)obj2;
			return strcmp(string1->name, string2->name);
        }
	}
	return false;
}

bool
string_ci_eq_pred(scm_obj_t obj1, scm_obj_t obj2)
{
	if (STRINGP(obj1)) {
		if (STRINGP(obj2)) {
			scm_string_t string1 = (scm_string_t)obj1;
			scm_string_t string2 = (scm_string_t)obj2;
			int size1 = HDR_STRING_SIZE(string1->hdr);
			int size2 = HDR_STRING_SIZE(string2->hdr);
			if (size1 == size2) {
				for (int i = 0; i < size1; i++) {
					if (toupper(string1->name[i]) == toupper(string2->name[i])) continue;
					return false;
				}
				return true;
			}
		}
	}
	return false;
}

int	utf8_sizeof_ucs4(uint32_t ucs4)
{
	if (ucs4 < 0x80) return 1;
	if (ucs4 < 0x800) return 2;
	if (ucs4 < 0x10000) return 3;
	if (ucs4 < 0x200000) return 4;
	fatal("utf8_sizeof_ucs4() out of range");
}

int
utf8_byte_count(const uint8_t datum)
{
	if (datum < 0x80) return 1;
    if (datum < 0xc0) return 1; // cnvt_utf8_to_ucs4() detect this
	if (datum < 0xe0) return 2;
	if (datum < 0xf0) return 3;
	if (datum < 0xf8) return 4;
	if (datum < 0xfc) return 5;
	return 6;
}

int
utf8_char_index_to_byte_offset(const uint8_t datum[], int index, int limit)
{
	int n = 0;
	for (int c = 0; c < index && n < limit; c++) n += utf8_byte_count(datum[n]);
	if (n >= limit) return -1;
	return n;
}

int
utf8_string_length(scm_string_t obj)
{
	uint8_t* datum = (uint8_t*)obj->name;
	int end = HDR_STRING_SIZE(obj->hdr);
	int c = 0;
	for (int n = 0; n < end; c++) n += utf8_byte_count(datum[n]);
	return c;
}

void
utf8_substring(scm_string_t obj, int from, int to, int* head, int* tail)
{
	uint8_t* datum = (uint8_t*)obj->name;
	int end = HDR_STRING_SIZE(obj->hdr);
    assert(from <= end);
    assert(to <= end);
    assert(from <= to);
	*head = end;
	*tail = end;
    int n = 0;
    while (n < end) {
        if (from == 0) *head = n;
        if (to == 0) {
            *tail = n;
            return;
        }
        from = from - 1;
        to = to - 1;
        n += utf8_byte_count(datum[n]);
    }
}

int
utf8_string_ref(scm_string_t obj, int index)
{
	uint8_t* datum = (uint8_t*)obj->name;
	int end = HDR_STRING_SIZE(obj->hdr);
	int offset = utf8_char_index_to_byte_offset(datum, index, end);
	if (offset < 0) return -1;
	uint32_t ucs4;
	if (cnvt_utf8_to_ucs4(datum + offset, &ucs4) < 1) return -1;
	return ucs4;
}

bool
utf8_string_set(object_heap_t* heap, scm_string_t obj, int index, int ch)
{
	uint8_t* datum = (uint8_t*)obj->name;
	int limit = heap->allocated_size(datum);
	int size_prev = HDR_STRING_SIZE(obj->hdr);
	int offset = utf8_char_index_to_byte_offset(datum, index, (limit < size_prev) ? limit : size_prev);
	if (offset < 0) return false;
	uint8_t utf8[4];
	int n_new = cnvt_ucs4_to_utf8(ch, utf8);
	int n_prev = utf8_byte_count(datum[offset]);
	if (n_new == n_prev) {
		for (int i = 0; i < n_new; i++) datum[offset + i] = utf8[i];
		return true;
	}
	if (n_new < n_prev) {
		for (int i = 0; i < n_new; i++) datum[offset + i] = utf8[i];
		memmove(datum + offset + n_new, datum + offset + n_prev, size_prev - offset - n_prev);
		int size_new = size_prev + n_new - n_prev;
		obj->hdr = scm_hdr_string | (size_new << HDR_STRING_SIZE_SHIFT);
		datum[size_new] = 0;
		return true;
	}
	int size_new = size_prev + n_new - n_prev;
	if (limit > size_new) {
		memmove(datum + offset + n_new, datum + offset + n_prev, size_prev - offset - n_prev);
		for (int i = 0; i < n_new; i++) datum[offset + i] = utf8[i];
		obj->hdr = scm_hdr_string | (size_new << HDR_STRING_SIZE_SHIFT);
		datum[size_new] = 0;
		return true;	
	}
	uint8_t* datum2 = (uint8_t*)heap->allocate_private(size_new + 1);
	memcpy(datum2, datum, offset);
	for (int i = 0; i < n_new; i++) datum2[offset + i] = utf8[i];
	memcpy(datum2 + offset + n_new, datum + offset + n_prev, size_prev - offset - n_prev);
	datum2[size_new] = 0;
	uint8_t* prev = (uint8_t*)obj->name;
	obj->name = (char*)datum2;
	obj->hdr = scm_hdr_string | (size_new << HDR_STRING_SIZE_SHIFT);
	heap->deallocate_private(prev);
	return true;
}










