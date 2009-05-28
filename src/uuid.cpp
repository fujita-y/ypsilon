/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "uuid.h"

/*
  Reference: A Universally Unique IDentifier (UUID) URN Namespace
             http://www.ietf.org/rfc/rfc4122.txt

      UUID                   = time-low "-" time-mid "-"
                               time-high-and-version "-"
                               clock-seq-and-reserved
                               clock-seq-low "-" node
      time-low               = 4hexOctet
      time-mid               = 2hexOctet
      time-high-and-version  = 2hexOctet
      clock-seq-and-reserved = hexOctet
      clock-seq-low          = hexOctet
      node                   = 6hexOctet
      hexOctet               = hexDigit hexDigit
      hexDigit =
            "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" /
            "a" / "b" / "c" / "d" / "e" / "f" /
            "A" / "B" / "C" / "D" / "E" / "F"
*/

static uint32_t rand8()
{
    uint32_t a = random();
    return (a & 0xff);
}

static uint32_t rand16()
{
    uint32_t a = random();
    uint32_t b = random();
    return ((a & 0xff) << 8) + (b & 0xff);
}

static uint32_t rand32()
{
    uint32_t a = random();
    uint32_t b = random();
    uint32_t c = random();
    return ((a & 0x3ff) << 22) + ((b & 0x7ff) << 11) + (c & 0x7ff);
}

static uint64_t rand48()
{
    uint64_t a = random();
    uint64_t b = random();
    uint64_t c = random();
    uint64_t d = random();
    return ((a & 0xfff) << 36) + ((b & 0xfff) << 24) + ((c & 0xfff) << 12) + (d & 0xfff);
}

void
uuid_v4(char* buf, int bufsize)             // version 4
{
    assert(bufsize > 36);
    uint32_t time_low;                      // octet[0-3]
    uint16_t time_mid;                      // octet[4-5]
    uint16_t time_hi_and_version;           // octet[6-7]
    uint8_t clock_seq_hi_and_reserved;      // octet[8]
    uint8_t clock_seq_lo;                   // octet[9]
    uint64_t node;                          // octed[10-15]
    time_low = rand32();
    time_mid = rand16();
    time_hi_and_version = rand16();
    clock_seq_hi_and_reserved = rand8();
    clock_seq_lo = rand8();
    node = rand48();
    uint16_t version = 0x4000;              // octet[6] 0100 xxxx octet[7] xxxx xxxx
    time_hi_and_version = version + (time_hi_and_version & 0xfff);
    uint8_t variant = 0x80;                 // octet[8] 100x xxxx
    clock_seq_hi_and_reserved = variant + (clock_seq_hi_and_reserved & 0x1f);
    snprintf(buf,
             bufsize,
             "%08x-%04x-%04x-%02x%02x-%04x%08x",
             time_low,
             time_mid,
             time_hi_and_version,
             clock_seq_hi_and_reserved,
             clock_seq_lo,
             (uint32_t)(node >> 32),
             (uint32_t)node);
}
