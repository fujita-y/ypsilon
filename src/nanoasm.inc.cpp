/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

void nanoasm::add(const reg8_t& dst, const reg8_t& src) {
    DB(0x00); MOD(src, dst);
}
void nanoasm::add(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x00); MOD(src, dst);
}
void nanoasm::add(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x00); MOD(src, dst);
}
void nanoasm::add(const reg32_t& dst, const reg32_t& src) {
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x01); MOD(src, dst);
}
void nanoasm::add(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x02); MOD(dst, src);
}
void nanoasm::add(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x02); MOD(dst, src);
}
void nanoasm::add(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x03); MOD(dst, src);
}
void nanoasm::add(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x03); MOD(dst, src);
}
void nanoasm::add(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x03); MOD(dst, src);
}
void nanoasm::add(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x03); MOD(dst, src);
}
void nanoasm::add(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x04); DB(imm8);
    } else {
        DB(0x80); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::add(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x05); DD(imm32);
        } else {
            DB(0x81); MOD(0, dst); DD(imm32);
        }
    }
}
void nanoasm::add(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x05); DD(imm32);
        } else {
            DB(0x81); MOD(0, dst); DD(imm32);
        }
    }
}
void nanoasm::add(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(0, dst); DB(imm8);
}
void nanoasm::add(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(0, dst); DB(imm8);
}
void nanoasm::add(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        DB(0x81); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::add(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        DB(0x81); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::add(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        DB(0x81); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::add(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(0, dst); DB(imm32);
    } else {
        DB(0x81); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::or_(const reg8_t& dst, const reg8_t& src) {
    DB(0x08); MOD(src, dst);
}
void nanoasm::or_(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x08); MOD(src, dst);
}
void nanoasm::or_(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x08); MOD(src, dst);
}
void nanoasm::or_(const reg32_t& dst, const reg32_t& src) {
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x09); MOD(src, dst);
}
void nanoasm::or_(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x0A); MOD(dst, src);
}
void nanoasm::or_(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x0A); MOD(dst, src);
}
void nanoasm::or_(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0B); MOD(dst, src);
}
void nanoasm::or_(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0B); MOD(dst, src);
}
void nanoasm::or_(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0B); MOD(dst, src);
}
void nanoasm::or_(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0B); MOD(dst, src);
}
void nanoasm::or_(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x0C); DB(imm8);
    } else {
        DB(0x80); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::or_(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x0D); DD(imm32);
        } else {
            DB(0x81); MOD(1, dst); DD(imm32);
        }
    }
}
void nanoasm::or_(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x0D); DD(imm32);
        } else {
            DB(0x81); MOD(1, dst); DD(imm32);
        }
    }
}
void nanoasm::or_(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(1, dst); DB(imm8);
}
void nanoasm::or_(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(1, dst); DB(imm8);
}
void nanoasm::or_(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        DB(0x81); MOD(1, dst); DD(imm32);
    }
}
void nanoasm::or_(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        DB(0x81); MOD(1, dst); DD(imm32);
    }
}
void nanoasm::or_(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        DB(0x81); MOD(1, dst); DD(imm32);
    }
}
void nanoasm::or_(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(1, dst); DB(imm32);
    } else {
        DB(0x81); MOD(1, dst); DD(imm32);
    }
}
void nanoasm::adc(const reg8_t& dst, const reg8_t& src) {
    DB(0x10); MOD(src, dst);
}
void nanoasm::adc(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x10); MOD(src, dst);
}
void nanoasm::adc(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x10); MOD(src, dst);
}
void nanoasm::adc(const reg32_t& dst, const reg32_t& src) {
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x11); MOD(src, dst);
}
void nanoasm::adc(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x12); MOD(dst, src);
}
void nanoasm::adc(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x12); MOD(dst, src);
}
void nanoasm::adc(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x13); MOD(dst, src);
}
void nanoasm::adc(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x13); MOD(dst, src);
}
void nanoasm::adc(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x13); MOD(dst, src);
}
void nanoasm::adc(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x13); MOD(dst, src);
}
void nanoasm::adc(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x14); DB(imm8);
    } else {
        DB(0x80); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::adc(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x15); DD(imm32);
        } else {
            DB(0x81); MOD(2, dst); DD(imm32);
        }
    }
}
void nanoasm::adc(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x15); DD(imm32);
        } else {
            DB(0x81); MOD(2, dst); DD(imm32);
        }
    }
}
void nanoasm::adc(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(2, dst); DB(imm8);
}
void nanoasm::adc(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(2, dst); DB(imm8);
}
void nanoasm::adc(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        DB(0x81); MOD(2, dst); DD(imm32);
    }
}
void nanoasm::adc(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        DB(0x81); MOD(2, dst); DD(imm32);
    }
}
void nanoasm::adc(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        DB(0x81); MOD(2, dst); DD(imm32);
    }
}
void nanoasm::adc(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(2, dst); DB(imm32);
    } else {
        DB(0x81); MOD(2, dst); DD(imm32);
    }
}
void nanoasm::sbb(const reg8_t& dst, const reg8_t& src) {
    DB(0x18); MOD(src, dst);
}
void nanoasm::sbb(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x18); MOD(src, dst);
}
void nanoasm::sbb(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x18); MOD(src, dst);
}
void nanoasm::sbb(const reg32_t& dst, const reg32_t& src) {
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x19); MOD(src, dst);
}
void nanoasm::sbb(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x1A); MOD(dst, src);
}
void nanoasm::sbb(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x1A); MOD(dst, src);
}
void nanoasm::sbb(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x1B); MOD(dst, src);
}
void nanoasm::sbb(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x1B); MOD(dst, src);
}
void nanoasm::sbb(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x1B); MOD(dst, src);
}
void nanoasm::sbb(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x1B); MOD(dst, src);
}
void nanoasm::sbb(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x1C); DB(imm8);
    } else {
        DB(0x80); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::sbb(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x1D); DD(imm32);
        } else {
            DB(0x81); MOD(3, dst); DD(imm32);
        }
    }
}
void nanoasm::sbb(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x1D); DD(imm32);
        } else {
            DB(0x81); MOD(3, dst); DD(imm32);
        }
    }
}
void nanoasm::sbb(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(3, dst); DB(imm8);
}
void nanoasm::sbb(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(3, dst); DB(imm8);
}
void nanoasm::sbb(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        DB(0x81); MOD(3, dst); DD(imm32);
    }
}
void nanoasm::sbb(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        DB(0x81); MOD(3, dst); DD(imm32);
    }
}
void nanoasm::sbb(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        DB(0x81); MOD(3, dst); DD(imm32);
    }
}
void nanoasm::sbb(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(3, dst); DB(imm32);
    } else {
        DB(0x81); MOD(3, dst); DD(imm32);
    }
}
void nanoasm::and_(const reg8_t& dst, const reg8_t& src) {
    DB(0x20); MOD(src, dst);
}
void nanoasm::and_(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x20); MOD(src, dst);
}
void nanoasm::and_(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x20); MOD(src, dst);
}
void nanoasm::and_(const reg32_t& dst, const reg32_t& src) {
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x21); MOD(src, dst);
}
void nanoasm::and_(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x22); MOD(dst, src);
}
void nanoasm::and_(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x22); MOD(dst, src);
}
void nanoasm::and_(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x23); MOD(dst, src);
}
void nanoasm::and_(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x23); MOD(dst, src);
}
void nanoasm::and_(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x23); MOD(dst, src);
}
void nanoasm::and_(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x23); MOD(dst, src);
}
void nanoasm::and_(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x24); DB(imm8);
    } else {
        DB(0x80); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::and_(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x25); DD(imm32);
        } else {
            DB(0x81); MOD(4, dst); DD(imm32);
        }
    }
}
void nanoasm::and_(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x25); DD(imm32);
        } else {
            DB(0x81); MOD(4, dst); DD(imm32);
        }
    }
}
void nanoasm::and_(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(4, dst); DB(imm8);
}
void nanoasm::and_(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(4, dst); DB(imm8);
}
void nanoasm::and_(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        DB(0x81); MOD(4, dst); DD(imm32);
    }
}
void nanoasm::and_(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        DB(0x81); MOD(4, dst); DD(imm32);
    }
}
void nanoasm::and_(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        DB(0x81); MOD(4, dst); DD(imm32);
    }
}
void nanoasm::and_(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(4, dst); DB(imm32);
    } else {
        DB(0x81); MOD(4, dst); DD(imm32);
    }
}
void nanoasm::sub(const reg8_t& dst, const reg8_t& src) {
    DB(0x28); MOD(src, dst);
}
void nanoasm::sub(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x28); MOD(src, dst);
}
void nanoasm::sub(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x28); MOD(src, dst);
}
void nanoasm::sub(const reg32_t& dst, const reg32_t& src) {
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x29); MOD(src, dst);
}
void nanoasm::sub(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x2A); MOD(dst, src);
}
void nanoasm::sub(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x2A); MOD(dst, src);
}
void nanoasm::sub(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x2B); MOD(dst, src);
}
void nanoasm::sub(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x2B); MOD(dst, src);
}
void nanoasm::sub(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x2B); MOD(dst, src);
}
void nanoasm::sub(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x2B); MOD(dst, src);
}
void nanoasm::sub(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x2C); DB(imm8);
    } else {
        DB(0x80); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::sub(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x2D); DD(imm32);
        } else {
            DB(0x81); MOD(5, dst); DD(imm32);
        }
    }
}
void nanoasm::sub(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x2D); DD(imm32);
        } else {
            DB(0x81); MOD(5, dst); DD(imm32);
        }
    }
}
void nanoasm::sub(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(5, dst); DB(imm8);
}
void nanoasm::sub(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(5, dst); DB(imm8);
}
void nanoasm::sub(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        DB(0x81); MOD(5, dst); DD(imm32);
    }
}
void nanoasm::sub(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        DB(0x81); MOD(5, dst); DD(imm32);
    }
}
void nanoasm::sub(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        DB(0x81); MOD(5, dst); DD(imm32);
    }
}
void nanoasm::sub(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(5, dst); DB(imm32);
    } else {
        DB(0x81); MOD(5, dst); DD(imm32);
    }
}
void nanoasm::xor_(const reg8_t& dst, const reg8_t& src) {
    DB(0x30); MOD(src, dst);
}
void nanoasm::xor_(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x30); MOD(src, dst);
}
void nanoasm::xor_(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x30); MOD(src, dst);
}
void nanoasm::xor_(const reg32_t& dst, const reg32_t& src) {
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x31); MOD(src, dst);
}
void nanoasm::xor_(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x32); MOD(dst, src);
}
void nanoasm::xor_(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x32); MOD(dst, src);
}
void nanoasm::xor_(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x33); MOD(dst, src);
}
void nanoasm::xor_(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x33); MOD(dst, src);
}
void nanoasm::xor_(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x33); MOD(dst, src);
}
void nanoasm::xor_(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x33); MOD(dst, src);
}
void nanoasm::xor_(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x34); DB(imm8);
    } else {
        DB(0x80); MOD(6, dst); DB(imm8);
    }
}
void nanoasm::xor_(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x35); DD(imm32);
        } else {
            DB(0x81); MOD(6, dst); DD(imm32);
        }
    }
}
void nanoasm::xor_(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x35); DD(imm32);
        } else {
            DB(0x81); MOD(6, dst); DD(imm32);
        }
    }
}
void nanoasm::xor_(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(6, dst); DB(imm8);
}
void nanoasm::xor_(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(6, dst); DB(imm8);
}
void nanoasm::xor_(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        DB(0x81); MOD(6, dst); DD(imm32);
    }
}
void nanoasm::xor_(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        DB(0x81); MOD(6, dst); DD(imm32);
    }
}
void nanoasm::xor_(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        DB(0x81); MOD(6, dst); DD(imm32);
    }
}
void nanoasm::xor_(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(6, dst); DB(imm32);
    } else {
        DB(0x81); MOD(6, dst); DD(imm32);
    }
}
void nanoasm::cmp(const reg8_t& dst, const reg8_t& src) {
    DB(0x38); MOD(src, dst);
}
void nanoasm::cmp(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x38); MOD(src, dst);
}
void nanoasm::cmp(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x38); MOD(src, dst);
}
void nanoasm::cmp(const reg32_t& dst, const reg32_t& src) {
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x39); MOD(src, dst);
}
void nanoasm::cmp(const reg8_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x3A); MOD(dst, src);
}
void nanoasm::cmp(const reg8_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x3A); MOD(dst, src);
}
void nanoasm::cmp(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x3B); MOD(dst, src);
}
void nanoasm::cmp(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x3B); MOD(dst, src);
}
void nanoasm::cmp(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x3B); MOD(dst, src);
}
void nanoasm::cmp(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x3B); MOD(dst, src);
}
void nanoasm::cmp(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0x3C); DB(imm8);
    } else {
        DB(0x80); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::cmp(const reg32_t& dst, int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x3D); DD(imm32);
        } else {
            DB(0x81); MOD(7, dst); DD(imm32);
        }
    }
}
void nanoasm::cmp(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        if (dst.m_regcode == regcode_rax) {
            DB(0x3D); DD(imm32);
        } else {
            DB(0x81); MOD(7, dst); DD(imm32);
        }
    }
}
void nanoasm::cmp(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(7, dst); DB(imm8);
}
void nanoasm::cmp(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0x80); MOD(7, dst); DB(imm8);
}
void nanoasm::cmp(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        DB(0x81); MOD(7, dst); DD(imm32);
    }
}
void nanoasm::cmp(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        DB(0x81); MOD(7, dst); DD(imm32);
    }
}
void nanoasm::cmp(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        DB(0x81); MOD(7, dst); DD(imm32);
    }
}
void nanoasm::cmp(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x83); MOD(7, dst); DB(imm32);
    } else {
        DB(0x81); MOD(7, dst); DD(imm32);
    }
}
void nanoasm::rol(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(0, dst);
    } else {
        DB(0xC0); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(0, dst);
    } else {
        DB(0xC0); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(0, dst);
    } else {
        DB(0xC0); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(0, dst);
    } else {
        DB(0xC1); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::rol(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(0, dst);
}
void nanoasm::rol(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(0, dst);
}
void nanoasm::rol(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(0, dst);
}
void nanoasm::rol(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(0, dst);
}
void nanoasm::rol(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(0, dst);
}
void nanoasm::rol(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(0, dst);
}
void nanoasm::rol(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(0, dst);
}
void nanoasm::rol(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(0, dst);
}
void nanoasm::rol(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(0, dst);
}
void nanoasm::ror(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(1, dst);
    } else {
        DB(0xC0); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(1, dst);
    } else {
        DB(0xC0); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(1, dst);
    } else {
        DB(0xC0); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(1, dst);
    } else {
        DB(0xC1); MOD(1, dst); DB(imm8);
    }
}
void nanoasm::ror(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(1, dst);
}
void nanoasm::ror(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(1, dst);
}
void nanoasm::ror(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(1, dst);
}
void nanoasm::ror(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(1, dst);
}
void nanoasm::ror(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(1, dst);
}
void nanoasm::ror(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(1, dst);
}
void nanoasm::ror(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(1, dst);
}
void nanoasm::ror(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(1, dst);
}
void nanoasm::ror(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(1, dst);
}
void nanoasm::rcl(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(2, dst);
    } else {
        DB(0xC0); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(2, dst);
    } else {
        DB(0xC0); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(2, dst);
    } else {
        DB(0xC0); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(2, dst);
    } else {
        DB(0xC1); MOD(2, dst); DB(imm8);
    }
}
void nanoasm::rcl(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(2, dst);
}
void nanoasm::rcl(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(2, dst);
}
void nanoasm::rcl(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(2, dst);
}
void nanoasm::rcl(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcl(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcl(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcl(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcl(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcl(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(2, dst);
}
void nanoasm::rcr(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(3, dst);
    } else {
        DB(0xC0); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(3, dst);
    } else {
        DB(0xC0); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(3, dst);
    } else {
        DB(0xC0); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(3, dst);
    } else {
        DB(0xC1); MOD(3, dst); DB(imm8);
    }
}
void nanoasm::rcr(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(3, dst);
}
void nanoasm::rcr(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(3, dst);
}
void nanoasm::rcr(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(3, dst);
}
void nanoasm::rcr(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(3, dst);
}
void nanoasm::rcr(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(3, dst);
}
void nanoasm::rcr(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(3, dst);
}
void nanoasm::rcr(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(3, dst);
}
void nanoasm::rcr(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(3, dst);
}
void nanoasm::rcr(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(3, dst);
}
void nanoasm::shl(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(4, dst);
    } else {
        DB(0xC0); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(4, dst);
    } else {
        DB(0xC0); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(4, dst);
    } else {
        DB(0xC0); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(4, dst);
    } else {
        DB(0xC1); MOD(4, dst); DB(imm8);
    }
}
void nanoasm::shl(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(4, dst);
}
void nanoasm::shl(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(4, dst);
}
void nanoasm::shl(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(4, dst);
}
void nanoasm::shl(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shl(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shl(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shl(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shl(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shl(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(4, dst);
}
void nanoasm::shr(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(5, dst);
    } else {
        DB(0xC0); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(5, dst);
    } else {
        DB(0xC0); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(5, dst);
    } else {
        DB(0xC0); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(5, dst);
    } else {
        DB(0xC1); MOD(5, dst); DB(imm8);
    }
}
void nanoasm::shr(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(5, dst);
}
void nanoasm::shr(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(5, dst);
}
void nanoasm::shr(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(5, dst);
}
void nanoasm::shr(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(5, dst);
}
void nanoasm::shr(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(5, dst);
}
void nanoasm::shr(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(5, dst);
}
void nanoasm::shr(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(5, dst);
}
void nanoasm::shr(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(5, dst);
}
void nanoasm::shr(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(5, dst);
}
void nanoasm::sar(const reg8_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD0); MOD(7, dst);
    } else {
        DB(0xC0); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(7, dst);
    } else {
        DB(0xC0); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD0); MOD(7, dst);
    } else {
        DB(0xC0); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const reg32_t& dst, int8_t imm8) {
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem32_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem32si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const reg64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem64_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const mem64si_t& dst, int8_t imm8) {
    REX(W + RXB(dst));
    if (imm8 == 1) {
        DB(0xD1); MOD(7, dst);
    } else {
        DB(0xC1); MOD(7, dst); DB(imm8);
    }
}
void nanoasm::sar(const reg8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD2); MOD(7, dst);
}
void nanoasm::sar(const mem8_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(7, dst);
}
void nanoasm::sar(const mem8si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD2); MOD(7, dst);
}
void nanoasm::sar(const reg32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(7, dst);
}
void nanoasm::sar(const mem32_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(7, dst);
}
void nanoasm::sar(const mem32si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(RXB(dst));
    DB(0xD3); MOD(7, dst);
}
void nanoasm::sar(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst));
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    DB(0xD3); MOD(7, dst);
}
void nanoasm::sar(const mem64_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(7, dst);
}
void nanoasm::sar(const mem64si_t& dst, const reg8_t& src) {
    if (src.m_regcode != regcode_rcx) ASSEMBLE_ERROR("expected CL for count operand");
    REX(W + RXB(dst));
    DB(0xD3); MOD(7, dst);
}
void nanoasm::inc(const reg8_t& dst) {
    DB(0xFE); MOD(0, dst);
}
#if ARCH_LP64
void nanoasm::inc(const reg32_t& dst) {
    DB(0xFF); MOD(0, dst);
}
#else
void nanoasm::inc(const reg32_t& dst) {
    DB(0x40 + dst.m_regcode);
}
#endif
void nanoasm::inc(const reg64_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(0, dst);
}
void nanoasm::inc(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0xFE); MOD(0, dst);
}
void nanoasm::inc(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0xFE); MOD(0, dst);
}
void nanoasm::inc(const mem32_t& dst) {
    REX(RXB(dst));
    DB(0xFF); MOD(0, dst);
}
void nanoasm::inc(const mem32si_t& dst) {
    REX(RXB(dst));
    DB(0xFF); MOD(0, dst);
}
void nanoasm::inc(const mem64_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(0, dst);
}
void nanoasm::inc(const mem64si_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(0, dst);
}
void nanoasm::dec(const reg8_t& dst) {
    DB(0xFE); MOD(1, dst);
}
#if ARCH_LP64
void nanoasm::dec(const reg32_t& dst) {
    DB(0xFF); MOD(1, dst);
}
#else
void nanoasm::dec(const reg32_t& dst) {
    DB(0x48 + dst.m_regcode);
}
#endif
void nanoasm::dec(const reg64_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(1, dst);
}
void nanoasm::dec(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0xFE); MOD(1, dst);
}
void nanoasm::dec(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0xFE); MOD(1, dst);
}
void nanoasm::dec(const mem32_t& dst) {
    REX(RXB(dst));
    DB(0xFF); MOD(1, dst);
}
void nanoasm::dec(const mem32si_t& dst) {
    REX(RXB(dst));
    DB(0xFF); MOD(1, dst);
}
void nanoasm::dec(const mem64_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(1, dst);
}
void nanoasm::dec(const mem64si_t& dst) {
    REX(W + RXB(dst));
    DB(0xFF); MOD(1, dst);
}
void nanoasm::not_(const reg8_t& src) {
    DB(0xF6); MOD(2, src);
}
void nanoasm::not_(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(2, src);
}
void nanoasm::not_(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(2, src);
}
void nanoasm::not_(const reg32_t& src) {
    DB(0xF7); MOD(2, src);
}
void nanoasm::not_(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(2, src);
}
void nanoasm::not_(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(2, src);
}
void nanoasm::not_(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(2, src);
}
void nanoasm::not_(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(2, src);
}
void nanoasm::not_(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(2, src);
}
void nanoasm::neg(const reg8_t& src) {
    DB(0xF6); MOD(3, src);
}
void nanoasm::neg(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(3, src);
}
void nanoasm::neg(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(3, src);
}
void nanoasm::neg(const reg32_t& src) {
    DB(0xF7); MOD(3, src);
}
void nanoasm::neg(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(3, src);
}
void nanoasm::neg(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(3, src);
}
void nanoasm::neg(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(3, src);
}
void nanoasm::neg(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(3, src);
}
void nanoasm::neg(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(3, src);
}
void nanoasm::mul(const reg8_t& src) {
    DB(0xF6); MOD(4, src);
}
void nanoasm::mul(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(4, src);
}
void nanoasm::mul(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(4, src);
}
void nanoasm::mul(const reg32_t& src) {
    DB(0xF7); MOD(4, src);
}
void nanoasm::mul(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(4, src);
}
void nanoasm::mul(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(4, src);
}
void nanoasm::mul(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(4, src);
}
void nanoasm::mul(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(4, src);
}
void nanoasm::mul(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(4, src);
}
void nanoasm::imul(const reg8_t& src) {
    DB(0xF6); MOD(5, src);
}
void nanoasm::imul(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(5, src);
}
void nanoasm::imul(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(5, src);
}
void nanoasm::imul(const reg32_t& src) {
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(5, src);
}
void nanoasm::imul(const reg32_t& dst, const reg32_t& src, int32_t imm32) {
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg32_t& dst, const mem32_t& src, int32_t imm32) {
    REX(RXB(src));
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg32_t& dst, const mem32si_t& src, int32_t imm32) {
    REX(RXB(src));
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg32_t& dst, int32_t imm32) {
    imul(dst, dst, imm32);
}
void nanoasm::imul(const reg32_t& dst, const reg32_t& src) {
    imul(dst, src, 1);
}
void nanoasm::imul(const reg32_t& dst, const mem32_t& src) {
    imul(dst, src, 1);
}
void nanoasm::imul(const reg32_t& dst, const mem32si_t& src) {
    imul(dst, src, 1);
}
void nanoasm::imul(const reg64_t& dst, const reg64_t& src, int32_t imm32) {
    REX(W + RXB(dst, src));
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg64_t& dst, const mem64_t& src, int32_t imm32) {
    REX(W + RXB(dst, src));
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg64_t& dst, const mem64si_t& src, int32_t imm32) {
    REX(W + RXB(dst, src));
    if (imm32 == 1) {
        DB(0x0F); DB(0xAF); MOD(dst, src);
    } else if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6B); MOD(dst, src); DB(imm32);
    } else {
        DB(0x69); MOD(dst, src); DD(imm32);
    }
}
void nanoasm::imul(const reg64_t& dst, int32_t imm32) {
    imul(dst, dst, imm32);
}
void nanoasm::imul(const reg64_t& dst, const reg64_t& src) {
    imul(dst, src, 1);
}
void nanoasm::imul(const reg64_t& dst, const mem64_t& src) {
    imul(dst, src, 1);
}
void nanoasm::imul(const reg64_t& dst, const mem64si_t& src) {
    imul(dst, src, 1);
}
void nanoasm::div(const reg8_t& src) {
    DB(0xF6); MOD(6, src);
}
void nanoasm::div(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(6, src);
}
void nanoasm::div(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(6, src);
}
void nanoasm::div(const reg32_t& src) {
    DB(0xF7); MOD(6, src);
}
void nanoasm::div(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(6, src);
}
void nanoasm::div(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(6, src);
}
void nanoasm::div(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(6, src);
}
void nanoasm::div(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(6, src);
}
void nanoasm::div(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(6, src);
}
void nanoasm::idiv(const reg8_t& src) {
    DB(0xF6); MOD(7, src);
}
void nanoasm::idiv(const mem8_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(7, src);
}
void nanoasm::idiv(const mem8si_t& src) {
    REX(RXB(src));
    DB(0xF6); MOD(7, src);
}
void nanoasm::idiv(const reg32_t& src) {
    DB(0xF7); MOD(7, src);
}
void nanoasm::idiv(const mem32_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(7, src);
}
void nanoasm::idiv(const mem32si_t& src) {
    REX(RXB(src));
    DB(0xF7); MOD(7, src);
}
void nanoasm::idiv(const reg64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(7, src);
}
void nanoasm::idiv(const mem64_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(7, src);
}
void nanoasm::idiv(const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0xF7); MOD(7, src);
}
void nanoasm::ja_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x77); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x87); REL32(target);
    } else {
        DB(0x77); REL8(target);
    }
}
void nanoasm::ja(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x77); REL8(target);
    } else {
        DB(0x0F); DB(0x87); REL32(target);
    }
}
void nanoasm::jae_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x73); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x83); REL32(target);
    } else {
        DB(0x73); REL8(target);
    }
}
void nanoasm::jae(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x73); REL8(target);
    } else {
        DB(0x0F); DB(0x83); REL32(target);
    }
}
void nanoasm::jb_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x72); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x82); REL32(target);
    } else {
        DB(0x72); REL8(target);
    }
}
void nanoasm::jb(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x72); REL8(target);
    } else {
        DB(0x0F); DB(0x82); REL32(target);
    }
}
void nanoasm::jbe_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x76); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x86); REL32(target);
    } else {
        DB(0x76); REL8(target);
    }
}
void nanoasm::jbe(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x76); REL8(target);
    } else {
        DB(0x0F); DB(0x86); REL32(target);
    }
}
void nanoasm::jc_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x72); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x82); REL32(target);
    } else {
        DB(0x72); REL8(target);
    }
}
void nanoasm::jc(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x72); REL8(target);
    } else {
        DB(0x0F); DB(0x82); REL32(target);
    }
}
void nanoasm::je_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x74); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x84); REL32(target);
    } else {
        DB(0x74); REL8(target);
    }
}
void nanoasm::je(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x74); REL8(target);
    } else {
        DB(0x0F); DB(0x84); REL32(target);
    }
}
void nanoasm::jg_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7F); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8F); REL32(target);
    } else {
        DB(0x7F); REL8(target);
    }
}
void nanoasm::jg(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7F); REL8(target);
    } else {
        DB(0x0F); DB(0x8F); REL32(target);
    }
}
void nanoasm::jge_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7D); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8D); REL32(target);
    } else {
        DB(0x7D); REL8(target);
    }
}
void nanoasm::jge(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7D); REL8(target);
    } else {
        DB(0x0F); DB(0x8D); REL32(target);
    }
}
void nanoasm::jl_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7C); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8C); REL32(target);
    } else {
        DB(0x7C); REL8(target);
    }
}
void nanoasm::jl(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7C); REL8(target);
    } else {
        DB(0x0F); DB(0x8C); REL32(target);
    }
}
void nanoasm::jle_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7E); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8E); REL32(target);
    } else {
        DB(0x7E); REL8(target);
    }
}
void nanoasm::jle(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7E); REL8(target);
    } else {
        DB(0x0F); DB(0x8E); REL32(target);
    }
}
void nanoasm::jnc_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x73); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x83); REL32(target);
    } else {
        DB(0x73); REL8(target);
    }
}
void nanoasm::jnc(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x73); REL8(target);
    } else {
        DB(0x0F); DB(0x83); REL32(target);
    }
}
void nanoasm::jne_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x75); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x85); REL32(target);
    } else {
        DB(0x75); REL8(target);
    }
}
void nanoasm::jne(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x75); REL8(target);
    } else {
        DB(0x0F); DB(0x85); REL32(target);
    }
}
void nanoasm::jno_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x71); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x81); REL32(target);
    } else {
        DB(0x71); REL8(target);
    }
}
void nanoasm::jno(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x71); REL8(target);
    } else {
        DB(0x0F); DB(0x81); REL32(target);
    }
}
void nanoasm::jnp_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7B); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8B); REL32(target);
    } else {
        DB(0x7B); REL8(target);
    }
}
void nanoasm::jnp(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7B); REL8(target);
    } else {
        DB(0x0F); DB(0x8B); REL32(target);
    }
}
void nanoasm::jns_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x79); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x89); REL32(target);
    } else {
        DB(0x79); REL8(target);
    }
}
void nanoasm::jns(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x79); REL8(target);
    } else {
        DB(0x0F); DB(0x89); REL32(target);
    }
}
void nanoasm::jo_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x70); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x80); REL32(target);
    } else {
        DB(0x70); REL8(target);
    }
}
void nanoasm::jo(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x70); REL8(target);
    } else {
        DB(0x0F); DB(0x80); REL32(target);
    }
}
void nanoasm::jp_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7A); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x8A); REL32(target);
    } else {
        DB(0x7A); REL8(target);
    }
}
void nanoasm::jp(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x7A); REL8(target);
    } else {
        DB(0x0F); DB(0x8A); REL32(target);
    }
}
void nanoasm::js_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x78); REL8(target);
    } else if (INREL32(target)) {
        DB(0x0F); DB(0x88); REL32(target);
    } else {
        DB(0x78); REL8(target);
    }
}
void nanoasm::js(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0x78); REL8(target);
    } else {
        DB(0x0F); DB(0x88); REL32(target);
    }
}
void nanoasm::jnbe_short(const symbol_t& target) {
    ja_short(target);
}
void nanoasm::jnbe(const symbol_t& target) {
    ja(target);
}
void nanoasm::jnb_short(const symbol_t& target) {
    jae_short(target);
}
void nanoasm::jnb(const symbol_t& target) {
    jae(target);
}
void nanoasm::jnae_short(const symbol_t& target) {
    jb_short(target);
}
void nanoasm::jnae(const symbol_t& target) {
    jb(target);
}
void nanoasm::jna_short(const symbol_t& target) {
    jbe_short(target);
}
void nanoasm::jna(const symbol_t& target) {
    jbe(target);
}
void nanoasm::jz_short(const symbol_t& target) {
    je_short(target);
}
void nanoasm::jz(const symbol_t& target) {
    je(target);
}
void nanoasm::jnz_short(const symbol_t& target) {
    jne_short(target);
}
void nanoasm::jnz(const symbol_t& target) {
    jne(target);
}
void nanoasm::jpo_short(const symbol_t& target) {
    jnp_short(target);
}
void nanoasm::jpo(const symbol_t& target) {
    jnp(target);
}
void nanoasm::jpe_short(const symbol_t& target) {
    jp_short(target);
}
void nanoasm::jpe(const symbol_t& target) {
    jp(target);
}
void nanoasm::jnle_short(const symbol_t& target) {
    jg_short(target);
}
void nanoasm::jnle(const symbol_t& target) {
    jg(target);
}
void nanoasm::jnl_short(const symbol_t& target) {
    jge_short(target);
}
void nanoasm::jnl(const symbol_t& target) {
    jge(target);
}
void nanoasm::jnge_short(const symbol_t& target) {
    jl_short(target);
}
void nanoasm::jnge(const symbol_t& target) {
    jl(target);
}
void nanoasm::jng_short(const symbol_t& target) {
    jle_short(target);
}
void nanoasm::jng(const symbol_t& target) {
    jle(target);
}
void nanoasm::cmova(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmova(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmova(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmova(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmova(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmova(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x47); MOD(dst, src);
}
void nanoasm::cmovae(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovae(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovae(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovae(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovae(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovae(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovb(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovb(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovb(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovb(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovb(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovb(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovbe(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovbe(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovbe(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovbe(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovbe(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovbe(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x46); MOD(dst, src);
}
void nanoasm::cmovc(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovc(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovc(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovc(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovc(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmovc(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x42); MOD(dst, src);
}
void nanoasm::cmove(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmove(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmove(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmove(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmove(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmove(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x44); MOD(dst, src);
}
void nanoasm::cmovg(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovg(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovg(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovg(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovg(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovg(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4F); MOD(dst, src);
}
void nanoasm::cmovge(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovge(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovge(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovge(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovge(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovge(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4D); MOD(dst, src);
}
void nanoasm::cmovl(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovl(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovl(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovl(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovl(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovl(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4C); MOD(dst, src);
}
void nanoasm::cmovle(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovle(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovle(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovle(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovle(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovle(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4E); MOD(dst, src);
}
void nanoasm::cmovnc(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovnc(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovnc(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovnc(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovnc(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovnc(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x43); MOD(dst, src);
}
void nanoasm::cmovne(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovne(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovne(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovne(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovne(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovne(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x45); MOD(dst, src);
}
void nanoasm::cmovno(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovno(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovno(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovno(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovno(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovno(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x41); MOD(dst, src);
}
void nanoasm::cmovnp(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovnp(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovnp(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovnp(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovnp(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovnp(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4B); MOD(dst, src);
}
void nanoasm::cmovns(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovns(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovns(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovns(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovns(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovns(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x49); MOD(dst, src);
}
void nanoasm::cmovo(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovo(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovo(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovo(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovo(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovo(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x40); MOD(dst, src);
}
void nanoasm::cmovp(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovp(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovp(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovp(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovp(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovp(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x4A); MOD(dst, src);
}
void nanoasm::cmovs(const reg32_t& dst, const reg32_t& src) {
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovs(const reg32_t& dst, const mem32_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovs(const reg32_t& dst, const mem32si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovs(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovs(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovs(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0x48); MOD(dst, src);
}
void nanoasm::cmovnbe(const reg32_t& dst, const reg32_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnbe(const reg32_t& dst, const mem32_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnbe(const reg32_t& dst, const mem32si_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnbe(const reg64_t& dst, const reg64_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnbe(const reg64_t& dst, const mem64_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnbe(const reg64_t& dst, const mem64si_t& src) {
    cmova(dst, src);
}
void nanoasm::cmovnb(const reg32_t& dst, const reg32_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnb(const reg32_t& dst, const mem32_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnb(const reg32_t& dst, const mem32si_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnb(const reg64_t& dst, const reg64_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnb(const reg64_t& dst, const mem64_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnb(const reg64_t& dst, const mem64si_t& src) {
    cmovae(dst, src);
}
void nanoasm::cmovnae(const reg32_t& dst, const reg32_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovnae(const reg32_t& dst, const mem32_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovnae(const reg32_t& dst, const mem32si_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovnae(const reg64_t& dst, const reg64_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovnae(const reg64_t& dst, const mem64_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovnae(const reg64_t& dst, const mem64si_t& src) {
    cmovb(dst, src);
}
void nanoasm::cmovna(const reg32_t& dst, const reg32_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovna(const reg32_t& dst, const mem32_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovna(const reg32_t& dst, const mem32si_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovna(const reg64_t& dst, const reg64_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovna(const reg64_t& dst, const mem64_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovna(const reg64_t& dst, const mem64si_t& src) {
    cmovbe(dst, src);
}
void nanoasm::cmovz(const reg32_t& dst, const reg32_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovz(const reg32_t& dst, const mem32_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovz(const reg32_t& dst, const mem32si_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovz(const reg64_t& dst, const reg64_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovz(const reg64_t& dst, const mem64_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovz(const reg64_t& dst, const mem64si_t& src) {
    cmove(dst, src);
}
void nanoasm::cmovnz(const reg32_t& dst, const reg32_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovnz(const reg32_t& dst, const mem32_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovnz(const reg32_t& dst, const mem32si_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovnz(const reg64_t& dst, const reg64_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovnz(const reg64_t& dst, const mem64_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovnz(const reg64_t& dst, const mem64si_t& src) {
    cmovne(dst, src);
}
void nanoasm::cmovpo(const reg32_t& dst, const reg32_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpo(const reg32_t& dst, const mem32_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpo(const reg32_t& dst, const mem32si_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpo(const reg64_t& dst, const reg64_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpo(const reg64_t& dst, const mem64_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpo(const reg64_t& dst, const mem64si_t& src) {
    cmovnp(dst, src);
}
void nanoasm::cmovpe(const reg32_t& dst, const reg32_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovpe(const reg32_t& dst, const mem32_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovpe(const reg32_t& dst, const mem32si_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovpe(const reg64_t& dst, const reg64_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovpe(const reg64_t& dst, const mem64_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovpe(const reg64_t& dst, const mem64si_t& src) {
    cmovp(dst, src);
}
void nanoasm::cmovnle(const reg32_t& dst, const reg32_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnle(const reg32_t& dst, const mem32_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnle(const reg32_t& dst, const mem32si_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnle(const reg64_t& dst, const reg64_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnle(const reg64_t& dst, const mem64_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnle(const reg64_t& dst, const mem64si_t& src) {
    cmovg(dst, src);
}
void nanoasm::cmovnl(const reg32_t& dst, const reg32_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnl(const reg32_t& dst, const mem32_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnl(const reg32_t& dst, const mem32si_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnl(const reg64_t& dst, const reg64_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnl(const reg64_t& dst, const mem64_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnl(const reg64_t& dst, const mem64si_t& src) {
    cmovge(dst, src);
}
void nanoasm::cmovnge(const reg32_t& dst, const reg32_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovnge(const reg32_t& dst, const mem32_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovnge(const reg32_t& dst, const mem32si_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovnge(const reg64_t& dst, const reg64_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovnge(const reg64_t& dst, const mem64_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovnge(const reg64_t& dst, const mem64si_t& src) {
    cmovl(dst, src);
}
void nanoasm::cmovng(const reg32_t& dst, const reg32_t& src) {
    cmovle(dst, src);
}
void nanoasm::cmovng(const reg32_t& dst, const mem32_t& src) {
    cmovle(dst, src);
}
void nanoasm::cmovng(const reg32_t& dst, const mem32si_t& src) {
    cmovle(dst, src);
}
void nanoasm::cmovng(const reg64_t& dst, const reg64_t& src) {
    cmovle(dst, src);
}
void nanoasm::cmovng(const reg64_t& dst, const mem64_t& src) {
    cmovle(dst, src);
}
void nanoasm::cmovng(const reg64_t& dst, const mem64si_t& src) {
    cmovle(dst, src);
}
void nanoasm::seta(const reg8_t& dst) {
    DB(0x0F); DB(0x97); MOD(2, dst);
}
void nanoasm::seta(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x97); MOD(2, dst);
}
void nanoasm::seta(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x97); MOD(2, dst);
}
void nanoasm::setae(const reg8_t& dst) {
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setae(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setae(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setb(const reg8_t& dst) {
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::setb(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::setb(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::setbe(const reg8_t& dst) {
    DB(0x0F); DB(0x96); MOD(2, dst);
}
void nanoasm::setbe(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x96); MOD(2, dst);
}
void nanoasm::setbe(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x96); MOD(2, dst);
}
void nanoasm::setc(const reg8_t& dst) {
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::setc(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::setc(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x92); MOD(2, dst);
}
void nanoasm::sete(const reg8_t& dst) {
    DB(0x0F); DB(0x94); MOD(2, dst);
}
void nanoasm::sete(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x94); MOD(2, dst);
}
void nanoasm::sete(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x94); MOD(2, dst);
}
void nanoasm::setg(const reg8_t& dst) {
    DB(0x0F); DB(0x9F); MOD(2, dst);
}
void nanoasm::setg(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9F); MOD(2, dst);
}
void nanoasm::setg(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9F); MOD(2, dst);
}
void nanoasm::setge(const reg8_t& dst) {
    DB(0x0F); DB(0x9D); MOD(2, dst);
}
void nanoasm::setge(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9D); MOD(2, dst);
}
void nanoasm::setge(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9D); MOD(2, dst);
}
void nanoasm::setl(const reg8_t& dst) {
    DB(0x0F); DB(0x9C); MOD(2, dst);
}
void nanoasm::setl(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9C); MOD(2, dst);
}
void nanoasm::setl(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9C); MOD(2, dst);
}
void nanoasm::setle(const reg8_t& dst) {
    DB(0x0F); DB(0x9E); MOD(2, dst);
}
void nanoasm::setle(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9E); MOD(2, dst);
}
void nanoasm::setle(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9E); MOD(2, dst);
}
void nanoasm::setnc(const reg8_t& dst) {
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setnc(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setnc(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x93); MOD(2, dst);
}
void nanoasm::setne(const reg8_t& dst) {
    DB(0x0F); DB(0x95); MOD(2, dst);
}
void nanoasm::setne(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x95); MOD(2, dst);
}
void nanoasm::setne(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x95); MOD(2, dst);
}
void nanoasm::setno(const reg8_t& dst) {
    DB(0x0F); DB(0x91); MOD(2, dst);
}
void nanoasm::setno(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x91); MOD(2, dst);
}
void nanoasm::setno(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x91); MOD(2, dst);
}
void nanoasm::setnp(const reg8_t& dst) {
    DB(0x0F); DB(0x9B); MOD(2, dst);
}
void nanoasm::setnp(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9B); MOD(2, dst);
}
void nanoasm::setnp(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9B); MOD(2, dst);
}
void nanoasm::setns(const reg8_t& dst) {
    DB(0x0F); DB(0x99); MOD(2, dst);
}
void nanoasm::setns(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x99); MOD(2, dst);
}
void nanoasm::setns(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x99); MOD(2, dst);
}
void nanoasm::seto(const reg8_t& dst) {
    DB(0x0F); DB(0x90); MOD(2, dst);
}
void nanoasm::seto(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x90); MOD(2, dst);
}
void nanoasm::seto(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x90); MOD(2, dst);
}
void nanoasm::setp(const reg8_t& dst) {
    DB(0x0F); DB(0x9A); MOD(2, dst);
}
void nanoasm::setp(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9A); MOD(2, dst);
}
void nanoasm::setp(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x9A); MOD(2, dst);
}
void nanoasm::sets(const reg8_t& dst) {
    DB(0x0F); DB(0x98); MOD(2, dst);
}
void nanoasm::sets(const mem8_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x98); MOD(2, dst);
}
void nanoasm::sets(const mem8si_t& dst) {
    REX(RXB(dst));
    DB(0x0F); DB(0x98); MOD(2, dst);
}
void nanoasm::setnbe(const reg8_t& dst) {
    seta(dst);
}
void nanoasm::setnbe(const mem8_t& dst) {
    seta(dst);
}
void nanoasm::setnbe(const mem8si_t& dst) {
    seta(dst);
}
void nanoasm::setnb(const reg8_t& dst) {
    setae(dst);
}
void nanoasm::setnb(const mem8_t& dst) {
    setae(dst);
}
void nanoasm::setnb(const mem8si_t& dst) {
    setae(dst);
}
void nanoasm::setnae(const reg8_t& dst) {
    setb(dst);
}
void nanoasm::setnae(const mem8_t& dst) {
    setb(dst);
}
void nanoasm::setnae(const mem8si_t& dst) {
    setb(dst);
}
void nanoasm::setna(const reg8_t& dst) {
    setbe(dst);
}
void nanoasm::setna(const mem8_t& dst) {
    setbe(dst);
}
void nanoasm::setna(const mem8si_t& dst) {
    setbe(dst);
}
void nanoasm::setz(const reg8_t& dst) {
    sete(dst);
}
void nanoasm::setz(const mem8_t& dst) {
    sete(dst);
}
void nanoasm::setz(const mem8si_t& dst) {
    sete(dst);
}
void nanoasm::setnz(const reg8_t& dst) {
    setne(dst);
}
void nanoasm::setnz(const mem8_t& dst) {
    setne(dst);
}
void nanoasm::setnz(const mem8si_t& dst) {
    setne(dst);
}
void nanoasm::setpo(const reg8_t& dst) {
    setnp(dst);
}
void nanoasm::setpo(const mem8_t& dst) {
    setnp(dst);
}
void nanoasm::setpo(const mem8si_t& dst) {
    setnp(dst);
}
void nanoasm::setpe(const reg8_t& dst) {
    setp(dst);
}
void nanoasm::setpe(const mem8_t& dst) {
    setp(dst);
}
void nanoasm::setpe(const mem8si_t& dst) {
    setp(dst);
}
void nanoasm::setnle(const reg8_t& dst) {
    setg(dst);
}
void nanoasm::setnle(const mem8_t& dst) {
    setg(dst);
}
void nanoasm::setnle(const mem8si_t& dst) {
    setg(dst);
}
void nanoasm::setnl(const reg8_t& dst) {
    setge(dst);
}
void nanoasm::setnl(const mem8_t& dst) {
    setge(dst);
}
void nanoasm::setnl(const mem8si_t& dst) {
    setge(dst);
}
void nanoasm::setnge(const reg8_t& dst) {
    setl(dst);
}
void nanoasm::setnge(const mem8_t& dst) {
    setl(dst);
}
void nanoasm::setnge(const mem8si_t& dst) {
    setl(dst);
}
void nanoasm::setng(const reg8_t& dst) {
    setle(dst);
}
void nanoasm::setng(const mem8_t& dst) {
    setle(dst);
}
void nanoasm::setng(const mem8si_t& dst) {
    setle(dst);
}
void nanoasm::test(const reg8_t& dst, const reg8_t& src) {
    DB(0x84); MOD(src, dst);
}
void nanoasm::test(const mem8_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x84); MOD(src, dst);
}
void nanoasm::test(const mem8si_t& dst, const reg8_t& src) {
    REX(RXB(dst));
    DB(0x84); MOD(src, dst);
}
void nanoasm::test(const reg8_t& dst, int8_t imm8) {
    if (dst.m_regcode == regcode_rax) {
        DB(0xA8); DB(imm8);
    } else {
        DB(0xF6); MOD(0, dst); DB(imm8);
    }
}
void nanoasm::test(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0xF6); MOD(0, dst); DB(imm8);
}
void nanoasm::test(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0xF6); MOD(0, dst); DB(imm8);
}
void nanoasm::test(const reg32_t& dst, const reg32_t& src) {
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const mem32_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const mem32si_t& dst, const reg32_t& src) {
    REX(RXB(dst));
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const reg32_t& dst, int32_t imm32) {
    if (dst.m_regcode == regcode_rax) {
        DB(0xA9); DD(imm32);
    } else {
        DB(0xF7); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::test(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    DB(0xF7); MOD(0, dst); DD(imm32);
}
void nanoasm::test(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    DB(0xF7); MOD(0, dst); DD(imm32);
}
void nanoasm::test(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x85); MOD(src, dst);
}
void nanoasm::test(const reg64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    if (dst.m_regcode == regcode_rax) {
        DB(0xA9); DD(imm32);
    } else {
        DB(0xF7); MOD(0, dst); DD(imm32);
    }
}
void nanoasm::test(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    DB(0xF7); MOD(0, dst); DD(imm32);
}
void nanoasm::test(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    DB(0xF7); MOD(0, dst); DD(imm32);
}
void nanoasm::mov(const reg8_t& dst, const reg8_t& src) {
    DB(0x88); MOD(src, dst);
}
void nanoasm::mov(const reg32_t& dst, const reg32_t& src) {
    DB(0x89); MOD(src, dst);
}
void nanoasm::mov(const reg64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    DB(0x89); MOD(src, dst);
}
void nanoasm::mov(const mem8_t& dst, const reg8_t& src) {
#if ARCH_LP64
    if (src.m_regcode == RAX && dst.direct64()) {
        REX(W + RXB(dst));
        DB(0xA2); DQ(dst.m_disp);
    } else {
        REX(RXB(dst));
        DB(0x88); MOD(src, dst);
    }
#else
    if (src.m_regcode == RAX && dst.direct32()) {
        DB(0xA2); DD(dst.m_disp);
    } else {
        DB(0x88); MOD(src, dst);
    }
#endif
}
void nanoasm::mov(const mem8si_t& dst, const reg8_t& src) {
#if ARCH_LP64
    if (src.m_regcode == RAX && dst.direct64()) {
        REX(W + RXB(dst));
        DB(0xA2); DQ(dst.m_disp);
    } else {
        REX(RXB(dst));
        DB(0x88); MOD(src, dst);
    }
#else
    if (src.m_regcode == RAX && dst.direct32()) {
        DB(0xA2); DD(dst.m_disp);
    } else {
        DB(0x88); MOD(src, dst);
    }
#endif
}
void nanoasm::mov(const mem32_t& dst, const reg32_t& src) {
#if ARCH_LP64
    REX(RXB(dst));
    DB(0x89); MOD(src, dst);
#else
    if (src.m_regcode == RAX && dst.direct32()) {
        DB(0xA3); DD(dst.m_disp);
    } else {
        DB(0x89); MOD(src, dst);
    }
#endif
}
void nanoasm::mov(const mem32si_t& dst, const reg32_t& src) {
#if ARCH_LP64
    REX(RXB(dst));
    DB(0x89); MOD(src, dst);
#else
    if (src.m_regcode == RAX && dst.direct32()) {
        DB(0xA3); DD(dst.m_disp);
    } else {
        DB(0x89); MOD(src, dst);
    }
#endif
}
void nanoasm::mov(const mem64_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    if (src.m_regcode == RAX && dst.direct64()) {
        DB(0xA3); DQ(dst.m_disp);
    } else {
        DB(0x89); MOD(src, dst);
    }
}
void nanoasm::mov(const mem64si_t& dst, const reg64_t& src) {
    REX(W + RXB(src, dst));
    if (src.m_regcode == RAX && dst.direct64()) {
        DB(0xA3); DQ(dst.m_disp);
    } else {
        DB(0x89); MOD(src, dst);
    }
}
void nanoasm::mov(const reg8_t& dst, const mem8_t& src) {
#if ARCH_LP64
    if (dst.m_regcode == RAX && src.direct64()) {
        REX(W + RXB(src));
        DB(0xA0); DQ(src.m_disp);
    } else {
        REX(RXB(src));
        DB(0x8A); MOD(dst, src);
    }
#else
    if (dst.m_regcode == RAX && src.direct32()) {
        DB(0xA0); DD(src.m_disp);
    } else {
        DB(0x8A); MOD(dst, src);
    }
#endif
}
void nanoasm::mov(const reg8_t& dst, const mem8si_t& src) {
#if ARCH_LP64
    if (dst.m_regcode == RAX && src.direct64()) {
        REX(W + RXB(src));
        DB(0xA0); DQ(src.m_disp);
    } else {
        REX(RXB(src));
        DB(0x8A); MOD(dst, src);
    }
#else
    if (dst.m_regcode == RAX && src.direct32()) {
        DB(0xA0); DD(src.m_disp);
    } else {
        DB(0x8A); MOD(dst, src);
    }
#endif
}
void nanoasm::mov(const reg32_t& dst, const mem32_t& src) {
#if ARCH_LP64
    REX(RXB(src));
    DB(0x8B); MOD(dst, src);
#else
    if (dst.m_regcode == RAX && src.direct32()) {
        DB(0xA1); DD(src.m_disp);
    } else {
        DB(0x8B); MOD(dst, src);
    }    
#endif
}
void nanoasm::mov(const reg32_t& dst, const mem32si_t& src) {
#if ARCH_LP64
    REX(RXB(src));
    DB(0x8B); MOD(dst, src);
#else
    if (dst.m_regcode == RAX && src.direct32()) {
        DB(0xA1); DD(src.m_disp);
    } else {
        DB(0x8B); MOD(dst, src);
    }    
#endif
}
void nanoasm::mov(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(dst, src));
    if (dst.m_regcode == RAX && src.direct64()) {
        DB(0xA1); DQ(src.m_disp);
    } else {
        DB(0x8B); MOD(dst, src);
    }
}
void nanoasm::mov(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(dst, src));
    if (dst.m_regcode == RAX && src.direct64()) {
        DB(0xA1); DQ(src.m_disp);
    } else {
        DB(0x8B); MOD(dst, src);
    }
}
void nanoasm::mov(const reg8_t& dst, int8_t imm8) {
    DB(0xB0 + dst.m_regcode); DB(imm8);
}
void nanoasm::mov(const reg32_t& dst, int32_t imm32) {
    DB(0xB8 + dst.m_regcode); DD(imm32);
}
void nanoasm::mov(const reg64_t& dst, int64_t imm64) { // todo: optimize
    REX(W + RXB(dst));
    DB(0xB8 + (dst.m_regcode & 7)); DQ(imm64);
}
void nanoasm::mov(const mem8_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0xC6); MOD(0, dst); DB(imm8);
}
void nanoasm::mov(const mem8si_t& dst, int8_t imm8) {
    REX(RXB(dst));
    DB(0xC6); MOD(0, dst); DB(imm8);
}
void nanoasm::mov(const mem32_t& dst, int32_t imm32) {
    REX(RXB(dst));
    DB(0xC7); MOD(0, dst); DD(imm32);
}
void nanoasm::mov(const mem32si_t& dst, int32_t imm32) {
    REX(RXB(dst));
    DB(0xC7); MOD(0, dst); DD(imm32);
}
void nanoasm::mov(const mem64_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    DB(0xC7); MOD(0, dst); DD(imm32);
}
void nanoasm::mov(const mem64si_t& dst, int32_t imm32) {
    REX(W + RXB(dst));
    DB(0xC7); MOD(0, dst); DD(imm32);
}
#if ARCH_LP64
void nanoasm::mov(const reg64_t& dst, const symbol_t& target) {
    REX(W + RXB(dst));
    DB(0xB8 + (dst.m_regcode & 7));
    DQ(absolute_reloc(target));
}
#else
void nanoasm::mov(const reg32_t& dst, const symbol_t& target) {
    DB(0xB8 + dst.m_regcode);
    DD(absolute_reloc(target));
}
#endif
void nanoasm::movzx(const reg32_t& dst, const reg8_t& src) {
    DB(0x0F); DB(0xB6); MOD(dst.m_regcode, src.m_regcode);
}
void nanoasm::movzx(const reg32_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0xB6); MOD(dst, src);
}
void nanoasm::movzx(const reg32_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0xB6); MOD(dst, src);
}
void nanoasm::movzx(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst.m_regcode, src.m_regcode));
    DB(0x0F); DB(0xB6); MOD(dst.m_regcode, src.m_regcode);
}
void nanoasm::movzx(const reg64_t& dst, const mem8_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0xB6); MOD(dst, src);
}
void nanoasm::movzx(const reg64_t& dst, const mem8si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0xB6); MOD(dst, src);
}
void nanoasm::movsx(const reg32_t& dst, const reg8_t& src) {
    DB(0x0F); DB(0xBE); MOD(dst.m_regcode, src.m_regcode);
}
void nanoasm::movsx(const reg32_t& dst, const mem8_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0xBE); MOD(dst, src);
}
void nanoasm::movsx(const reg32_t& dst, const mem8si_t& src) {
    REX(RXB(src));
    DB(0x0F); DB(0xBE); MOD(dst, src);
}
void nanoasm::movsx(const reg64_t& dst, const reg8_t& src) {
    REX(W + RXB(dst.m_regcode, src.m_regcode));
    DB(0x0F); DB(0xBE); MOD(dst.m_regcode, src.m_regcode);
}
void nanoasm::movsx(const reg64_t& dst, const mem8_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0xBE); MOD(dst, src);
}
void nanoasm::movsx(const reg64_t& dst, const mem8si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x0F); DB(0xBE); MOD(dst, src);
}
void nanoasm::movsxd(const reg64_t& dst, const reg32_t& src) {
    REX(W + RXB(dst.m_regcode, src.m_regcode));
    DB(0x63); MOD(dst.m_regcode, src.m_regcode);
}
void nanoasm::movsxd(const reg64_t& dst, const mem32_t& src) {
    REX(W + RXB(dst, src));
    DB(0x63); MOD(dst, src);
}
void nanoasm::movsxd(const reg64_t& dst, const mem32si_t& src) {
    REX(W + RXB(dst, src));
    DB(0x63); MOD(dst, src);
}
#if ARCH_LP64
void nanoasm::lea(const reg64_t& dst, const mem8_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const mem8si_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const mem32_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const mem32si_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const mem64_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const mem64si_t& src) {
    REX(W + RXB(src));
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg64_t& dst, const symbol_t& target) {
    REX(W + ((dst.m_regcode >> 1) & 0x4));
    DB(0x8D);
    DB(0x05 + ((dst.m_regcode & 7) << 3));
    DD(relative_reloc(target));
}
#else
void nanoasm::lea(const reg32_t& dst, const mem8_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const mem8si_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const mem32_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const mem32si_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const mem64_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const mem64si_t& src) {
    DB(0x8D); MOD(dst, src);
}
void nanoasm::lea(const reg32_t& dst, const symbol_t& target) {
    mov(dst, target);
}
#endif
#if ARCH_LP64
void nanoasm::push(const reg64_t& src) {
    REX(RXB(src));
    DB(0x50 + (src.m_regcode & 7));
}
void nanoasm::push(const mem64_t& src) {
    REX(RXB(src));
    DB(0xFF); MOD(6, src);
}
void nanoasm::push(const mem64si_t& src) {
    REX(RXB(src));
    DB(0xFF); MOD(6, src);
}
void nanoasm::pop(const reg64_t& dst) {
    REX(RXB(dst));
    DB(0x58 + (dst.m_regcode & 7));
}
void nanoasm::pop(const mem64_t& dst) {
    REX(RXB(dst));
    DB(0x8F); MOD(0, dst);
}
void nanoasm::pop(const mem64si_t& dst) {
    REX(RXB(dst));
    DB(0x8F); MOD(0, dst);
}
#else
void nanoasm::push(const reg32_t& src) {
    DB(0x50 + src.m_regcode);
}
void nanoasm::push(const mem32_t& src) {
    DB(0xFF); MOD(6, src);
}
void nanoasm::push(const mem32si_t& src) {
    DB(0xFF); MOD(6, src);
}
void nanoasm::pop(const reg32_t& dst) {
    DB(0x58 + dst.m_regcode);
}
void nanoasm::pop(const mem32_t& dst) {
    DB(0x8F); MOD(0, dst);
}
void nanoasm::pop(const mem32si_t& dst) {
    DB(0x8F); MOD(0, dst);
}
#endif
void nanoasm::push(int32_t imm32) {
    if (imm32 >= INT8_MIN && imm32 <= INT8_MAX) {
        DB(0x6A); DB(imm32);
    } else {
        DB(0x68); DD(imm32);
    }
}
void nanoasm::jmp_short(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0xEB); REL8(target);
    } else if (INREL32(target)) {
        DB(0xE9); REL32(target);
    } else {
        DB(0xEB); REL8(target);
    }
}
void nanoasm::jmp(const symbol_t& target) {
    if (INREL8(target)) {
        DB(0xEB); REL8(target);
    } else {
        DB(0xE9); REL32(target);
    }
}
#if ARCH_LP64
void nanoasm::jmp(const reg64_t& dst) {
    DB(0xFF); MOD(4, dst);
}
void nanoasm::jmp(const mem64_t& dst) {
    DB(0xFF); MOD(4, dst);
}
void nanoasm::jmp(const mem64si_t& dst) {
    DB(0xFF); MOD(4, dst);
}
#else
void nanoasm::jmp(const reg32_t& dst) {
    DB(0xFF); MOD(4, dst);
}
void nanoasm::jmp(const mem32_t& dst) {
    DB(0xFF); MOD(4, dst);
}
void nanoasm::jmp(const mem32si_t& dst) {
    DB(0xFF); MOD(4, dst);
}
#endif
void nanoasm::call(const symbol_t& target) {
    DB(0xE8); REL32(target);
}
#if ARCH_LP64
void nanoasm::call(const reg64_t& dst) {
    DB(0xFF); MOD(2, dst);
}
void nanoasm::call(const mem64_t& dst) {
    DB(0xFF); MOD(2, dst);
}
void nanoasm::call(const mem64si_t& dst) {
    DB(0xFF); MOD(2, dst);
}
#else
void nanoasm::call(const reg32_t& dst) {
    DB(0xFF); MOD(2, dst);
}
void nanoasm::call(const mem32_t& dst) {
    DB(0xFF); MOD(2, dst);
}
void nanoasm::call(const mem32si_t& dst) {
    DB(0xFF); MOD(2, dst);
}
#endif
void nanoasm::ret() {
    DB(0xC3);
}
