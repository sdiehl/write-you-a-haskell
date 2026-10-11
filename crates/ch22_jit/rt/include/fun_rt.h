/* Fun Chapter 21 ABI. Words are typed by generated code, never dynamically inspected.
 * Call rt_init on the main thread before allocating. Pointer words must stay rooted
 * across allocations. The runtime owns all returned objects; never call free. */
#ifndef FUN_RT_21_H
#define FUN_RT_21_H
#include <stdint.h>
#include <stddef.h>
typedef uint64_t FunWord;
void rt_init(void);
FunWord *rt_alloc(size_t total_words);
uint8_t *rt_alloc_atomic(size_t bytes);
FunWord rt_apply(FunWord closure, size_t n, const FunWord *args);
FunWord rt_make_pap(FunWord closure, size_t n, const FunWord *args);
void rt_closure_set(FunWord closure, size_t index, FunWord value);
_Noreturn void rt_exit(int32_t code);
_Noreturn void rt_error(FunWord message);
_Noreturn void rt_div_zero(void);
_Noreturn void rt_pattern_fail(const char *file, uint32_t line, uint32_t col);
FunWord rt_apply1(FunWord closure, FunWord a0);
FunWord rt_apply2(FunWord closure, FunWord a0, FunWord a1);
FunWord rt_apply3(FunWord closure, FunWord a0, FunWord a1, FunWord a2);
FunWord rt_apply4(FunWord closure, FunWord a0, FunWord a1, FunWord a2, FunWord a3);
FunWord rt_prim_intAdd(FunWord a, FunWord b);
FunWord rt_prim_intSub(FunWord a, FunWord b);
FunWord rt_prim_intMul(FunWord a, FunWord b);
FunWord rt_prim_intNeg(FunWord a);
FunWord rt_prim_intQuot(FunWord a, FunWord b);
FunWord rt_prim_intRem(FunWord a, FunWord b);
FunWord rt_prim_intDiv(FunWord a, FunWord b);
FunWord rt_prim_intMod(FunWord a, FunWord b);
FunWord rt_prim_intEq(FunWord a, FunWord b);
FunWord rt_prim_intLt(FunWord a, FunWord b);
FunWord rt_prim_intLe(FunWord a, FunWord b);
FunWord rt_prim_dblAdd(FunWord a, FunWord b);
FunWord rt_prim_dblSub(FunWord a, FunWord b);
FunWord rt_prim_dblMul(FunWord a, FunWord b);
FunWord rt_prim_dblDiv(FunWord a, FunWord b);
FunWord rt_prim_dblNeg(FunWord a);
FunWord rt_prim_dblSqrt(FunWord a);
FunWord rt_prim_dblExp(FunWord a);
FunWord rt_prim_dblLog(FunWord a);
FunWord rt_prim_dblSin(FunWord a);
FunWord rt_prim_dblCos(FunWord a);
FunWord rt_prim_dblPow(FunWord a, FunWord b);
FunWord rt_prim_dblEq(FunWord a, FunWord b);
FunWord rt_prim_dblLt(FunWord a, FunWord b);
FunWord rt_prim_dblLe(FunWord a, FunWord b);
FunWord rt_prim_intToDbl(FunWord a);
FunWord rt_prim_dblFloor(FunWord a);
FunWord rt_prim_dblRound(FunWord a);
FunWord rt_prim_dblTruncate(FunWord a);
FunWord rt_prim_chrOrd(FunWord a);
FunWord rt_prim_chrChr(FunWord a);
FunWord rt_prim_chrEq(FunWord a, FunWord b);
FunWord rt_prim_chrLt(FunWord a, FunWord b);
FunWord rt_prim_strAppend(FunWord a, FunWord b);
FunWord rt_prim_strLength(FunWord a);
FunWord rt_prim_strEq(FunWord a, FunWord b);
FunWord rt_prim_strCompare(FunWord a, FunWord b);
FunWord rt_prim_strUnpack(FunWord a);
FunWord rt_prim_strPack(FunWord a);
FunWord rt_prim_showInt(FunWord a);
FunWord rt_prim_showDouble(FunWord a);
FunWord rt_prim_showChar(FunWord a);
FunWord rt_prim_showString(FunWord a);
FunWord rt_prim_readInt(FunWord a);
FunWord rt_prim_error(FunWord a);
FunWord rt_prim_putStr(FunWord a);
FunWord rt_prim_getLine(FunWord a);
FunWord rt_prim_exit(FunWord a);
#endif
