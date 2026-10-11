#include "fun_rt.h"
int main(void) {
    rt_init();
    FunWord answer = rt_prim_intAdd((40ULL << 1) | 1, (2ULL << 1) | 1);
    rt_prim_putStr(rt_prim_showInt(answer));
    FunWord returned = rt_prim_trace(rt_prim_showInt(answer), answer);
    if (returned != answer) return 2;
    rt_exit(0);
}
