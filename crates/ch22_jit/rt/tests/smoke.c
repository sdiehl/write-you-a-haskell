#include "fun_rt.h"
int main(void) {
    rt_init();
    FunWord answer = rt_prim_intAdd((40ULL << 1) | 1, (2ULL << 1) | 1);
    rt_prim_putStr(rt_prim_showInt(answer));
    rt_exit(0);
}
