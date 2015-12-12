#include <stddef.h>

typedef struct T
{
  enum { NONE, SOME } tag;
  union
  {
     void *none;
     int some;
  } value;
} Option;

int main()
{
    Option a = { .tag = NONE, .value = { .none = NULL } };
    Option b = { .tag = SOME, .value = { .some = 3 } };
}
