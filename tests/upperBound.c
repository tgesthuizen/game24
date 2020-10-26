#define main xmain
#include "../game24.c"

#undef main

int main() {
  enum { arrsize = 6 };
  uint16_t array[arrsize] = {1, 2, 3, 5, 6, 7};
  uint16_t *first = array, *last = array + arrsize;
#define CHECK_PTR_EQ(lhs, rhs)                                                 \
  {                                                                            \
    void *lhsPtr = (lhs), *rhsPtr = (rhs);                                     \
    if (lhsPtr != rhsPtr) {                                                    \
      printf("%s: %d: Expected result to be %p but found it to be %p\n",       \
             __FILE__, __LINE__, lhsPtr, rhsPtr);                              \
    }                                                                          \
  }
  CHECK_PTR_EQ(upperBound(first, last, 2), first + 1);
  CHECK_PTR_EQ(upperBound(first, last, 4), first + 3);
  CHECK_PTR_EQ(upperBound(first, last, 1234), last);
  CHECK_PTR_EQ(upperBound(first, last, 0), first);
}
