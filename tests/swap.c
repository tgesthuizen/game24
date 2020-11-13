#define main xmain
#include "../iteration2.c"

#undef main

int result = 0;

struct ABitOfEverything {
  int a;
  short b;
  float c;
  long d;
};

static void checkStructEq(const struct ABitOfEverything *found,
                          const struct ABitOfEverything *expected) {
  if (found->a != expected->a || found->b != expected->b ||
      found->c != expected->c || found->d != expected->d) {
    printf("%s: %d: Expected i and l to be equivalent bout they are not: "
           "{%d, %d, %f, %d} vs. {%d, %d, %f, %d}\n",
           __FILE__, __LINE__, (int)found->a, (int)found->b, (float)found->c,
           (int)found->d, (int)expected->a, (int)expected->b,
           (float)expected->c, (int)expected->d);
    result = 1;
  }
}

int main() {
  {
    int i = 1, j = 2;
    swap(&i, &j);
    if (j != 1 || i != 2) {
      printf("%s: %d: Expected i=2, j=1, but found them to be i=%d, j=%d\n",
             __FILE__, __LINE__, i, j);
    }
  }
  {
    int i = 1;
    swap(&i, &i);
    if (i != 1) {
      printf(
          "%s: %d: Expected i to stay unchanged after self swap, but found it "
          "to be %d",
          __FILE__, __LINE__, i);
    }
  }
  {
    struct ABitOfEverything i = {1, 2, 3, 4}, j = {5, 6, 7, 8};
    const struct ABitOfEverything k = i, l = j;
    swap(&i, &j);
    checkStructEq(&i, &l);
    checkStructEq(&j, &k);
  }
  return result;
}
