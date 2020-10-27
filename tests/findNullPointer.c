#define main xmain
#include "../game24.c"

#undef main
enum { data_size = 4 };

int main() {
  unsigned char *data[data_size] = {(void *)1, (void *)2, (void *)3, NULL};
  unsigned char **result = findNullPointer(data, data + data_size);
  if (result != data + data_size - 1) {
    printf("Expected result to be %p but found it to be %p\n",
           (void *)(data + data_size - 1), (void *)result);
  }
  data[data_size - 1] = (void *)4;
  result = findNullPointer(data, data + data_size);
  if (result != data + data_size) {
    printf("Expected result to be %p but found it to be %p\n",
           (void *)(data + data_size), (void *)result);
  }
}
