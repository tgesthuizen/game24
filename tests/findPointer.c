#define main xmain
#include "../game24.c"

#undef main
enum { data_size = 4 };

int main() {
  unsigned char *data[data_size] = {(void *)1, (void *)2, (void *)3, NULL};
  unsigned char **result = findPointer(data, data + data_size, NULL);
  int err = 0;
  if (result != data + data_size - 1) {
    printf("Expected result to be %p but found it to be %p\n",
           (void *)(data + data_size - 1), (void *)result);
    err = 1;
  }
  data[data_size - 1] = (void *)4;
  result = findPointer(data, data + data_size, NULL);
  if (result != data + data_size) {
    printf("Expected result to be %p but found it to be %p\n",
           (void *)(data + data_size), (void *)result);
    err = 1;
  }
  return err;
}