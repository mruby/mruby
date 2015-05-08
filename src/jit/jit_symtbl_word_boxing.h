#define SYMTBL_SIZE 0
static void* symtbl[SYMTBL_SIZE];
void init_symtbl() {
  static int init = 0;
  if(init == 0) {
    init = 1;
  }
}
