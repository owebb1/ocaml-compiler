#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "printer.h"

int64_t bird_main(int64_t* heap_cur,int64_t* end_of_heap) asm("bird_main");

int main(int argc, char** argv) {
  //allocate big chunk of heap memory for bird program to use
  //words
  int size  = 1048576;

  // start_of_heap same as heap_cur
  int64_t* heap_cur = (int64_t*)malloc(sizeof(int64_t)*size);

  //end of heap is start + memory allocated 
  int64_t* end_of_heap = heap_cur + size; 

  // pass the heap pointer to bird
  int64_t result = bird_main(heap_cur,end_of_heap);
  printValue(result);
  return 0;
}
