#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t* start_of_stack;
extern uint64_t* end_of_stack;
extern uint64_t* start_of_heap;
extern uint64_t* end_of_heap;
extern uint64_t* heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
// #define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%016"PRIx64":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}

void print_stack(){
  printf("\nSTACK:\n");
  
  int64_t* curr = end_of_stack;
  while(curr<start_of_stack){
    debugf("%016"PRIx64":  ", curr);
    debugf("%016"PRIx64"\n", *curr);
    curr = curr + 1;
  }
  printf("\n");
}

bool is_pointer(int64_t ptr){
  if (((ptr & 0x3) == 0x1) && ptr >= start_of_heap && ptr < heap_cursor){
    return true;
  }
  return false;
}

bool is_marked(int64_t* ptr){
  // ptr is a machine pointer
  ptr = ptr + 1; // move forward to the GC word 
  int64_t gc_word = *ptr; // deref the ptr to get GC word
  return (gc_word != 0x0);
}

bool is_tuple(int64_t* ptr){
  // get num_args for closure or num_elements for tuple
  int64_t value = * ptr;
  // check if high bit is set
  return ((value & 0x8000000000000000) == 0x0);
}

bool is_closure(int64_t* ptr){
  return (false==is_tuple(ptr));
}

int64_t* get_machine_ptr(int64_t* bird_ptr){
  int64_t bird_ptr_as_int = bird_ptr;
  int64_t* machine_ptr = (int64_t*)(bird_ptr_as_int-1);
  return machine_ptr;
}

int64_t* get_bird_ptr(int64_t* machine_ptr){
  int64_t machine_ptr_as_int = machine_ptr;
  int64_t* bird_ptr = (int64_t*)(machine_ptr_as_int+1);
  return bird_ptr;
}

void dfs(int64_t* root){
  //convert bird ptr to machine ptr
  int64_t* machineRootPtr = get_machine_ptr(root);

  //check if marked 
  if(is_marked(machineRootPtr)){
    return;
  }
  
  //mark i.e set GC word to 0x1
  int64_t* gc_ptr = machineRootPtr + 1;
  *gc_ptr = 0xFFFFFFFFFFFFFFFF; // TODO: CHANGE BACK TO 0x1

  // 
  if(is_tuple(machineRootPtr)){
    // get the number of elements in the tuple
    int64_t num_elements = *machineRootPtr;

    for(int64_t i=0;i<num_elements;i++){
      // skip the NumElems and GC word and start looping through the elements
      int64_t* elem_val = *(machineRootPtr + i + 2);
      if(is_pointer(elem_val)){
        dfs(elem_val); // auto cast elem val into a c pointer i.e. a bird pointer
      }
    }
  }else if(is_closure(machineRootPtr)){
    int64_t num_bird_args = *machineRootPtr;
    // clear the high bit to get the number of args in a closure
    int64_t num_args = num_bird_args - 0x8000000000000000;
    // iterate and recurse for each argument
    for(int64_t j=0; j<num_args; j++){
      // skip numArgs,GC Word, numParams and Function pointer 
      int64_t arg_val = *(machineRootPtr + j + 4);
      if(is_pointer(arg_val)){
        dfs(arg_val);
      }
    }
  }
  return;
}

void tidyup(){
  int64_t* heap_idx = heap_cursor;
  while(heap_idx<end_of_heap){
    *heap_idx = 0xbbadbadbadbadbad;
    heap_idx = heap_idx + 1;
  }

}

void unmark(){
  //initialize 
  int64_t* next_heap_object = start_of_heap;

  //iterate until 
  while(next_heap_object<heap_cursor){
    //get size of current heap object 
    int size;
    if (is_tuple(next_heap_object)){
      size = *next_heap_object ;
      size = size + 2;
    }else{
    int64_t num_bird_args = *next_heap_object;
    int64_t num_args = num_bird_args - 0x8000000000000000;
    size = num_args;
    size = size + 4; 
    }
    
    // set GC word to zero
    *(next_heap_object + 1) = 0x0;
    next_heap_object = next_heap_object + size;
  }
}

int64_t* compact(){
  //initialize 
  int64_t* next_heap_object = start_of_heap;
  int64_t* new_heap_cursor = start_of_heap;

  //iterate until 
  while(next_heap_object<heap_cursor){
    //get size of current heap object 
    int size;
    if (is_tuple(next_heap_object)){
      size = *next_heap_object ;
      size = size + 2;
    }else{
    int64_t num_bird_args = *next_heap_object;
    int64_t num_args = num_bird_args - 0x8000000000000000;
    size = num_args;
    size = size + 4; 
    }
    
    // 
    if (is_marked(next_heap_object)){
      // get the new starting address for the heap object 
      int64_t* new_addr = *(next_heap_object + 1);

      // copy the size of the heap object
      memmove(new_addr,next_heap_object,sizeof(uint64_t)*size);

      //increment the new heap_cursor 
      new_heap_cursor = new_heap_cursor + size;
    }
    // move next heap object forward
    next_heap_object = next_heap_object + size;
  }
  return new_heap_cursor;
}

void update(){
  // update stack 
  int64_t* curr = end_of_stack;
  while(curr<start_of_stack){
    //look for bird pointers 
    int64_t* stack_entry = *curr;

    if(is_pointer(stack_entry)){
      //convert to machine pointer
      int64_t* machine_ptr = get_machine_ptr(stack_entry);
      int64_t* new_addr = *(machine_ptr + 1);
      int64_t* bird_ptr = get_bird_ptr(new_addr);
      *curr = bird_ptr;
    }
    curr = curr + 1;
  }
  
  //initialize 
  int64_t* next_heap_object = start_of_heap;
  // debugf("Start of heap: %016"PRIx64"\n", start_of_heap);
  // debugf("End of heap: %016"PRIx64"\n", end_of_heap);

  //iterate until 
  while(next_heap_object<heap_cursor){
    //get size of current heap object 
    int64_t size = 0;
    if (is_tuple(next_heap_object)){
      size = *next_heap_object ;
     
      for(int64_t i=0;i<size;i++){
        // skip the NumElems and GC word and start looping through the elements
        int64_t* elem_val = *(next_heap_object + i + 2);
        //check if element is a pointer
        if(is_pointer(elem_val)){
          //convert to machine pointer
          int64_t* machine_ptr = get_machine_ptr(elem_val);
          int64_t* new_addr = *(machine_ptr + 1);
          int64_t* bird_ptr = get_bird_ptr(new_addr);
          // update the old bird pointer with the new one
          *(next_heap_object + i + 2) = bird_ptr;
        }
      }
      size = size + 2;
      // next_heap_object = next_heap_object + size;
    } else{
      int64_t num_bird_args = *next_heap_object;
      size = num_bird_args - 0x8000000000000000;
      for(int64_t j=0; j<size; j++){
        // skip numArgs,GC Word, numParams and Function pointer 
        int64_t arg_val = *(next_heap_object + j + 4);
        if(is_pointer(arg_val)){
          //convert to machine pointer
          int64_t* machine_ptr = get_machine_ptr(arg_val);
          int64_t* new_addr = *(machine_ptr + 1);
          int64_t* bird_ptr = get_bird_ptr(new_addr);
          // update the old bird pointer with the new one
          *(next_heap_object + j + 4) = bird_ptr;
        }
      }
      size = size + 4;
      // next_heap_object = next_heap_object + size;
    }
    next_heap_object = next_heap_object + size;
    // debugf("Next Heap Object: %016"PRIx64"\n", next_heap_object);
    
  }
}


void forward(){
  //initialize 
  int64_t* next_heap_object = start_of_heap;
  int64_t* next_live_dest = start_of_heap;

  //iterate until 
  while(next_heap_object<heap_cursor){
    //get size of current heap object 
    int size;
    if (is_tuple(next_heap_object)){
      size = *next_heap_object ;
      size = size + 2;
    }else{
    int64_t num_bird_args = *next_heap_object;
    int64_t num_args = num_bird_args - 0x8000000000000000;
    size = num_args;
    size = size + 4; 
    }
    
    if (is_marked(next_heap_object)){
      //set GC word to the next live destination
      *(next_heap_object+1) = next_live_dest;
      next_live_dest = next_live_dest + size;
    }
    // move next heap object forward
    next_heap_object = next_heap_object + size;
  }

}

void mark(int64_t* curr_sp){
  // base case when we go above the stack
  if(curr_sp < end_of_stack){
    return;
  }else{
    //deref the stack pointer
    int64_t value = *curr_sp;
    
    //check 
    if (is_pointer(value)){
      //dfs 
      dfs(value);
    }
    //subtract from the pointer to bump up the stack pointer
    int64_t* next_sp = curr_sp - 1; // c pointers add by the amount of the pointer size i.e. 8 bytes
    
    //recurse
    mark(next_sp);
  }
}


void gc(int64_t desired_free) {
  // mark the GC words
  // print_stack();
  mark(start_of_stack);
  // printf("After Mark\n:");
  dump_heap();

  // move the new addr to GC word
  forward();
  // printf("After Forward:\n");
  dump_heap();

  // update all bird ptr to point to the new addr
  update();
  // printf("After Update:\n");
  dump_heap();
  // print_stack();

  // moves object to their new addr and update the heap cursor
  int64_t* new_heap_cursor = compact();  
  heap_cursor = new_heap_cursor;
  debugf("\nNew Heap Cursor: %016"PRIx64"", new_heap_cursor);
  // printf("After compact:\n");
  dump_heap();

  // set GC words back to zero
  unmark();
  // printf("After Unmark:\n");
  dump_heap();

  // set unused bits of the heap to unique values
  tidyup();
  // printf("After Tidyup:\n");
  dump_heap();
  // printf("GC was called\n");

  int64_t heap_cursor_int = heap_cursor;
  int64_t end_of_heap_int = end_of_heap;
  debugf("heap cursor:%016"PRIx64"\n", heap_cursor);
  debugf("end of heap:%016"PRIx64"\n", end_of_heap);
  debugf("diff: %d\n", end_of_heap_int - heap_cursor_int);
  debugf("desired:%d", desired_free);
  

  if((end_of_heap_int-heap_cursor_int)<desired_free){
    stopWithError(7);
  }
}
