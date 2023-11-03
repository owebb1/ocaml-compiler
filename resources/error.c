#include <stdlib.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int.\n");
      break;
    case 2:
      printf("Expected a boolean.\n");
      break;
    /* TODO: put your other error cases here */
    case 3:
      printf("Expected a tuple.\n");
      break;
    case 4:
      printf("Invalid tuple index.\n");
      break;
    case 5:
      printf("Expected a closure.\n");
      break;
    case 7:
      printf("out of memory\n");
      break;
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
