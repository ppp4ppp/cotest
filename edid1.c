
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef unsigned char byte;


int
parse_refresh_rate( byte* edid )
{
  edid [2] = edid [2] + 1;
  edid [3] = edid [3] + 2;
  edid [4] = 0;
  /*
  unsigned i;
  byte block[100];
  memcpy (block, edid, 5);
  block [2] = block [2] + 1;
  block [3] = block [2] + 2;
  block [4] = 0;
  */
  
  return 0;
}
