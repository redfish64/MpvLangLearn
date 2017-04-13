
#include <stdio.h>
#include <mpv/client.h>
#include "foo.h"


int fakeout(mpv_handle *ctx)
{
  printf("Hey there, context is %X\n",ctx);
  return -1;
}
