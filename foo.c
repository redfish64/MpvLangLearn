
#include <stdio.h>
#include <mpv/client.h>
#include "foo.h"


int set_multiple_subfiles(mpv_handle *ctx, int num_files, char **subfiles)
{
    mpv_node v[num_files];

    int i = 0;
    for(i = 0; i < num_files; i++)
      {
	v[i].u.string=subfiles[i];
	v[i].format= MPV_FORMAT_STRING;
      }
    mpv_node_list l;
    l.num = num_files;
    l.values = &v;
    mpv_node rv;
    rv.format=MPV_FORMAT_NODE_ARRAY;
    rv.u.list = &l;
    
    return mpv_set_option(ctx,"sub-file",MPV_FORMAT_NODE,&rv);
}
