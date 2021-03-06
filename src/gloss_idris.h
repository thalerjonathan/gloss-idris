#ifndef __GLOSS_IDRIS_H
#define __GLOSS_IDRIS_H

#include <idris_rts.h>

typedef void (*MousePosClbk) (void*, double, double, void*);

struct PNGLoad {
  int pngWidth;
  int pngHeight;
  int pngFormat; // 0: RGB, 1: RGBA
  void* pngRawData;
} PNGLoad;

struct PNGLoad* png_load(const char * file_name);

void initFontRendering();
void renderString(const char* str);

void installMousePosClbk(void* win, MousePosClbk clbk, void* stateRef);

#endif // __GLOSS_IDRIS_H
