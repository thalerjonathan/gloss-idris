#ifndef __GL_IDRIS_H
#define __GL_IDRIS_H

#include <idris_rts.h>


// -------------------------------------------------------------- [ buffers ]


// set a double in a buffer
void idr_buffers_set_double(void* buf, int index, double val);

// allocate an empty double buffer of len doubles
void* idr_buffers_double_buffer(int len);

// the size of a double
int idr_buffers_double_size();

// set a double as a float in a buffer
void idr_buffers_set_float(void* buf, int index, double val);

// read a float from a float buffer and returns it as a double
double idr_buffers_read_float(void* buf, int index);

// allocate an empty float buffer of length len
void* idr_buffers_float_buffer(int len);

// the size of a float
int idr_buffers_float_size();


void idr_buffers_set_string(void* buf, int index, const char* str);

void* idr_buffers_string_buffer(int len);

void idr_buffers_free_string_buffer(void* buf, int len);

// set an int in a buffer
void idr_buffers_set_int(void* buf, int index, int val);

// read an int from the buffer
int idr_buffers_read_int(void* buf, int index);

// creates an int buffer
void* idr_buffers_int_buffer(int len);

// the size of a int
int idr_buffers_int_size();

// -------------------------------------------------------------- [ old functions ]

int idr_init_glew();

//char* idr_glGetString(int name);

struct PNGLoad {
  int pngWidth;
  int pngHeight;
  int pngFormat; // 0: RGB, 1: RGBA
  void* pngRawData;
} PNGLoad;

struct PNGLoad* png_load(const char * file_name);

GLuint png_texture_load(const char * file_name);

int png_texture(GLenum target, GLint level, const char * file_name);

void idr_main_loop();

void printShaderLog(int shaderId);


// get current time - returns an idris value
void* idr_currentTimeMicros(VM* vm);

#endif
