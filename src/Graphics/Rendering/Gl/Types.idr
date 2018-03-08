module Graphics.Rendering.Gl.Types

%access public export

GLenum : Type
GLenum = Int

GLbitfield : Type
GLbitfield = Int

GLboolean : Type
GLboolean = Int

GLbyte : Type
GLbyte = Char

GLubyte : Type
GLubyte = Char

GLshort : Type
GLshort = Int

GLushort : Type
GLushort = Int

GLint : Type
GLint = Int

GLuint : Type
GLuint = Int

GLsizei : Type
GLsizei = Int

GLchar : Type
GLchar = Char

GLdouble : Type
GLdouble = Double

GLclampd : Type
GLclampd = Double

-- only temporary
GLfloat : Type
GLfloat = Double

GLclampf : Type
GLclampf = Double

-- the pointer size of the data len*sizeof(whatever)
GLsizeiptr : Type
GLsizeiptr = Int

class GlEnum a where
  toGlInt   : a -> Int

