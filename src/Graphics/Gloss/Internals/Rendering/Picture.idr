module Graphics.Gloss.Internals.Rendering.Picture

import Data.IORef

import Graphics.Rendering.Gl.Gl41 as GL

import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Rendering.Bitmap
import Graphics.Gloss.Internals.Rendering.Circle
import Graphics.Gloss.Internals.Rendering.Common 
import Graphics.Gloss.Internals.Rendering.State

{-
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Gloss.Internals.Rendering.Circle
import Graphics.Gloss.Internals.Data.Color
import System.Mem.StableName
import Foreign.ForeignPtr
import Data.IORef
import Data.List
import Control.Monad
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors   as GLU
import qualified Graphics.UI.GLUT                       as GLUT
-}

-- Errors ---------------------------------------------------------------------
handleError : String -> GLenum -> IO ()
handleError place err
  = case err of
      GL_STACK_OVERFLOW
        => print $ unwords
        [ "Gloss / OpenGL Stack Overflow " ++ show place
        , "  This program uses the Gloss vector graphics library, which tried to"
        , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
        , "  than your OpenGL implementation supports. The OpenGL spec requires"
        , "  all implementations to have a transform stack depth of at least 32,"
        , "  and Gloss tries not to push the stack when it doesn't have to, but"
        , "  that still wasn't enough."
        , ""
        , "  You should complain to your harware vendor that they don't provide"
        , "  a better way to handle this situation at the OpenGL API level."
        , ""
        , "  To make this program work you'll need to reduce the number of nested"
        , "  transforms used when defining the Picture given to Gloss. Sorry." ]

      -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
      --   When using GLUT under Windows 7 it complains about InvalidOperation, 
      --   but doesn't provide any other details. All the examples look ok, so 
      --   we're just ignoring the error for now.
      GL_INVALID_OPERATION
        => pure ()
      _ 
        => print $ unwords
        [  "Gloss / OpenGL Internal Error " ++ show place
        ,  "  Please report this on haskell-gloss@googlegroups.com."
        ,  show err ]

checkErrors : String -> IO ()
checkErrors place = do
    es <- allErrors
    when 
      (isCons es)
      (traverse_ (handleError place) es)
  where
    allErrors : IO (List GLenum)
    allErrors = do
      e <- GL.glGetError
      case e of 
        GL_NO_ERROR => pure []
        _           => do
          es <- allErrors
          pure (e :: es) 

-- Utils ----------------------------------------------------------------------
||| Turn alpha blending on or off
setBlendAlpha : Bool -> IO ()
setBlendAlpha True = do
  -- GL.blend        $= GL.Enabled
  -- GL.blendFunc    $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.glEnable GL_BLEND
  GL.glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
setBlendAlpha False = do
  -- GL.blend        $= GL.Disabled
  -- GL.blendFunc    $= (GL.One, GL.Zero)    
  GL.glDisable GL_BLEND
  GL.glBlendFunc GL_ONE GL_ZERO

||| Turn line smoothing on or off
setLineSmooth : Bool -> IO ()
setLineSmooth True  = GL.glEnable GL_LINE_SMOOTH -- GL.lineSmooth $= GL.Enabled
setLineSmooth False = GL.glDisable GL_LINE_SMOOTH -- GL.lineSmooth $= GL.Disabled

||| send 2d vertex coordinates to opengl
vertexPFs : Path -> IO ()
vertexPFs []               = pure ()
vertexPFs ((x, y) :: rest) = do  
  GL.glVertex2f x y
  vertexPFs rest

-- Textures -------------------------------------------------------------------
||| Install a texture into OpenGL.
installTexture :  Int 
               -> Int
               -> BitmapData
               -> Bool
               -> IO Texture
installTexture width height bitmapData cacheMe = do
  let ptr = bitmapPointer bitmapData
{-
  let fmt = bitmapFormat bitmapData
  let glFormat 
          = case pixelFormat fmt of
                  PxABGR => GL_RGBA -- TODO: not sure what to do in this case
                  PxRGBA => GL_RGBA
-}

  -- Allocate texture handle for texture
  (tex :: _) <- GL.glGenTextures 1
  --GL.textureBinding GL.Texture2D $= Just tex
  GL.glBindTexture GL_TEXTURE_2D tex

  -- Sets the texture in imgData as the current texture
  -- This copies the data from the pointer into OpenGL texture memory, 
  -- so it's ok if the foreignptr gets garbage collected after this.
  GL.glTexImage2D
    GL_TEXTURE_2D     -- target
    0                 -- level
    GL_RGBA           -- TODO internal format, what if pixelformat is ABGR??
    (gsizei width)    -- width
    (gsizei height)   -- height
    0                 -- border, must be 0
    GL_RGBA           -- TODO: not sure if this is correct glFormat          -- format of the pixel data
    GL_UNSIGNED_BYTE  -- data type of the pixel data
    ptr               -- the pointer to image data in memory
    {-
      GL.texImage2D
          GL.Texture2D
          GL.NoProxy
          0
          GL.RGBA8
          (GL.TextureSize2D
                  (gsizei width)
                  (gsizei height))
          0
          (GL.PixelData glFormat GL.UnsignedByte ptr)
  -}

  -- Make a stable name that we can use to identify this data again.
  -- If the user gives us the same texture data at the same size then we
  -- can avoid loading it into texture memory again.
  -- TODO name <- makeStableName bitmapData
  let name = 0

  pure $ MkTexture name width height ptr tex cacheMe
  {-
          { texName       = name
          , texWidth      = width
          , texHeight     = height
          , texData       = fptr
          , texObject     = tex
          , texCacheMe    = cacheMe }
-}

||| Load a texture.
|||   If we've seen it before then use the pre-installed one from the texture
|||   cache, otherwise load it into OpenGL.
loadTexture : IORef (List Texture)
            -> Int -> Int -> BitmapData
            -> Bool
            -> IO Texture
loadTexture refTextures width height imgData cacheMe = do   
  textures <- readIORef refTextures
  -- Try and find this same texture in the cache.
  --name <- makeStableName imgData

  let mTexCached
          = find (\tex => texData  tex == bitmapPointer imgData 
                        && texWidth  tex == width
                        && texHeight tex == height)
          textures

  case mTexCached of
    Just tex => pure tex

    Nothing => do
      tex <- installTexture width height imgData cacheMe
      when
        cacheMe
        (writeIORef refTextures (tex :: textures))

      pure tex

||| If this texture does not have its `cacheMe` flag set then delete it from 
|||   OpenGL and free the GPU memory.
freeTexture : Texture -> IO ()
freeTexture tex =
  if texCacheMe tex 
    then pure ()
    else GL.glDeleteTextures 1 [texObject tex] --GL.deleteObjectNames [texObject tex]

polyCoords : RowOrder -> List (Double, Double)
polyCoords BottomToTop = [(0,0), (1,0), (1,1), (0,1)]
polyCoords TopToBottom = [(0,1), (1,1), (1,0), (0,0)]

drawPicture : State -> Double -> Picture -> IO ()         
drawPicture state circScale picture =
  case picture of
      -- nothin'
      Blank
        => pure ()

      -- line
      Line path       
        => do
            GL.glBegin GL_LINE_STRIP
            vertexPFs path
            GL.glEnd 

      -- polygon (where?)
      Polygon path
        => if stateWireframe state
            then do
                  GL.glBegin GL_LINE_LOOP
                  vertexPFs path
                  GL.glEnd
            else do
                  GL.glBegin GL_POLYGON
                  vertexPFs path
                  GL.glEnd

      -- circle
      Circle radius
        => renderCircle 0 0 circScale radius 0
      
      ThickCircle radius thickness
        => renderCircle 0 0 circScale radius thickness
      
      -- arc
      Arc a1 a2 radius
        => renderArc 0 0 circScale radius a1 a2 0
            
      ThickArc a1 a2 radius thickness
        => renderArc 0 0 circScale radius a1 a2 thickness
            
      -- stroke text
      --      text looks weird when we've got blend on,
      --      so disable it during the renderString call.
      Text str 
        => do
          GL.glDisable GL_BLEND
          -- TODO: implement text-rendering preservingMatrix $ GLUT.renderString GLUT.Roman str
          GL.glEnable GL_BLEND

      -- colors with float components.
      Color col p
        => if stateColor state
            then do
              -- oldColor <- get GL.currentColor
              oldColor <- GL.getFloat4 GL_COLOR_CLEAR_VALUE
              let RGBA r g b a  = col
              --GL.currentColor  $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
              GL.glClearColor (gf r) (gf g) (gf b) (gf a)
              drawPicture state circScale p
              --GL.currentColor  $= oldColor   
              let (or, og, ob, oa) = oldColor  
              GL.glClearColor (gf or) (gf og) (gf ob) (gf oa) 
            else drawPicture state circScale p

      -- Translation --------------------------
      -- Easy translations are done directly to avoid calling GL.perserveMatrix.
      Translate posX posY (Circle radius)
        => renderCircle posX posY circScale radius 0

      Translate posX posY (ThickCircle radius thickness)
        => renderCircle posX posY circScale radius thickness

      Translate posX posY (Arc a1 a2 radius)
        => renderArc posX posY circScale radius a1 a2 0

      Translate posX posY (ThickArc a1 a2 radius thickness)
        => renderArc posX posY circScale radius a1 a2 thickness

      Translate tx ty (Rotate deg p)
        => preservingMatrix $ do  
            GL.glTranslated tx ty 0
            GL.glRotated    deg 0 0 (-1)
            drawPicture state circScale p

      Translate tx ty p
        => preservingMatrix $ do
            GL.glTranslated tx ty 0
            drawPicture state circScale p

      -- Rotation -----------------------------
      -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
      Rotate _   (Circle radius)
        => renderCircle   0 0 circScale radius 0

      Rotate _   (ThickCircle radius thickness)
        => renderCircle   0 0 circScale radius thickness

      Rotate deg (Arc a1 a2 radius)
        => renderArc      0 0 circScale radius (a1-deg) (a2-deg) 0

      Rotate deg (ThickArc a1 a2 radius thickness)
        => renderArc      0 0 circScale radius (a1-deg) (a2-deg) thickness

      Rotate deg p
        => preservingMatrix $ do
            GL.glRotated deg 0 0 (-1)
            drawPicture state circScale p

      -- Scale --------------------------------
      Scale sx sy p
        => preservingMatrix $ do
            GL.glScaled sx sy 1
            let mscale = max sx sy
            drawPicture state (circScale * mscale) p
 
      -- Bitmap -------------------------------
      Bitmap width height imgData cacheMe => do  
        -- Load the image data into a texture,
        -- or grab it from the cache if we've already done that before.
        tex <- loadTexture (stateTextures state) width height imgData cacheMe
  
        -- Set up wrap and filtering mode
        --GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
        --GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
        --GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
        GL.glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
        GL.glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
        GL.glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
        GL.glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST

        -- Enable texturing
        -- GL.texture GL.Texture2D $= GL.Enabled
        -- GL.textureFunction      $= GL.Combine
        GL.glEnable GL_TEXTURE_2D
        GL.glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_COMBINE

        -- Set current texture
        --GL.textureBinding GL.Texture2D $= Just (texObject tex)
        GL.glBindTexture GL_TEXTURE_2D (texObject tex)

        -- Set to opaque
        --oldColor <- get GL.currentColor
        --GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
        (or, og, ob, oa) <- GL.getFloat4 GL_COLOR_CLEAR_VALUE
        GL.glClearColor 1 1 1 1

        -- Draw textured polygon
        --GL.renderPrimitive GL.Polygon
        let polyCoords = polyCoords (rowOrder (bitmapFormat imgData))
        let poly = List.zip (bitmapPathd (cast width) (cast height)) polyCoords
        GL.glBegin GL_POLYGON
        {- traverse_ (\((pX, pY), (tX, tY)) => do
          GL.glTexCoord2f (gf tX) (gf tY)
          GL.glVertex2f   (gf pX) (gf pY)) poly
          -}
        GL.glEnd

        -- Restore color
        --GL.currentColor $= oldColor
        GL.glClearColor (gf or) (gf og) (gf ob) (gf oa) 

        -- Disable texturing
        --GL.texture GL.Texture2D $= GL.Disabled
        GL.glDisable GL_TEXTURE_2D

        -- Free uncachable texture objects.
        freeTexture tex
              
      Pictures ps
        => traverse_ (drawPicture state circScale) ps

-- Render a picture into the current OpenGL context.
--
--   Assumes that the OpenGL matrix mode is set to @Modelview@
--
-- @ state     Current rendering state.
-- @ circScale View port scale, which controls the level of detail. Use 1.0 to start with.
-- @ picture   Picture to render.
export
renderPicture :  (state : State) 
              -> (circScale : Double)
              -> (picture : Picture)
              -> IO ()
renderPicture state circScale picture = do   
  -- Setup render state for world
  setLineSmooth (stateLineSmooth state)
  setBlendAlpha (stateBlendAlpha state)
  
  -- Draw the picture
  checkErrors "before drawPicture."
  drawPicture state circScale picture
  checkErrors "after drawPicture."