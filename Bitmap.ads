with Interfaces;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

use Interfaces;
use Ada.Streams.Stream_IO;

package Bitmap is

  type PixelData is array (Integer range <>) of Unsigned_32;
  type PixelDataRef is access PixelData;

  procedure delete is new Ada.Unchecked_Deallocation(Object => PixelData, Name => PixelDataRef);

  type Image is record
    width  : Integer;
    height : Integer;
    data   : PixelDataRef;
  end record;

  procedure Init(im : out Image; w : Integer; h : Integer);
  procedure Delete(im : in out Image);

  procedure LoadBMP(im : in out Image; a_fileName : String);
  procedure SaveBMP(im : Image; a_fileName : String);

private

  subtype WORD  is Unsigned_16;
  subtype DWORD is Unsigned_32;

  type Pixel is record
    r,g,b : Unsigned_8;
  end record;


  type BITBAPFILEHEADER is record
    bfType      : WORD;
    bfSize      : DWORD;
    bfReserved1 : WORD;
    bfReserved2 : WORD;
    bfOffBits   : DWORD;
  end record;

  type BITMAPINFOHEADER is record
    biSize          : DWORD;
    biWidth         : DWORD;
    biHeight        : DWORD;
    biPlanes        : WORD;
    biBitCount      : WORD;
    biCompression   : DWORD;
    biSizeImage     : DWORD;
    biXPelsPerMeter : DWORD;
    biYPelsPerMeter : DWORD;
    biClrUsed       : DWORD;
    biClrImportant  : DWORD;
  end record;

end Bitmap;
