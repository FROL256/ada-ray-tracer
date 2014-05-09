with Interfaces;
with Ada.Streams.Stream_IO;

use Interfaces;
use Ada.Streams.Stream_IO;


package body Bitmap is

  procedure Init(im : out Image; w : Integer; h : Integer) is
  begin
    im.width  := w;
    im.height := h;
    im.data   := new PixelData(0 .. w*h-1);
  end Init;

  procedure Delete(im : in out Image) is
  begin
    im.width  := 0;
    im.height := 0;
    delete(im.data);
    im.data   := null;
  end Delete;

  procedure LoadBMP(im : in out Image; a_fileName : String) is
  begin
    null;
  end LoadBMP;


  procedure SaveBMP(im : Image; a_fileName : String) is

    BMP_File : File_Type;
    S        : Stream_Access;

    header   : BITBAPFILEHEADER;
    info     : BITMAPINFOHEADER;
    px       : Pixel;
    pxU      : Unsigned_32;

  begin

    header.bfType      := 16#4d42#;
    header.bfSize      := 14 + 40 + DWORD(im.width*im.height*3);
    header.bfReserved1 := 0;
    header.bfReserved2 := 0;
    header.bfOffBits   := 14 + 40;

    info.biSize          := 40;
    info.biWidth         := LONG(im.width);
    info.biHeight        := LONG(im.height);
    info.biPlanes        := 1;
    info.biBitCount      := 24;
    info.biCompression   := 0;
    info.biSizeImage     := 0;
    info.biXPelsPerMeter := 0;
    info.biYPelsPerMeter := 0;
    info.biClrUsed       := 0;
    info.biClrImportant  := 0;

    Create(File => BMP_File,
           Mode => Out_File,
           Name => a_fileName,
           Form => "");

    S := Stream(BMP_File);

    BITBAPFILEHEADER'Write(S, header);
    BITMAPINFOHEADER'Write(S, info);

    for i in im.data'First .. im.data'Last loop

      pxU  := im.data(i);

      px.b := Unsigned_8(Shift_Right(pxU,0)  and 255);
      px.g := Unsigned_8(Shift_Right(pxU,8)  and 255);
      px.r := Unsigned_8(Shift_Right(pxU,16) and 255);

      Pixel'Write(S, px);

    end loop;

    Close(BMP_File);

  end SaveBMP;

end Bitmap;
