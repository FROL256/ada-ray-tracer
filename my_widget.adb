with Glib;            use Glib;
with Gdk;             use Gdk;
with Gtk;             use Gtk;
with Gdk.Color;       use Gdk.Color;
with Gdk.Drawable;    use Gdk.Drawable;
with Gdk.Event;       use Gdk.Event;
with Gdk.GC;          use Gdk.GC;
with Gtk.Widget;      use Gtk.Widget;
with Gdk.Window;      use Gdk.Window;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Gdk.Rgb;         use Gdk.Rgb;
with Gtk.Object;      use Gtk.Object;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtkada.Types;    use Gtkada.Types;
with Gtk.Main;        use Gtk.Main;
with Glib.Error;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Real_Time;
with Interfaces;
with Ray_Tracer;

use Ada.Integer_Text_IO;
use Ray_Tracer;
use Interfaces;
use Ada.Real_Time;

package body My_Widget is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
  use Float_Functions;

  procedure ExtractToGdkPixbuf(screenBuffer : ScreenBufferData; scrBuff : in out Gdk_Pixbuf) is
    tempColor      : Rgb_Record;
    extractedColor : Unsigned_32;
    ScreenBufferPixels   : Gdk.Rgb.Rgb_Buffer_Access := Get_Pixels(scrBuff);
  begin

    tempColor.Red   := Guchar(0);
    tempColor.Green := Guchar(255);
    tempColor.Blue  := Guchar(0);

    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop

      extractedColor  := screenBuffer(integer(x), integer(height-1-y));

      tempColor.Red   := Guchar(Shift_Right(extractedColor,0)  and 255);
      tempColor.Green := Guchar(Shift_Right(extractedColor,8)  and 255);
      tempColor.Blue  := Guchar(Shift_Right(extractedColor,16) and 255);

      ScreenBufferPixels(Guint(y*width+x)) := tempColor;

      end loop;
    end loop;

  end ExtractToGdkPixbuf;


  procedure ConvertGdkPixbufToFloat3Array(scrBuff : in Gdk_Pixbuf; dstBuff : out AccumBuff; w : integer; h : integer) is
    tempColor : Rgb_Record;
    pixels    : Gdk.Rgb.Rgb_Buffer_Access := Get_Pixels(scrBuff);
  begin

    for y in 0 .. height-1 loop
      for x in 0 .. width-1 loop

        tempColor := pixels(Guint(y*width+x));
        dstBuff(x,height-1-y) := ( (float(tempColor.Red)/255.0)   ** Ray_Tracer.g_gamma,  -- with gamma transform
                                   (float(tempColor.Green)/255.0) ** Ray_Tracer.g_gamma,
                                   (float(tempColor.Blue)/255.0)  ** Ray_Tracer.g_gamma);


      end loop;
    end loop;

  end ConvertGdkPixbufToFloat3Array;



end My_Widget;
