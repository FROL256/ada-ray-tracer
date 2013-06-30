with Glib; use Glib;
with Gdk.GC;
with Gdk.Pixbuf;
with Gdk.Rgb;
with Gtk.Drawing_Area;
with Ada.Unchecked_Deallocation;
with Ray_Tracer;

package My_Widget is


  procedure ExtractToGdkPixbuf(screenBuffer : Ray_Tracer.ScreenBufferData;
                               scrBuff : in out Gdk.Pixbuf.Gdk_Pixbuf);


  procedure ConvertGdkPixbufToFloat3Array(scrBuff : in Gdk.Pixbuf.Gdk_Pixbuf;
                                          dstBuff : out Ray_Tracer.AccumBuff;
                                          w : integer; h : integer);


end My_Widget;
