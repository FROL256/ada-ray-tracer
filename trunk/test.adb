with Gtk.Box;          use Gtk.Box;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;         use Gtk.Main;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Window;       use Gtk.Window;
with Gtk.Widget;       use Gtk.Widget;
with Gdk;              use Gdk;
with Gdk.Event;        use Gdk.Event;
with Gdk.Window;

with My_Widget; use My_Widget;
with Text_IO; use Text_IO;

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


with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Real_Time;
with Interfaces;
with Ray_Tracer;

use Ada.Integer_Text_IO;
use Ray_Tracer;
use Interfaces;
use Ada.Real_Time;



procedure Test is

   t1,t2 : Ada.Real_Time.Time;
   temp  : Ada.Real_Time.Time_Span;
   sec, sec2 : Ada.Real_Time.Seconds_Count;
   saveErr : Glib.Error.GError;

   pixbuff : Gdk_Pixbuf := null;
   imgbuff : Gdk_Pixbuf := null;

   counter : integer := 0;
   spp     : integer;
begin

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   pixbuff := Gdk.Pixbuf.Gdk_New (Colorspace      => Colorspace_RGB,
         			  Has_Alpha       => False,
         			  Bits_Per_Sample => 8,
         			  Width           => Gint(Ray_Tracer.width),
         			  Height          => Gint(Ray_Tracer.height));


   -- init renderer
   --
   Ray_Tracer.InitCornellBoxScene;
   Ray_Tracer.ResizeViewport(Ray_Tracer.width, Ray_Tracer.height);


   -- load datasets from files
   --

  --if Ray_Tracer.width = 800 and Ray_Tracer.height = 600 then
   --  g_mltTestImage := new Ray_Tracer.AccumBuff(0..Ray_Tracer.width-1, 0..Ray_Tracer.height-1);
   --  Gdk_New_From_File(imgbuff, "tiger_800x600.jpg", saveErr);
   --  ConvertGdkPixbufToFloat3Array(imgbuff, g_mltTestImage.all, Ray_Tracer.width, Ray_Tracer.height);
   --end if;

   Put_Line("render start");
   Put("threads_num = "); Put(integer'Image(Ray_Tracer.threads_num)); Put_Line("");

   -- main rendering loop
   --
   t1 := Ada.Real_Time.Clock;

   while true loop

     Ray_Tracer.MultiThreadedPathTracing;

     t2 := Ada.Real_Time.Clock;

     spp := Ray_Tracer.GetSPP;

     Split (t1, sec, temp);
     Split (t2, sec2, temp);

     Put("pass "); Put(integer'Image(counter)); Put(" -");
     Put(Integer'Image(Integer(sec2-sec)));
     Put("s elasped. spp = "); Put_Line(integer'Image(spp));

     ExtractToGdkPixbuf(Ray_Tracer.screen_buffer.all, pixbuff);
     Save(pixbuff, "ART_render.PNG", Gdk.Pixbuf.PNG, saveErr);

     counter := counter + 1;

  end loop;



end Test;

