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

   package Target_Cb is new Gtk.Handlers.User_Callback (Target_Widget_Record, String);
   package Window_Cb is new Gtk.Handlers.Return_Callback (Gtk_Window_Record, Boolean);

   function On_Main_Window_Delete_Event(Object : access Gtk_Window_Record'Class) return Boolean;
   --  Callback for delete_event.

   function On_Main_Window_Delete_Event(Object : access Gtk_Window_Record'Class) return Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Gtk_Exit (0);
      return True;
   end On_Main_Window_Delete_Event;

   procedure Won
     (Widget  : access Target_Widget_Record'Class;
      Message : in     String);
   procedure Won
     (Widget  : access Target_Widget_Record'Class;
      Message : in     String)
   is
      pragma Unreferenced (Widget);
   begin
      Text_IO.Put_Line (Message);
   end Won;

   --Main_W : Gtk_Window;
   --Ok     : Target_Widget;
   --Box    : Gtk_Box;

   t1,t2 : Ada.Real_Time.Time;
   temp  : Ada.Real_Time.Time_Span;
   sec, sec2 : Ada.Real_Time.Seconds_Count;
   saveErr : Glib.Error.GError;

   pixbuff : Gdk_Pixbuf := null;

   counter : integer := 0;
   spp     : integer;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   --Gtk_New (Main_W, Window_Toplevel);
   --Gtk_New_Vbox (Box, False, 0);
   --Add (Main_W, Box);
   --Gtk_New (Ok);
   --Pack_Start (Box, Ok, True, True);
   --Window_Cb.Connect (Main_W, "delete_event", Window_Cb.To_Marshaller (On_Main_Window_Delete_Event'Access));
   --Show_All (Main_W);
   --Gtk.Main.Main;

   pixbuff := Gdk.Pixbuf.Gdk_New (Colorspace      => Colorspace_RGB,
         			  Has_Alpha       => False,
         			  Bits_Per_Sample => 8,
         			  Width           => Gint(Ray_Tracer.width),
         			  Height          => Gint(Ray_Tracer.height));

   Ray_Tracer.InitCornellBoxScene;

   Ray_Tracer.ResizeViewport(Ray_Tracer.width, Ray_Tracer.height);

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

     ExtractToGdkPixbuf(pixbuff);
     Save(pixbuff, "ART_render.PNG", Gdk.Pixbuf.PNG, saveErr);

     counter := counter + 1;

   end loop;

end Test;

