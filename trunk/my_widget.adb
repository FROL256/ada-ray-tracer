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



package body My_Widget is

   old_width, old_height : Gint := Gint(0);
   ScreenBuffer          : Gdk_Pixbuf := null;
   ScreenBufferPixels    : Gdk.Rgb.Rgb_Buffer_Access := null;

   Class_Record : GObject_Class := Uninitialized_Class;

   --  Array of the signals created for this widget
   Signals : Chars_Ptr_Array := "bullseye" + "missed";

   package Internal_Cb is new Handlers.Callback (Target_Widget_Record);
   package Return_Boolean_Cb is new Handlers.Return_Callback (Target_Widget_Record, Boolean);

   --  Define our own marshaller, since this is not one of the
   --  standard one.
   package Size_Cb is new Handlers.Callback (Target_Widget_Record);
   package Requisition_Marshaller is new Size_Cb.Marshallers.Generic_Marshaller(Gtk_Requisition_Access, Gtk.Widget.Get_Requisition);

   package Allocation_Cb is new Handlers.Callback (Target_Widget_Record);
   package Allocation_Marshaller is new Allocation_Cb.Marshallers.Generic_Marshaller (Gtk_Allocation_Access, Gtk.Widget.Get_Allocation);

   procedure UpdateScreen(Widget  : access Target_Widget_Record'Class;
                          Win  : Gdk.Window.Gdk_Window;
                          ScreenBuffer : Gdk_Pixbuf;
                          ScreenBufferPixels : Gdk.Rgb.Rgb_Buffer_Access;
                          width, height : Gint);

   procedure UpdateScreen(Widget  : access Target_Widget_Record'Class;
                          Win  : Gdk.Window.Gdk_Window;
                          ScreenBuffer : Gdk_Pixbuf;
                          ScreenBufferPixels : Gdk.Rgb.Rgb_Buffer_Access;
                          width, height : Gint) is

      tempColor      : Rgb_Record;
      extractedColor : Unsigned_32;
   begin

      tempColor.Red   := Guchar(0);
      tempColor.Green := Guchar(255);
      tempColor.Blue  := Guchar(0);

      for y in 0 .. height-1 loop
        for x in 0 .. width-1 loop

          extractedColor  := Ray_Tracer.screen_buffer(integer(x), integer(height-1-y));

          tempColor.Red   := Guchar(Shift_Right(extractedColor,0)  and 255);
          tempColor.Green := Guchar(Shift_Right(extractedColor,8)  and 255);
          tempColor.Blue  := Guchar(Shift_Right(extractedColor,16) and 255);

          ScreenBufferPixels(Guint(y*width+x)) := tempColor;

        end loop;
      end loop;

      Render_To_Drawable ( Pixbuf => ScreenBuffer,
			   Drawable => Win,
			   GC => Widget.Gc_Out,
			   Src_X  => Gint(0),
			   Src_Y  => Gint(0),
			   Dest_X => Gint(0),
			   Dest_Y => Gint(0),
			   Width  => Gint(width),
			   Height => Gint(height)
                         );
   end;

   -----------------
   -- Draw_Target --
   -----------------

   --subtype MyBitMap is Gdk.Bitmap;

   function Draw_Target
     (Widget  : access Target_Widget_Record'Class)
      return Boolean;

   function Draw_Target
     (Widget  : access Target_Widget_Record'Class)
      return Boolean
      --  This function is called when we need to redraw the widget (for
      --  instance whenever part of it has been cleared
   is
      width, height : Gint := Gint(512);
      Win   : Gdk.Window.Gdk_Window := Get_Window (Widget);
      t1,t2 : Ada.Real_Time.Time;
      temp  : Ada.Real_Time.Time_Span;
      sec, sec2 : Ada.Real_Time.Seconds_Count;

   begin

      Gdk.Drawable.Get_Size (Win, width, height);

      width := width - (width mod 4); -- a hack, need to understand what happen

      if old_width = width and old_height = height then
        UpdateScreen(Widget => Widget, Win => Win, ScreenBuffer => ScreenBuffer, ScreenBufferPixels => ScreenBufferPixels, width => width, height => height);
        return true;
      end if;

      old_width  := width;
      old_height := height;

      delete(ScreenBuffer);
      ScreenBuffer := Gdk.Pixbuf.Gdk_New (Colorspace      => Colorspace_RGB,
         				  Has_Alpha       => False,
         				  Bits_Per_Sample => 8,
         				  Width           => width,
         				  Height          => height);

      ScreenBufferPixels := Get_Pixels(ScreenBuffer);

      Ray_Tracer.ResizeViewport(integer(width), integer(height));

      Put_Line ("init scene");
      Ray_Tracer.InitScene;

      Put("rendering, res ="); Put(Gint'Image(width)); Put(" x"); Put_line(Gint'Image(height));

      t1 := Ada.Real_Time.Clock;

      Ray_Tracer.MultiThreadedRayTracing;

      t2 := Ada.Real_Time.Clock;

      Ada.Text_IO.Put("rendering time ~ ");
      Split (t1, sec, temp);
      Split (t2, sec2, temp);
      Put(Integer'Image(Integer(sec2-sec)));
      Put_Line (" secons;");

      UpdateScreen(Widget => Widget, Win => Win, ScreenBuffer => ScreenBuffer, ScreenBufferPixels => ScreenBufferPixels, width => width,height => height);

      declare
        tempErr : Glib.Error.GError;
      begin
        Save(ScreenBuffer, "ART_render.PNG", Gdk.Pixbuf.PNG, tempErr);
      end;

      Put_Line("Image saved");

      return true;

   end Draw_Target;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request (Widget : access Target_Widget_Record'Class; Requisition : in Gtk_Requisition_Access);
   procedure Size_Request (Widget : access Target_Widget_Record'Class; Requisition : in Gtk_Requisition_Access) is
   begin
      Requisition.Width  := 800;
      Requisition.Height := 600;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end Size_Request;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Target_Widget) is
      --  Used to create a new widget
   begin
      Widget := new Target_Widget_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Target_Widget_Record'Class) is
   begin

      Gtk.Drawing_Area.Initialize (Widget);
      Gtk.Object.Initialize_Class_Record(Widget, Signals, Class_Record, "TestGtkTargetWidget");

      --  Set up the appropriate callbacks to redraw, ...
      Return_Boolean_Cb.Connect (Widget, "expose_event", Return_Boolean_Cb.To_Marshaller (Draw_Target'Access), True);
      Size_Cb.Connect (Widget, "size_request", Requisition_Marshaller.To_Marshaller (Size_Request'Access));

   end Initialize;

end My_Widget;
