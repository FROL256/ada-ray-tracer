with Gtk.Box;          use Gtk.Box;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;         use Gtk.Main;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Window;       use Gtk.Window;

with My_Widget; use My_Widget;
with Text_IO;

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

   Main_W : Gtk_Window;
   Ok     : Target_Widget;
   Box    : Gtk_Box;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New_Vbox (Box, False, 0);
   Add (Main_W, Box);

   Gtk_New (Ok);
   Pack_Start (Box, Ok, True, True);

   Window_Cb.Connect (Main_W, "delete_event", Window_Cb.To_Marshaller (On_Main_Window_Delete_Event'Access));

   Show_All (Main_W);

   Gtk.Main.Main;
end Test;

