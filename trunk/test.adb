with Text_IO;
with Ada.Text_IO;


with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Real_Time;
with Interfaces;
with Ray_Tracer;
with Bitmap;

use Ada.Integer_Text_IO;
use Ray_Tracer;
use Interfaces;
use Ada.Real_Time;
use Ada.Text_IO;
use Text_IO;



procedure Test is

   t1,t2    : Ada.Real_Time.Time;
   temp     : Ada.Real_Time.Time_Span;
   sec,sec2 : Ada.Real_Time.Seconds_Count;

   counter : integer := 0;
   spp     : integer;
   image   : Bitmap.Image; -- image for saving screen buffer to file

begin

   -- init renderer
   --
   Ray_Tracer.InitCornellBoxScene;
   Ray_Tracer.ResizeViewport(Ray_Tracer.width, Ray_Tracer.height);

   Bitmap.Init(image, Ray_Tracer.width, Ray_Tracer.height);

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

     -- copy frame to image and update file on disk
     --
     for y in 0 .. Ray_Tracer.height-1 loop
       for x in 0 .. Ray_Tracer.width-1 loop
         image.data(y*Ray_Tracer.width + x) := Ray_Tracer.screen_buffer(x,y);
       end loop;
     end loop;

     Bitmap.SaveBMP(image, "ART_render.bmp");

     counter := counter + 1;

  end loop;

  Bitmap.Delete(image);

end Test;

