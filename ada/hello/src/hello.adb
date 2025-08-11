with Ada.Text_IO;

procedure Hello is
begin
   for I in Arr'Range (20) loop
      for J in Arr'Range (20) loop
         Ada.TEXT_IO ("#");
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end;
