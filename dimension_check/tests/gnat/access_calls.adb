with Units; use Units;

procedure Access_Calls is
   L : Length := 1.0 * mm;
   F : Length_Function;
begin
   --  GNAT stops with a bug box on a dimension mismatch in a call through an access value, so only correct calls
   --  are compared here. tests/cases/calls.adb checks the mismatches.
   L := F (L);
   L := F.all (L);
end Access_Calls;
