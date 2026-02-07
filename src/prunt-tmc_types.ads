--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

pragma Extensions_Allowed (On);

package Prunt.TMC_Types is

   type TMC_Boolean is new Boolean with Size => 1;
   for TMC_Boolean use (False => 0, True => 1);

   type Unsigned_1 is range 0 .. 2**1 - 1 with Size => 1, Annotate => (Prunt_Config, User_Config);
   type Unsigned_2 is range 0 .. 2**2 - 1 with Size => 2, Annotate => (Prunt_Config, User_Config);
   type Unsigned_3 is range 0 .. 2**3 - 1 with Size => 3, Annotate => (Prunt_Config, User_Config);
   type Unsigned_4 is range 0 .. 2**4 - 1 with Size => 4, Annotate => (Prunt_Config, User_Config);
   type Unsigned_5 is range 0 .. 2**5 - 1 with Size => 5, Annotate => (Prunt_Config, User_Config);
   type Unsigned_6 is range 0 .. 2**6 - 1 with Size => 6, Annotate => (Prunt_Config, User_Config);
   type Unsigned_7 is range 0 .. 2**7 - 1 with Size => 7, Annotate => (Prunt_Config, User_Config);
   type Unsigned_8 is range 0 .. 2**8 - 1 with Size => 8, Annotate => (Prunt_Config, User_Config);
   type Unsigned_9 is range 0 .. 2**9 - 1 with Size => 9, Annotate => (Prunt_Config, User_Config);
   type Unsigned_10 is range 0 .. 2**10 - 1 with Size => 10, Annotate => (Prunt_Config, User_Config);
   type Unsigned_11 is range 0 .. 2**11 - 1 with Size => 11, Annotate => (Prunt_Config, User_Config);
   type Unsigned_12 is range 0 .. 2**12 - 1 with Size => 12, Annotate => (Prunt_Config, User_Config);
   type Unsigned_13 is range 0 .. 2**13 - 1 with Size => 13, Annotate => (Prunt_Config, User_Config);
   type Unsigned_14 is range 0 .. 2**14 - 1 with Size => 14, Annotate => (Prunt_Config, User_Config);
   type Unsigned_15 is range 0 .. 2**15 - 1 with Size => 15, Annotate => (Prunt_Config, User_Config);
   type Unsigned_16 is range 0 .. 2**16 - 1 with Size => 16, Annotate => (Prunt_Config, User_Config);
   type Unsigned_17 is range 0 .. 2**17 - 1 with Size => 17, Annotate => (Prunt_Config, User_Config);
   type Unsigned_18 is range 0 .. 2**18 - 1 with Size => 18, Annotate => (Prunt_Config, User_Config);
   type Unsigned_19 is range 0 .. 2**19 - 1 with Size => 19, Annotate => (Prunt_Config, User_Config);
   type Unsigned_20 is range 0 .. 2**20 - 1 with Size => 20, Annotate => (Prunt_Config, User_Config);
   type Unsigned_21 is range 0 .. 2**21 - 1 with Size => 21, Annotate => (Prunt_Config, User_Config);
   type Unsigned_22 is range 0 .. 2**22 - 1 with Size => 22, Annotate => (Prunt_Config, User_Config);
   type Unsigned_23 is range 0 .. 2**23 - 1 with Size => 23, Annotate => (Prunt_Config, User_Config);
   type Unsigned_24 is range 0 .. 2**24 - 1 with Size => 24, Annotate => (Prunt_Config, User_Config);
   type Unsigned_25 is range 0 .. 2**25 - 1 with Size => 25, Annotate => (Prunt_Config, User_Config);
   type Unsigned_26 is range 0 .. 2**26 - 1 with Size => 26, Annotate => (Prunt_Config, User_Config);
   type Unsigned_27 is range 0 .. 2**27 - 1 with Size => 27, Annotate => (Prunt_Config, User_Config);
   type Unsigned_28 is range 0 .. 2**28 - 1 with Size => 28, Annotate => (Prunt_Config, User_Config);
   type Unsigned_29 is range 0 .. 2**29 - 1 with Size => 29, Annotate => (Prunt_Config, User_Config);
   type Unsigned_30 is range 0 .. 2**30 - 1 with Size => 30, Annotate => (Prunt_Config, User_Config);
   type Unsigned_31 is range 0 .. 2**31 - 1 with Size => 31, Annotate => (Prunt_Config, User_Config);
   type Unsigned_32 is range 0 .. 2**32 - 1 with Size => 32, Annotate => (Prunt_Config, User_Config);

end Prunt.TMC_Types;
