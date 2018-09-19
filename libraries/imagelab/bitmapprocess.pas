{
This unit contains the most of the procedures which are used
for processing images.

Copyright (C) 2003 - 2011 Strikos Nikolaos

This file is part of Digital Image Magnifier.

Digital Image Magnifier is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Digital Image Magnifier is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

}

unit BitmapProcess;

interface

uses
     Math, BGRABitmap, BGRABitmapTypes;

     type ImageArray=array of array of single;

     type BitmapPointer = ^TBGRABitmap;

     procedure BGRAPixelRepetition (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABilinear (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABicubic (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRABicubicPolyrama (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRAPolyrama4(Input, Output:TBGRABitmap);
     procedure BGRABicubicPolyrama6 (Input, Output : BitmapPointer; fm: Single;
                               first, last: Integer);
     procedure BGRABicubicPolyrama7 (Input : TBGRABitmap; Output : BitmapPointer; fm: Single;
                               first, last: Integer);
     procedure BGRABicubicCatmullRom (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
     procedure BGRAInvertColors(Input : TBGRABitmap);
     procedure BGRARemap (Input : TBGRABitmap; Output : TBGRABitmap; width, height : Integer);
     procedure BGRAUnsharp3(Input : TBGRABitmap; sigma, f: Single);
     procedure Gauss(sigma: Single; Width: Integer; var Kernel: ImageArray);
     procedure BGRASetAlpha(Input : TBGRABitmap);

     function c(x:single):single;         //bicubic filter
     function poly3(x:single):single;  //this function implements a different bicubic filter of panorama tools
     function catmullrom(x:single):single;  //this function implements a different bicubic filter of panorama tools
     function s(x:single):single;         //quadratic filter
     function hf(x:single):single;        //Hermite filter
     function sinc(x:single):single;      //sinc function
     function Lancf(x,a:single):single;     //Lanc filter
     function mf(x:single):single;        //Mitchel filter

var
  percent : Single;

implementation

procedure BGRAPixelRepetition (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   i,j:int64;
   k,l:integer;
   p, pOutput : PBGRAPixel;
begin
  for l := 0 to Output.Height-1 do
  begin
    y:=(l+0.5)/fm;
    i:=trunc(y);
    if i>=Input.Height then
       i:=Input.Height-1;
    pOutput:=Output.ScanLine[l];
    p := Input.ScanLine[i];
    for k := 0 to Output.Width-1 do
    begin
      x:=(k+0.5)/fm;
      j:=trunc(x);
      if j>=Input.Width then
         j:=Input.Width-1;
      pOutput[k].red:=p[j].red;
      pOutput[k].green:=p[j].green;
      pOutput[k].blue:=p[j].blue;
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABilinear (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j, j1, j2:integer;
   k,l:integer;
   p1, p2, pOutput : PBGRAPixel;
begin
  for l := 0 to Output.Height-1 do
  begin
    y := l/fm;
    i:=trunc(y);
    a:=y-i;

    p1 := Input.ScanLine[i];

    //take care of border pixels
    if i < Input.Height-1 then
        p2 := Input.ScanLine[i+1]
    else
        p2 := Input.ScanLine[i];

    pOutput:=Output.ScanLine[l];

    for k := 0 to Output.Width-1 do
    begin
      x := k/fm;
      j:=trunc(x);
      b:=x-j;
      j1 := j;

      //take care of border pixels
      if j < Input.Width-1 then
         j2 := j+1
      else
         j2 := j;

      pOutput[k].red:=Round(p1[j1].red*(1-a)*(1-b) + p1[j2].red*(1-a)*b + p2[j1].red*a*(1-b) + p2[j2].red*a*b);
      pOutput[k].green:=Round(p1[j1].green*(1-a)*(1-b) + p1[j2].green*(1-a)*b + p2[j1].green*a*(1-b) + p2[j2].green*a*b);
      pOutput[k].blue:=Round(p1[j1].blue*(1-a)*(1-b) + p1[j2].blue*(1-a)*b + p2[j1].blue*a*(1-b) + p2[j2].blue*a*b);
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

//need to take care of border pixels
procedure BGRABicubic (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j:int64;
   k,l:integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;

begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=c(1+a);
      ca[k][1]:=c(a);
      ca[k][2]:=c(1-a);
      ca[k][3]:=c(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    if i>=Input.Height-3 then
       i:=Input.Height-4;
    if i<1 then i:=1;
    pOutput:=Output.ScanLine[l];
    p0 := Input.ScanLine[i-1];
    p1 := Input.ScanLine[i];
    p2 := Input.ScanLine[i+1];
    p3 := Input.ScanLine[i+2];
    c5:=c(1+b);
    c6:=c(b);
    c7:=c(1-b);
    c8:=c(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];
      if j>=Input.Width-1 then
         j:=Input.Width-2;
      pOutput[k].red:= Round(c5*(p0[j-1].red*c1 + p0[j].red*c2 + p0[j+1].red*c3 + p0[j+2].red*c4)
                           + c6*(p1[j-1].red*c1 + p1[j].red*c2 + p1[j+1].red*c3 + p1[j+2].red*c4)
                           + c7*(p2[j-1].red*c1 + p2[j].red*c2 + p2[j+1].red*c3 + p2[j+2].red*c4)
                           + c8*(p3[j-1].red*c1 + p3[j].red*c2 + p3[j+1].red*c3 + p3[j+2].red*c4));

      pOutput[k].green:= Round(c5*(p0[j-1].green*c1 + p0[j].green*c2 + p0[j+1].green*c3 + p0[j+2].green*c4)
                             + c6*(p1[j-1].green*c1 + p1[j].green*c2 + p1[j+1].green*c3 + p1[j+2].green*c4)
                             + c7*(p2[j-1].green*c1 + p2[j].green*c2 + p2[j+1].green*c3 + p2[j+2].green*c4)
                             + c8*(p3[j-1].green*c1 + p3[j].green*c2 + p3[j+1].green*c3 + p3[j+2].green*c4));

      pOutput[k].blue:= Round(c5*(p0[j-1].blue*c1 + p0[j].blue*c2 + p0[j+1].blue*c3 + p0[j+2].blue*c4)
                            + c6*(p1[j-1].blue*c1 + p1[j].blue*c2 + p1[j+1].blue*c3 + p1[j+2].blue*c4)
                            + c7*(p2[j-1].blue*c1 + p2[j].blue*c2 + p2[j+1].blue*c3 + p2[j+2].blue*c4)
                            + c8*(p3[j-1].blue*c1 + p3[j].blue*c2 + p3[j+1].blue*c3 + p3[j+2].blue*c4));
      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j1].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j1].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j1].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j1].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j1].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j1].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j1].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j1].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  Output.InvalidateBitmap; // changed by direct access
  end;
end;

procedure BGRABicubicCatmullRom (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=catmullrom(1+a);
      ca[k][1]:=catmullrom(a);
      ca[k][2]:=catmullrom(1-a);
      ca[k][3]:=catmullrom(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output.ScanLine[l];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=catmullrom(1+b);
    c6:=catmullrom(b);
    c7:=catmullrom(1-b);
    c8:=catmullrom(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j1].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j1].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j1].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j1].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j1].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j1].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j1].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j1].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRAPolyrama4(Input, Output:TBGRABitmap);
var
   i, j: Integer;
   l, k : Integer;
   x, y: Single;
   sum: Single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   rgb : Integer;
   filterColumn, filterRow : Integer;
   p0, p1, p2, p3 : PBGRAPixel;
   p,pOutput : array of PBGRAPixel;
   mykernel : array of array of Single;
begin

     SetLength(myKernel, 4, 4);

     myKernel[0][0]:=0.0;
     myKernel[0][1]:=1;
     myKernel[0][2]:=0;
     myKernel[0][3]:=0;

     myKernel[1][0]:=-0.10546875;
     myKernel[1][1]:=0.87890625;
     myKernel[1][2]:=0.26171875;
     myKernel[1][3]:=-0.03515625;

     myKernel[2][0]:=-0.09375;
     myKernel[2][1]:=0.59375;
     myKernel[2][2]:=0.59375;
     myKernel[2][3]:=-0.09375;

     myKernel[3][0]:=myKernel[1][3];
     myKernel[3][1]:=myKernel[1][2];
     myKernel[3][2]:=myKernel[1][1];
     myKernel[3][3]:=myKernel[1][0];

     filterColumn := 0;
     filterRow := 0;

     SetLength(p, Input.Height);
     SetLength(pOutput, Output.Height);

     for i := 0 to Input.Height - 1 do
         p[i]:=Input.ScanLine[i];
     for i := 0 to Output.Height - 1 do
         pOutput[i] := Output.ScanLine[i];

     for l := 10 to Output.Height - 10 do
     begin
          y:=l/4+1;
          i:=trunc(y);

          if (i > 0) then p0 := pOutput[i-1]
          else p0 := pOutput[0];

          p1 := pOutput[i];

          if ( i < Output.Height - 1) then p2 := pOutput[i+1]
          else p2 := pOutput[Output.Height - 1];

          if (i < Output.Height - 2) then p3 := pOutput[i+2]
          else p3 := pOutput[Output.Height - 1];


          c5:=myKernel[filterRow][0];
          c6:=myKernel[filterRow][1];
          c7:=myKernel[filterRow][2];
          c8:=myKernel[filterRow][3];
          filterRow := filterRow + 1;
          if filterRow > 3 then filterRow := 0;

          for k := 0 to Output.Width - 1 do
          begin
               x:=k/4+1;
               j:=trunc(x);
               c1:=myKernel[filterColumn][0];
               c2:=myKernel[filterColumn][1];
               c3:=myKernel[filterColumn][2];
               c4:=myKernel[filterColumn][3];
               filterColumn := filterColumn + 1;
               if filterColumn > 3 then filterColumn := 0;

               //for rgb := 0 to 2 do
               //begin
                  sum := c5*(p0[j-1].red*c1+    //The bicubic interpolation occurs here.
                             p0[j].red*c2+
                             p0[j+1].red*c3+
                             p0[j+2].red*c4)+

                         c6*(p1[j-1].red*c1+
                             p1[j].red*c2+
                             p1[j+1].red*c3+
                             p1[j+2].red*c4)+

                         c7*(p2[j-1].red*c1+
                             p2[j].red*c2+
                             p2[j+1].red*c3+
                             p2[j+2].red*c4)+

                         c8*(p3[j-1].red*c1+
                             p3[j].red*c2+
                             p3[j+1].red*c3+
                             p3[j+2].red*c4);

                    if sum > 255 then sum := 255;
                    if sum < 0 then sum := 0;

                    pOutput[l][k].red := round(sum);
                    pOutput[l][k].green := 255;
                    pOutput[l][k].blue := 255;
               //end;
          end;
     end;


     Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama6 (Input, Output : BitmapPointer; fm: Single;
                               first, last: Integer);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output^.Width, 4);

   for k := 0 to Output^.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := first to last - 1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output^.ScanLine[l];

    //take care of border pixels
    if i>0 then
    	p0 := Input^.ScanLine[i-1]
    else
    	p0 := Input^.ScanLine[0];

    p1 := Input^.ScanLine[i];

    if i<Input^.Height-1 then p2 := Input^.ScanLine[i+1]
    else p2 := Input^.ScanLine[Input^.Height-1];

    if i<Input^.Height-2 then p3 := Input^.ScanLine[i+2]
    else p3 := Input^.ScanLine[Input^.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := 0 to Output^.Width - 1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input^.Width-1 then j3:=j+1
      else j3:=Input^.Width-1;

      if j<Input^.Width-2 then j4:=j+2
      else j4:=Input^.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output^.InvalidateBitmap; // changed by direct access
end;

procedure BGRABicubicPolyrama7 (Input : TBGRABitmap; Output : BitmapPointer; fm: Single;
                          first, last: Integer);
var
   x,y : single;
   a,b : single;
   i,j : int64;
   k,l : integer;
   j1, j3, j4 : integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   percent := 0;
   SetLength(ca, Output^.Width, 4);

   for k := 0 to Output^.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=poly3(1+a);
      ca[k][1]:=poly3(a);
      ca[k][2]:=poly3(1-a);
      ca[k][3]:=poly3(2-a);
   end;

  for l := first to last - 1 do
  begin

    percent := (l - first) / (last - first) * 100;

    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    pOutput:=Output^.ScanLine[l];

    //take care of border pixels
    if i>0 then
    	p0 := Input.ScanLine[i-1]
    else
    	p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];

    //calculate factors
    c5:=poly3(1+b);
    c6:=poly3(b);
    c7:=poly3(1-b);
    c8:=poly3(2-b);
    for k := 0 to Output^.Width - 1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];

      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;

      Sum :=  Round(c5*(p0[j1].red*c1 + p0[j].red*c2 + p0[j3].red*c3 + p0[j4].red*c4)
                  + c6*(p1[j1].red*c1 + p1[j].red*c2 + p1[j3].red*c3 + p1[j4].red*c4)
                  + c7*(p2[j1].red*c1 + p2[j].red*c2 + p2[j3].red*c3 + p2[j4].red*c4)
                  + c8*(p3[j1].red*c1 + p3[j].red*c2 + p3[j3].red*c3 + p3[j4].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j].green*c1 + p0[j].green*c2 + p0[j3].green*c3 + p0[j4].green*c4)
                 + c6*(p1[j].green*c1 + p1[j].green*c2 + p1[j3].green*c3 + p1[j4].green*c4)
                 + c7*(p2[j].green*c1 + p2[j].green*c2 + p2[j3].green*c3 + p2[j4].green*c4)
                 + c8*(p3[j].green*c1 + p3[j].green*c2 + p3[j3].green*c3 + p3[j4].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j].blue*c1 + p0[j].blue*c2 + p0[j3].blue*c3 + p0[j4].blue*c4)
                 + c6*(p1[j].blue*c1 + p1[j].blue*c2 + p1[j3].blue*c3 + p1[j4].blue*c4)
                 + c7*(p2[j].blue*c1 + p2[j].blue*c2 + p2[j3].blue*c3 + p2[j4].blue*c4)
                 + c8*(p3[j].blue*c1 + p3[j].blue*c2 + p3[j3].blue*c3 + p3[j4].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output^.InvalidateBitmap; // changed by direct access
end;

//need to take care of border pixels
{procedure BGRABicubicCatmullRom (Input : TBGRABitmap ; fm:extended; Output : TBGRABitmap);
var
   x,y:single;
   a,b:single;
   i,j:int64;
   k,l:integer;
   j1, j3, j4 : Integer;
   p0, p1, p2, p3, pOutput : PBGRAPixel;
   ca : array of array of single;
   c1,c2,c3,c4,c5,c6,c7,c8:single;
   sum :single;
begin

   SetLength(ca, Output.Width, 4);

   for k := 0 to Output.Width - 1 do    //We calculate the factors of the multiplication
   begin                                //for each row. They remain the same, so we don't have to
      x:=(k/fm)+1;                       //calculate them again for each pixel
      j:=trunc(x);
      a:=x-j;
      ca[k][0]:=catmullrom(1+a);
      ca[k][1]:=catmullrom(a);
      ca[k][2]:=catmullrom(1-a);
      ca[k][3]:=catmullrom(2-a);
   end;

  for l := 0 to Output.Height-1 do
  begin
    y:= l/fm;
    i:=trunc(y);
    b:=y-i;
    if i>=Input.Height-3 then
       i:=Input.Height-4;
    if i<1 then i:=1;
    pOutput:=Output.ScanLine[l];
    p0 := Input.ScanLine[i-1];
    p1 := Input.ScanLine[i];
    p2 := Input.ScanLine[i+1];
    p3 := Input.ScanLine[i+2];

    //take care of border pixels
    if i>0 then p0 := Input.ScanLine[i-1]
    else p0 := Input.ScanLine[0];

    p1 := Input.ScanLine[i];

    if i<Input.Height-1 then p2 := Input.ScanLine[i+1]
    else p2 := Input.ScanLine[Input.Height-1];

    if i<Input.Height-2 then p3 := Input.ScanLine[i+2]
    else p3 := Input.ScanLine[Input.Height-1];


    c5:=catmullrom(1+b);
    c6:=catmullrom(b);
    c7:=catmullrom(1-b);
    c8:=catmullrom(2-b);
    for k := 0 to Output.Width-1 do
    begin
      x:= k/fm;
      j:=trunc(x);
      c1:=ca[k][0];
      c2:=ca[k][1];
      c3:=ca[k][2];
      c4:=ca[k][3];
      //if j>=Input.Width-1 then
      //   j:=Input.Width-2;


      //take care of border pixels
      if j>0 then j1:=j-1
      else j1:=0;

      if j<Input.Width-1 then j3:=j+1
      else j3:=Input.Width-1;

      if j<Input.Width-2 then j4:=j+2
      else j4:=Input.Width-1;


      Sum :=  Round(c5*(p0[j-1].red*c1 + p0[j].red*c2 + p0[j+1].red*c3 + p0[j+2].red*c4)
                           + c6*(p1[j-1].red*c1 + p1[j].red*c2 + p1[j+1].red*c3 + p1[j+2].red*c4)
                           + c7*(p2[j-1].red*c1 + p2[j].red*c2 + p2[j+1].red*c3 + p2[j+2].red*c4)
                           + c8*(p3[j-1].red*c1 + p3[j].red*c2 + p3[j+1].red*c3 + p3[j+2].red*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].red:= Round(sum);

      sum := Round(c5*(p0[j-1].green*c1 + p0[j].green*c2 + p0[j+1].green*c3 + p0[j+2].green*c4)
                             + c6*(p1[j-1].green*c1 + p1[j].green*c2 + p1[j+1].green*c3 + p1[j+2].green*c4)
                             + c7*(p2[j-1].green*c1 + p2[j].green*c2 + p2[j+1].green*c3 + p2[j+2].green*c4)
                             + c8*(p3[j-1].green*c1 + p3[j].green*c2 + p3[j+1].green*c3 + p3[j+2].green*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].green:= Round(sum);

      sum := Round(c5*(p0[j-1].blue*c1 + p0[j].blue*c2 + p0[j+1].blue*c3 + p0[j+2].blue*c4)
                            + c6*(p1[j-1].blue*c1 + p1[j].blue*c2 + p1[j+1].blue*c3 + p1[j+2].blue*c4)
                            + c7*(p2[j-1].blue*c1 + p2[j].blue*c2 + p2[j+1].blue*c3 + p2[j+2].blue*c4)
                            + c8*(p3[j-1].blue*c1 + p3[j].blue*c2 + p3[j+1].blue*c3 + p3[j+2].blue*c4));

      if sum > 255 then sum := 255;
      if sum <0 then sum :=0;

      pOutput[k].blue:= Round(sum);

      pOutput[k].alpha:=255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;  }

procedure BGRAInvertColors(Input : TBGRABitmap);
var
   i, j : integer;
   p : PBGRAPixel;
begin
  for i := 0 to Input.Height-1 do
  begin
    p := Input.ScanLine[i];
    for j := 0 to Input.Width-1 do
    begin
      p[j].red:=255 - p[j].red;
      p[j].green:=255 - p[j].green;
      p[j].blue:=255 - p[j].blue;
      p[j].alpha:=255;
    end;
  end;
  Input.InvalidateBitmap; // changed by direct access
end;

procedure BGRARemap (Input : TBGRABitmap; Output : TBGRABitmap; width, height : Integer);
var
   i,j:integer;
   k,l:integer;
   p, pOutput : PBGRAPixel;
   g: integer;
   a, b: Integer;
   x0, y0, dx, dy: Integer;
   u, v: Integer;
begin

  for i := 0 to Output.Height - 1 do
  begin
    pOutput := Output.ScanLine[i];
    for j := 0 to Output.Width - 1 do
    begin
      pOutput[j].red := 255;
      pOutput[j].green := 255;
      pOutput[j].blue := 255;
    end;
  end;


  //Ellipsis width, height
  a := width;
  b := height;

  //Image center

  x0 := Input.Width div 2;
  y0 := Input.Height div 2;


  for l := 0 to Input.Height-1 do
  begin
    p := Input.ScanLine[l];
    dy := l - y0;

    //i:=l;
    //pOutput:=Output.ScanLine[l];
    //p := Input.ScanLine[i];
    for k := 0 to Input.Width-1 do
    begin

      dx := k - x0;

      u := dx;
      if (dy >=0 ) then
         v := Round ( dy + b * exp(-(dx * dx)/(a*a)) * (1 - abs(dy)/(y0)) )
      else
         v := Round ( dy - b * exp(-(dx * dx)/(a*a)) * (1 - abs(dy)/(y0)) );

      i := y0 + v;
      j := x0 + u;

      if (i >= 0) and (i < Output.Height) then
         pOutput := Output.ScanLine[i];

      if (j>=0) and (j < Output.Width) then
      begin
           pOutput[j].red := p[k].red;
           pOutput[j].green := p[k].green;
           pOutput[j].blue := p[k].blue;
      end;

      //g := (p[j].red + p[j].green + p[j].blue) div 3;
      g := 255;
      //pOutput[k].red := g;
      //pOutput[k].green := g;
      //pOutput[k].blue := g;
      //pOutput[k].alpha := 255;
    end;
  end;
  Output.InvalidateBitmap; // changed by direct access
end;

procedure BGRAUnsharp3(Input : TBGRABitmap; sigma, f: Single);
var
   i, j, j0, j1, j2, KernelWidth : integer;
   value: single;
   p0, p1, p2: PBGRAPixel;
   BlurRed, BlurGreen, BlurBlue, MaskRed, MaskGreen, MaskBlue : array of array of single;
   Kernel : ImageArray;
begin

  SetLength(MaskRed, Input.Height, Input.Width);
  SetLength(MaskGreen, Input.Height, Input.Width);
  SetLength(MaskBlue, Input.Height, Input.Width);
  SetLength(BlurRed, Input.Height, Input.Width);
  SetLength(BlurGreen, Input.Height, Input.Width);
  SetLength(BlurBlue, Input.Height, Input.Width);

  KernelWidth := 3;

  SetLength(Kernel, KernelWidth, KernelWidth);
  Gauss(sigma, KernelWidth, Kernel);

  for i := 0 to Input.Height - 1 do
  begin

    p1 := Input.ScanLine[i];

    if i > 0 then
       p0 := Input.ScanLine[i - 1]
    else
       p0 := Input.ScanLine[i];

    if i < Input.Height - 1 then
       p2 := Input.ScanLine[i + 1]
    else
       p2 := Input.ScanLine[i];


    for j := 0 to Input.Width - 1 do
    begin

      j1 := j;

      if j > 0 then
         j0 := j - 1
      else
         j0 := j;

      if j < Input.Width then
         j2 := j + 1
      else
         j2 := j;

      BlurRed[i][j] := Kernel[0][0] * p0[j0].red + Kernel[0][1] * p0[j1].red + Kernel[0][2] * p0[j2].red
                     + Kernel[1][0] * p1[j0].red + Kernel[1][1] * p1[j1].red + Kernel[1][2] * p1[j2].red
                     + Kernel[2][0] * p2[j0].red + Kernel[2][1] * p2[j1].red + Kernel[2][2] * p2[j2].red;


      BlurGreen[i][j] := Kernel[0][0] * p0[j0].green + Kernel[0][1] * p0[j1].green + Kernel[0][2] * p0[j2].green
                       + Kernel[1][0] * p1[j0].green + Kernel[1][1] * p1[j1].green + Kernel[1][2] * p1[j2].green
                       + Kernel[2][0] * p2[j0].green + Kernel[2][1] * p2[j1].green + Kernel[2][2] * p2[j2].green;



      BlurBlue[i][j] := Kernel[0][0] * p0[j0].blue + Kernel[0][1] * p0[j1].blue + Kernel[0][2] * p0[j2].blue
                      + Kernel[1][0] * p1[j0].blue + Kernel[1][1] * p1[j1].blue + Kernel[1][2] * p1[j2].blue
                      + Kernel[2][0] * p2[j0].blue + Kernel[2][1] * p2[j1].blue + Kernel[2][2] * p2[j2].blue;

      end;
  end;

  for i := 0 to Input.Height - 1 do
  begin

       p1 := Input.ScanLine[i];

       for j := 0 to Input.Width - 1 do
       begin
            MaskRed[i][j] := p1[j].red - BlurRed[i][j];
            MaskGreen[i][j] := p1[j].green - BlurGreen[i][j];
            MaskBlue[i][j] := p1[j].blue - BlurBlue[i][j];
       end;
  end;

  for i := 0 to Input.Height - 1 do
  begin
       //pOutput := Output.ScanLine[i];
       p1 := Input.ScanLine[i];

       for j := 0 to Input.Width - 1 do
       begin

         value := p1[j].red + f * MaskRed[i][j];
         if value < 0 then value := 0;
         if value > 255 then value := 255;

         //pOutput[j].red := Trunc(value);
         p1[j].red := Trunc(value);

         value := p1[j].green + f * MaskGreen[i][j];
         if value < 0 then value := 0;
         if value > 255 then value := 255;

         //pOutput[j].green := Trunc(value);
         p1[j].green := Trunc(value);

         value := p1[j].blue + f * MaskBlue[i][j];
         if value < 0 then value := 0;
         if value > 255 then value := 255;

         //pOutput[j].blue := Trunc(value);
         p1[j].blue := Trunc(value);
         p1[j].alpha := 255;
       end;
  end;


  Input.InvalidateBitmap; // changed by direct access
  MaskRed := nil;
  MaskGreen := nil;
  MaskBlue := nil;
  BlurRed := nil;
  BlurGreen := nil;
  BlurBlue := nil;
  Kernel := nil;
end;

procedure Gauss(sigma: Single; Width: Integer; var Kernel: ImageArray);
var
   Mean, Sum : Single;
   x, y: Integer;
begin
   Mean := Width div 2;
   Sum := 0.0;

   for x := 0 to Width - 1 do
   begin
     for y := 0 to Width - 1 do
     begin
       kernel[x][y] := Exp(  -0.5 * (Power((x-mean)/sigma, 2.0) + Power((y-mean)/sigma,2.0))  )
                       / (2 * pi * sigma * sigma );

       sum := sum + kernel[x][y];
     end;
   end;

   //writeln(sum);

   for x := 0 to Width - 1 do
   begin
     for y := 0 to Width - 1 do
     begin
       kernel[x][y] := kernel[x][y] / sum;
       //write(kernel[x][y]);
       //write(' ');
     end;
     //writeln;
   end;

end;

procedure BGRASetAlpha(Input : TBGRABitmap);
var
   i, j : integer;
   p : PBGRAPixel;
begin
  for i := 0 to Input.Height-1 do
  begin
    p := Input.ScanLine[i];
    for j := 0 to Input.Width-1 do
    begin
      p[j].alpha:=255;
    end;
  end;
  Input.InvalidateBitmap; // changed by direct access
end;

function c(x:single):single;  //this function implements the bicubic filter
begin
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=0.5*abs(x*x*x)-sqr(x)+2/3
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=-1/6*abs(x*x*x)+sqr(x)-2*x+4/3
    else Result:=0;
end;

function poly3(x:single):single;  //this function implements a different bicubic filter of panorama tools
var
  A : single;
begin
    A:=-0.75;
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=(A+2)*x*x*x - (A+3)*x*x + 1
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=A*x*x*x - 5*A*x*x + 8*A*x - 4*A
    else Result:=0;
end;

function catmullrom(x:single):single;  //this function implements catmull-rom bicubic filter
var
  A, B : single;
begin
    A:=1-abs(x);
    B:=2-abs(x);
    if (abs(x)>=0) and (abs(x)<=1) then
     Result:=0.5*(-3*A*A*A + 4*A*A + A)
    else if (abs(x)>1) and (abs(x)<=2) then
     Result:=0.5*(B*B*B-B*B)
    else Result:=0;
end;

function s(x:single):single;  //this function implements the quadratic filter
begin
    if (abs(x)>=0) and (abs(x)<=0.5) then
     Result:=-sqr(x)+0.75
    else if (abs(x)>0.5) and (abs(x)<=1.5) then
     Result:=0.5*sqr(x)-1.5*abs(x)+9/8
    else Result:=0;

end;

function hf(x:single):single;  //this function implements the hermite filter
begin
    if abs(x)<=1 then
       Result:=2*(abs(x)*abs(x)*abs(x))-3*(abs(x)*abs(x))+1
    else Result:=0;
end;

function Lancf(x,a:single):single; //this function implements the lanc filter
begin
    if abs(x)<=a then
    begin
      if x<>0 then Result := a*sin(Pi*x)*sin(Pi/a*x)/(Pi*Pi*x*x)
      else Result:=1;
    end
    else Result:=0;
end;

function sinc(x:single):single;     //the sinc function
begin
    if x<>0 then result:=sin(Pi*x)/(Pi*x)
    else Result:=1;
end;

function mf(x:single):single; //this function implements the mitchel filter;
begin
    if abs(x)<=1 then
       Result:=1/6*(7*abs(x)*abs(x)*abs(x)-12*x*x+16/3)
    else if (abs(x)>1) and (abs(x)<2) then
         Result:=1/6*(-7/3*abs(x*x*x)+12*x*x-20*abs(x)+32/3)
    else Result:=0;
end;

end.
