unit dockedglass;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LCLIntf, LCLType, BGRABitmap, BitmapProcess, dockedglasssettings, appsettings,
  math;
type

  { TDockedGlassWindow }

  TDockedGlassWindow = class(TForm)
    Image: TImage;
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HandleDockTimer(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     MouseIsDown: boolean;
     PX, PY: integer;
     w, h : integer;
     fm : single;
     bmpDisplay : TBitmap;
     srcBitmap, dstBitmap: TBGRABitmap;
     Timer : TTimer;
     mSettings : TDockedGlassSettings;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure GetBitmap;
     procedure SetSettings(Settings : TDockedGlassSettings);
  end;

var
  vDockedGlass: TDockedGlassWindow;

implementation

{ TDockedGlassWindow }

constructor TDockedGlassWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bmpDisplay := TBitmap.Create;
  FormStyle:=fsSystemStayOnTop;
  {$IFDEF Unix}
  ShowInTaskBar := stNever;
  {$ENDIF}
  fm := vConfigurations.iMagnification;
  Self.Width := 400;
  Self.Height := 400;
  w := Trunc(Self.Width / fm);
  h := Trunc(Self.Height / fm) ;
  srcBitmap := TBGRABitmap.Create(w, h);
  dstBitmap := TBGRABitmap.Create(Self.Width, Self.Height);
  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.Interval := 25;
  Timer.OnTimer := HandleDockTimer;
end;

destructor TDockedGlassWindow.Destroy;
begin
  bmpDisplay.Free;
  srcBitmap.Free;
  dstBitmap.Free;
  Timer.Free;
  inherited Destroy;
end;

procedure TDockedGlassWindow.HandleDockTimer(Sender: TObject);
begin
     GetBitmap;
end;

procedure TDockedGlassWindow.GetBitmap;
var
  ScreenDC: HDC;
  p : TPoint;
  p1, p2, EndX, EndY: integer;
  mouseX, mouseY : integer;
  {$IFDEF Unix}
  margin : integer;
  {$ENDIF}
begin

  if  not MouseIsDown then
  begin

      if ( fm <> vConfigurations.iMagnification) then
      begin
      fm := vConfigurations.iMagnification;
      Image.Height := Self.Height;
       Image.Width := Self.Width;
       w := Ceil(Self.Width / fm);
       h := Ceil(Self.Height / fm) ;
       if srcBitmap <> nil then
       begin
           srcBitmap.Free;
           srcBitmap := TBGRABitmap.Create(w, h);
       end;
       if dstBitmap <> nil then
       begin
           dstBitmap.Free;
           dstBitmap := TBGRABitmap.Create(Self.Width, Self.Height);
       end;
      end;



      ScreenDC := GetDC(0);

       {$ifdef Darwin}
       bmpRetinaDisplay := TBitmap.Create;
       bmpRetinaDisplay.LoadFromDevice(ScreenDC);
       bmpDisplay.Width := CXScreen;
       bmpDisplay.Height := CYScreen;
       StretchBlt(bmpDisplay.Canvas.Handle, 0, 0, CXScreen, CYScreen,
                  bmpRetinaDisplay.Canvas.Handle, 0, 0, bmpRetinaDisplay.Width, bmpRetinaDisplay.Height,
                  SRCCOPY);
       bmpRetinaDisplay.Free;
       {$else}
       bmpDisplay.LoadFromDevice(ScreenDC);
       {$endif}


       GetCursorPos(p);

       {if     (p.x > Self.Left)
          AND (p.x < Self.Left + Self.Width)
          AND (p.y > Self.Top)
          AND (p.y < Self.Top + Self.H) then
          Exit;}

       p1 := p.x - w div 2;
       p2 := p.y - h div 2;

       mouseX := Image.Width div 2;
       mouseY := Image.Height div 2;

       if p1 < 0 then
       begin
          mouseX := mouseX + Round(p1 * fm); //p1 is negative so we add it, not subtract it
          p1 := 0;
       end;
       if p2 < 0 then
       begin
           mouseY := mouseY + Round(p2 * fm);
           p2 := 0;
       end;

       EndX := p1 + w;
       EndY := p2 + h;

       if p1 + w > Screen.Width then
       begin
           mouseX := mouseX + Round( (p1 + w - Screen.Width) * fm);
           p1 := Screen.Width - w;
       end;


       if p2 + h > Screen.Height then
       begin
           mouseY := mouseY + Round( (p2 + h - Screen.Height) * fm );
           p2 := Screen.Height - h;
       end;


       srcBitmap.Canvas.CopyRect(Bounds(0, 0, w, h),
                                        bmpDisplay.Canvas,
                                        //myRect);
                                        Bounds(p1, p2, w, h));
       BGRAUnsharp3(srcBitmap, 3.0, 0.5);
       BGRABicubicCatmullRom(srcBitmap, fm, dstBitmap);
       Image.Picture.Bitmap.Assign(dstBitmap);

       {$IFDEF Unix}
       //Image.Picture.Bitmap.Assign(dstBitmap);

       Image.Canvas.Pen.Color:= clBlack;
       for margin := 0 to 4 do
       begin
          Image.Canvas.MoveTo(0 + margin, 0 + margin);
          Image.Canvas.LineTo(Image.Width - margin, 0 + margin);
          Image.Canvas.MoveTo(0 + margin, 0 + margin);
          Image.Canvas.LineTo(0 + margin, 0 + Image.Height - margin);
          Image.Canvas.MoveTo(Image.Width - margin, 0 + margin);
          Image.Canvas.LineTo(0 + Image.Width - margin, 0 + Image.Height - margin);
          Image.Canvas.MoveTo(0 + margin, 0 + Image.Height - margin);
          Image.Canvas.LineTo(0 + Image.Width - margin, 0 + Image.Height - margin);
       end;

       Image.Canvas.Pen.Width := 4;
       Image.Canvas.Pen.Color:= clRed;
       Image.Canvas.MoveTo(mouseX, mouseY - 50);
       Image.Canvas.LineTo(mouseX, mouseY + 50);
       Image.Canvas.MoveTo(mouseX - 50, mouseY);
       Image.Canvas.LineTo(mouseX + 50, mouseY);
       {$ELSE}
       dstBitmap.Draw(Image.Canvas, 0, 0, True);
       Image.Canvas.Brush.Color := clBlack;
       Image.Canvas.FrameRect(Bounds(0, 0, Image.Width, Image.Height));
       Image.Canvas.FrameRect(Bounds(0 + 1, 0 + 1, Image.Width - 2, Image.Height - 2));
       Image.Canvas.Brush.Color := clGray;
       Image.Canvas.FrameRect(Bounds(0 + 2, 0 + 2, Image.Width - 4, Image.Height - 4));
       Image.Canvas.FrameRect(Bounds(0 + 3, 0 + 3, Image.Width - 6, Image.Height - 6));
       {$ENDIF}
       ReleaseDC(0, ScreenDC);
  end;
end;

procedure TDockedGlassWindow.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if Button = mbLeft then begin
       MouseIsDown := True;
       PX := X;
       PY := Y;
    end
    else if Button = mbRight then
    begin
       Hide;
    end;
end;

procedure TDockedGlassWindow.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TDockedGlassWindow.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TDockedGlassWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sizeChanged : Boolean;
begin
  sizeChanged := False;
   case Key of
   VK_DOWN:
   begin
        sizeChanged := True;
        Self.Height := Self.Height + 10;
        if Self.Height > Screen.Height then
           Self.Height := Screen.Height;
   end;
   VK_UP:
   begin
        sizeChanged := True;
        Self.Height := Self.Height - 10;
        if Self.Height < 100 then
           Self.Height := 100;
   end;
   VK_RIGHT:
   begin
        sizeChanged := True;
        Self.Width := Self.Width + 10;
        if Self.Width > Screen.Width then
           Self.Width := Screen.Width;
   end;
   VK_Left:
   begin
        sizeChanged := True;
        Self.Width := Self.Width - 10;
        if Self.Width < 100 then
           Self.Width := 100;
   end;
   VK_ADD:
   begin
     sizeChanged := True;
     vConfigurations.iMagnification := vConfigurations.iMagnification + 0.5;
     if vConfigurations.iMagnification > 16 then vConfigurations.iMagnification := 16;
     fm := vConfigurations.iMagnification;
   end;
   VK_SUBTRACT:
   begin
     sizeChanged := True;
     vConfigurations.iMagnification := vConfigurations.iMagnification - 0.5;
     if vConfigurations.iMagnification < 1.0 then vConfigurations.iMagnification := 1.0;
     fm := vConfigurations.iMagnification;
   end;
   else
       Exit;
   end;

   if sizeChanged = True then
   begin
       Image.Height := Self.Height;
       Image.Width := Self.Width;
       w := Ceil(Self.Width / fm);
       h := Ceil(Self.Height / fm) ;
       if srcBitmap <> nil then
       begin
           srcBitmap.Free;
           srcBitmap := TBGRABitmap.Create(w, h);
       end;
       if dstBitmap <> nil then
       begin
           dstBitmap.Free;
           dstBitmap := TBGRABitmap.Create(Self.Width, Self.Height);
       end;
       vConfigurations.SaveDockedGlassSettings(Left, Top, Width, Height);
   end;
end;

procedure TDockedGlassWindow.ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
     if MouseIsDown then
     begin
        Left := Left + (X - PX);
        Top := Top + (Y - PY);
     end;
end;

procedure TDockedGlassWindow.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := False;
   mSettings.SetX(Left);
   mSettings.SetY(Top);
   mSettings.SetWidth(Width);
   mSettings.SetHeight(Height);
   vConfigurations.SaveDockedGlassSettings(Left, Top, Width, Height);
end;

procedure TDockedGlassWindow.ImageMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  i, delta: Integer;
begin
  delta := wheeldelta div 120;

  for i := 1 to abs(delta) do
  begin
    // Call same code as from VK_ADD, VK_SUBTRACT handlers
    if (WheelDelta > 0.0) then
     vConfigurations.iMagnification := vConfigurations.iMagnification + 0.5
    else vConfigurations.iMagnification := vConfigurations.iMagnification - 0.5;
  end;

  // Hard limits for the magnification
  if vConfigurations.iMagnification < 1.0 then vConfigurations.iMagnification := 1.0
  else if vConfigurations.iMagnification > 16 then vConfigurations.iMagnification := 16;

  fm := vConfigurations.iMagnification;

  Image.Height := Self.Height;
  Image.Width := Self.Width;
  w := Ceil(Self.Width / fm);
  h := Ceil(Self.Height / fm) ;
  if srcBitmap <> nil then
  begin
       srcBitmap.Free;
       srcBitmap := TBGRABitmap.Create(w, h);
  end;
  if dstBitmap <> nil then
  begin
       dstBitmap.Free;
       dstBitmap := TBGRABitmap.Create(Self.Width, Self.Height);
  end;
end;

procedure TDockedGlassWindow.SetSettings(Settings : TDockedGlassSettings);
begin
   mSettings := Settings;
   Left := mSettings.GetX;
   Top := mSettings.GetY;
   Width := mSettings.GetWidth;
   Height := mSettings.GetHeight;
   Image.Height := Self.Height;
   Image.Width := Self.Width;
   fm := mSettings.GetFm;
   w := ceil(Self.Width / fm);
   h := ceil(Self.Height / fm) ;
   if srcBitmap <> nil then
   begin
       srcBitmap.Free;
       srcBitmap := TBGRABitmap.Create(w, h);
   end;
   if dstBitmap <> nil then
   begin
       dstBitmap.Free;
       dstBitmap := TBGRABitmap.Create(Self.Width, Self.Height);
   end;
end;

initialization
  {$I dockedglass.lrs}

end.

