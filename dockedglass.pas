unit dockedglass;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LCLIntf, LCLType, Menus, BGRABitmap, BitmapProcess,
  dockedglasssettings, appsettings, math;
type

  { TDockedGlassWindow }

  TDockedGlassWindow = class(TForm)
  	Image: TImage;
    IncWidthMenuItem: TMenuItem;
    DecWidthMenuItem: TMenuItem;
    IncHeightMenuItem: TMenuItem;
    DecHeightMenuItem: TMenuItem;
    HideMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    ZoomInMenuItem: TMenuItem;
    ZoomOutMenuItem: TMenuItem;
    PopupMenu: TPopupMenu;
    procedure DecHeightMenuItemClick(Sender: TObject);
    procedure DecWidthMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HandleDockTimer(Sender: TObject);
    procedure HideMenuItemClick(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IncHeightMenuItemClick(Sender: TObject);
    procedure IncWidthMenuItemClick(Sender: TObject);
    procedure ZoomInMenuItemClick(Sender: TObject);
    procedure ZoomOutMenuItemClick(Sender: TObject);
  private
     MouseIsDown: boolean;  //User holds mouse down to drag the form around
     FormX, FormY: integer; //form coordinates - we use it for dragging the form around
     PX, PY : integer; //Coordinates of the area that should be enlarged
     CursorX, CursorY : integer; //global mouse coordinates around the screen - we use it to know where the mouse is
     mouseDrawX, mouseDrawY : integer; //coordinates of mouse position inside the form - we use it to draw the mouse;
     w, h : integer; //Width and height of the area that will be enlarged
     fm : single;
     bmpDisplay : TBitmap;
     srcBitmap, dstBitmap: TBGRABitmap;
     Timer : TTimer;
     mSettings : TDockedGlassSettings;
     SaveCursor : TCursor;
     procedure IncWidth;
     procedure DecWidth;
     procedure IncHeight;
     procedure DecHeight;
     procedure ZoomIn;
     procedure ZoomOut;
     procedure SizeChanged;
     procedure DrawBitmap;
     procedure GetBitmap;
     function CheckBounds(x, y : integer) : boolean;
     procedure CheckFm;
     procedure LoadBitmap(ScreenDC: HDC);
     procedure GetMouseCoords;
     procedure DrawBorder;
     procedure DrawMouse;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
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

procedure TDockedGlassWindow.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TDockedGlassWindow.SetSettings(Settings : TDockedGlassSettings);
begin
   mSettings := Settings;
   Left := mSettings.GetX;
   Top := mSettings.GetY;
   Width := mSettings.GetWidth;
   Height := mSettings.GetHeight;

   SizeChanged;
end;

procedure TDockedGlassWindow.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;
end;

procedure TDockedGlassWindow.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if Button = mbLeft then begin
    	MouseIsDown := True;
       	FormX := X;
       	FormY := Y;
    	SaveCursor := Image.Cursor;
        Image.Cursor := crHandPoint;
    end
end;

procedure TDockedGlassWindow.ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
     if MouseIsDown then
     begin
        Left := Left + (X - FormX);
        Top := Top + (Y - FormY);
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
    Image.Cursor := SaveCursor;
end;

procedure TDockedGlassWindow.HandleDockTimer(Sender: TObject);
begin
	DrawBitmap;
end;

procedure TDockedGlassWindow.DrawBitmap;
begin
	if  not MouseIsDown then
		GetBitmap;
end;

procedure TDockedGlassWindow.GetBitmap;
var
	ScreenDC: HDC;
	p : TPoint;
begin
	GetCursorPos(p);
    CursorX := p.x;
    CursorY := p.y;

	if CheckBounds(CursorX, CursorY) = True then
		Exit;

    CheckFm;
    ScreenDC := GetDC(0);
    LoadBitmap(ScreenDC);
    GetMouseCoords;

    srcBitmap.Canvas.CopyRect(Bounds(0, 0, w, h),
                              bmpDisplay.Canvas,
                              Bounds(PX, PY, w, h));
    BGRAUnsharp3(srcBitmap, 3.0, 0.5);
    BGRABicubicCatmullRom(srcBitmap, fm, dstBitmap);

  	{$IFDEF Unix}
   	Image.Picture.Bitmap.Assign(dstBitmap);
    {$ELSE}
    dstBitmap.Draw(Image.Canvas, 0, 0, True);
   	{$ENDIF}

    DrawBorder;
    DrawMouse;

   	ReleaseDC(0, ScreenDC);
end;

function TDockedGlassWindow.CheckBounds(x, y: integer) : boolean;
begin
	if  (x > Self.Left)
    AND (x < Self.Left + Self.Width)
    AND (y > Self.Top)
    AND (y < Self.Top + Self.Height) then
    	Result := True
    else
        Result := False;
end;

procedure TDockedGlassWindow.CheckFm;
begin
    //This check is necessary because the user may have changed
    //iMagnification from system tray
    if ( fm <> vConfigurations.iMagnification) then
    begin
    	fm := vConfigurations.iMagnification;
        SizeChanged;
	end;
end;

procedure TDockedGlassWindow.LoadBitmap(ScreenDC: HDC);
begin
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
end;

procedure TDockedGlassWindow.GetMouseCoords;
var
    offset, right, bottom : integer;

begin
	PX := CursorX - w div 2; //Holds top left x-coordinate of the area that should be magnified
    PY := CursorY - h div 2; //Holds top left y-coordinate of the area that should be magnified

	mouseDrawX := Image.Width div 2;  //Holds where mouse should be painted x-coordinate inside dock
	mouseDrawY := Image.Height div 2; //Holds where mouse should be painted y-coordinate inside dock
                                      //Normally mouseDrawX and mouseDrawY are in the middle of the form

	if PX < 0 then //If mouse goes to the left of the screen
	begin
    	offset := Round(PX * fm);
    	mouseDrawX := mouseDrawX + offset;  //If mouse goes to the left, we have to subtract offset
        									//So the mouse will be drawn to the left of the middle
        									//px is negative so we add it, not subtract it
        PX := 0;
   	end;

    if PY < 0 then //if mouse goes to the top of the screen
   	begin
    	offset := Round(PY * fm);
        mouseDrawY := mouseDrawY + offset;
        PY := 0;
	end;

    //Check right
    right := PX + w;
    offset := right - Screen.Width;
   	if offset > 0 then  //If mouse goes to the right of the screen
	begin
    	offset := Round(offset * fm);          //Scale offset
        mouseDrawX := mouseDrawX + offset;     //So the mouse will be draw to the right of the middle
        PX := Screen.Width - w;   			   //This is as far we can go to the right
	end;

    //Check bottom
    bottom := PY + h;
    offset := bottom - Screen.Height;
   	if offset > 0 then
	begin
        offset := Round( offset * fm);
        mouseDrawY := mouseDrawY + offset;
    	PY := Screen.Height - h;               //This is as far we can go to the bottom
   	end;
end;

procedure TDockedGlassWindow.DrawBorder;
{$IFDEF Unix}
var
	margin : integer;
{$ENDIF}
begin
 	{$IFDEF Unix}
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
   	{$ELSE}

	Image.Canvas.Brush.Color := clBlack;
  	Image.Canvas.FrameRect(Bounds(0, 0, Image.Width, Image.Height));
  	Image.Canvas.FrameRect(Bounds(0 + 1, 0 + 1, Image.Width - 2, Image.Height - 2));
  	Image.Canvas.Brush.Color := clGray;
  	Image.Canvas.FrameRect(Bounds(0 + 2, 0 + 2, Image.Width - 4, Image.Height - 4));
  	Image.Canvas.FrameRect(Bounds(0 + 3, 0 + 3, Image.Width - 6, Image.Height - 6));
  	{$ENDIF}
end;

procedure TDockedGlassWindow.DrawMouse;
begin
	Image.Canvas.Pen.Width := 4;
  	Image.Canvas.Pen.Color:= clRed;
  	Image.Canvas.MoveTo(mouseDrawX, mouseDrawY - 50);
  	Image.Canvas.LineTo(mouseDrawX, mouseDrawY + 50);
  	Image.Canvas.MoveTo(mouseDrawX - 50, mouseDrawY);
  	Image.Canvas.LineTo(mouseDrawX + 50, mouseDrawY);
end;

procedure TDockedGlassWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   case Key of
   VK_DOWN: IncHeight;
   VK_UP: DecHeight;
   VK_RIGHT: IncWidth;
   VK_LEFT: DecWidth;
   VK_ADD: ZoomIn;
   VK_SUBTRACT: ZoomOut;
   VK_ESCAPE: Hide;
   else
       Exit;
   end;
end;

procedure TDockedGlassWindow.IncWidthMenuItemClick(Sender: TObject);
begin
	IncWidth;
end;

procedure TDockedGlassWindow.DecWidthMenuItemClick(Sender: TObject);
begin
  DecWidth;
end;

procedure TDockedGlassWindow.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TDockedGlassWindow.DecHeightMenuItemClick(Sender: TObject);
begin
  DecHeight;
end;

procedure TDockedGlassWindow.IncHeightMenuItemClick(Sender: TObject);
begin
  IncHeight;
end;

procedure TDockedGlassWindow.ZoomInMenuItemClick(Sender: TObject);
begin
	ZoomIn;
end;

procedure TDockedGlassWindow.ZoomOutMenuItemClick(Sender: TObject);
begin
	ZoomOut;
end;

procedure TDockedGlassWindow.IncWidth;
begin
	Self.Width := Self.Width + 20;
	if Self.Width > Screen.Width then
    	Self.Width := Screen.Width;

    SizeChanged;
end;

procedure TDockedGlassWindow.DecWidth;
begin
	Self.Width := Self.Width - 20;
	if Self.Width < 100 then
		Self.Width := 100;

    SizeChanged;
end;

procedure TDockedGlassWindow.IncHeight;
begin
	Self.Height := Self.Height + 20;
	if Self.Height > Screen.Height then
    	Self.Height := Screen.Height;

    SizeChanged;
end;

procedure TDockedGlassWindow.DecHeight;
begin
	Self.Height := Self.Height - 20;
	if Self.Height < 100 then
		Self.Height := 100;

    SizeChanged;
end;

procedure TDockedGlassWindow.ZoomIn;
begin
	vConfigurations.iMagnification := vConfigurations.iMagnification + 0.5;
    if vConfigurations.iMagnification > 16 then
    	vConfigurations.iMagnification := 16;
    fm := vConfigurations.iMagnification;

    SizeChanged;
end;

procedure TDockedGlassWindow.ZoomOut;
begin
	vConfigurations.iMagnification := vConfigurations.iMagnification - 0.5;
    if vConfigurations.iMagnification < 1.0 then
    	vConfigurations.iMagnification := 1.0;
    fm := vConfigurations.iMagnification;

    SizeChanged;
end;

procedure TDockedGlassWindow.SizeChanged;
begin
	Image.Height := Self.Height;
    Image.Width := Self.Width;
    {$IFNDEF Unix}
    Image.Picture.Bitmap.SetSize(Image.Width, Image.Height);
    {$ENDIF}
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

procedure TDockedGlassWindow.HideMenuItemClick(Sender: TObject);
begin
  Hide;
end;

procedure TDockedGlassWindow.ImageDblClick(Sender: TObject);
begin
	Hide;
end;

initialization
  {$I dockedglass.lrs}

end.

