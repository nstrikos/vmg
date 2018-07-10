{
olddelphitrayicon.pas

This file implements a simple windows only TTrayIcon to be used on old Delphi
versions that don't come with a TTrayIcon pre-installed. It is a modifyed version
of the TTrayIcon from Lazarus, relicensed by the author (Felipe Monteiro de
Carvalho) to GPL for license uniformity in the magnifier code.

Copyright (C) 1998 - 2007 Harri Pyy, Chris O'Donnell, Felipe Monteiro de Carvalho

This file is part of Virtual Magnifying Glass.

Virtual Magnifying Glass is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Virtual Magnifying Glass is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating Virtual Magnifying Glass into proprietary
programs.

AUTHORS: Chris O'Donnell, Felipe Monteiro de Carvalho and Harri Pyy
}
unit olddelphitrayicon;

interface

uses                         
  SysUtils, Classes, Controls, Forms, Menus, Graphics, Windows, Messages, ShellAPI;

type

  { TCustomTrayIcon }

  TCustomTrayIcon = class(TComponent)
  private
    FPopUpMenu: TPopupMenu;
    FIcon: TIcon;
    FHint: string;
    FVisible, FShowIcon: Boolean;
    FOnPaint, FOnClick, FOnDblClick: TNotifyEvent;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    function GetCanvas: TCanvas;
    procedure SetVisible(Value: Boolean);
    class function InternalHide(const ATrayIcon: TCustomTrayIcon): Boolean;
    class function InternalShow(const ATrayIcon: TCustomTrayIcon): Boolean;
  public
    Handle: Cardinal;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Hide: Boolean;
    function Show: Boolean;
    procedure InternalUpdate;
    function GetPosition: TPoint;
    property Canvas: TCanvas read GetCanvas;
    property PopUpMenu: TPopupMenu read FPopUpMenu write FPopUpMenu;
    property Icon: TIcon read FIcon write FIcon;
    property Hint: string read FHint write FHint;
    property ShowIcon: Boolean read FShowIcon write FShowIcon;
    property Visible: Boolean read FVisible write SetVisible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  { TTrayIcon }
  
  TTrayIcon = class(TCustomTrayIcon)
  published
    property PopUpMenu;
    property Icon;
    property Hint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnPaint;
  end;

implementation

const
  szClassName = 'TTrayIconClass';
  szAppTitle = 'apptitle';
  uIDTrayIcon = 25;

var
  vwsTrayIcon: TCustomTrayIcon;

{*******************************************************************
*  TrayWndProc ()
*
*  DESCRIPTION:    Window procedure that processes messages for the
*                 systray icon
*
*  PARAMETERS:     Standard Mouse Messages have this parameters:
*
*                  fwKeys = wParam;        // key flags
*                  xPos = LOWORD(lParam);  // horizontal position of cursor
*                  yPos = HIWORD(lParam);  // vertical position of cursor
*                                          //* Those positions seam to be wrong
*                                          // Use Mouse.CursorPos instead
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
function TrayWndProc(Handle: HWND; iMsg: UINT; WParam_: WPARAM; LParam_:LPARAM):LRESULT; stdcall;
var
  pt: TPoint;
begin
  if iMsg = WM_USER + uIDTrayIcon then
  begin
     case LParam_ of
      WM_RBUTTONUP:
      begin
        if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
         mbRight, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
        if Assigned(vwsTrayIcon.PopUpMenu) then
        begin
          pt := Mouse.CursorPos;// Gets cursor position in screen coords

          // Apparently SetForegroundWindow and PostMessage are necessary
          // because we're invoking the shortcut menu from a notification icon
          // This is an attempt to prevent from messing with the Z-order
          SetForegroundWindow(Handle);
          PostMessage(Handle, WM_NULL, 0, 0);
          vwsTrayIcon.PopUpMenu.Popup(pt.x, pt.y);
        end;
      end;
      WM_RBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbRight, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_RBUTTONDBLCLK: if Assigned(vwsTrayIcon.OnDblClick) then vwsTrayIcon.OnDblClick(Application);

      WM_MBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbMiddle, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_MBUTTONUP: if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
       mbMiddle, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));

      WM_LBUTTONUP:
      begin
        if Assigned(vwsTrayIcon.OnMouseUp) then vwsTrayIcon.OnMouseUp(Application,
         mbLeft, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
        if Assigned(vwsTrayIcon.OnClick) then vwsTrayIcon.OnClick(Application);
      end;
      WM_LBUTTONDOWN: if Assigned(vwsTrayIcon.OnMouseDown) then vwsTrayIcon.OnMouseDown(Application,
       mbLeft, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
      WM_LBUTTONDBLCLK: if Assigned(vwsTrayIcon.OnDblClick) then vwsTrayIcon.OnDblClick(Application);

      WM_MOUSEMOVE: if Assigned(vwsTrayIcon.OnMouseMove) then
       vwsTrayIcon.OnMouseMove(Application, KeysToShiftState(WParam_), LOWORD(lParam_), HIWORD(lParam_));
     end;

     Result := 1;
     Exit;
  end;

  Result := DefWindowProc(Handle, iMsg, WParam_, LParam_);
end;

{*******************************************************************
*  TCustomTrayIcon.Create ()
*
*  DESCRIPTION:    Creates an object from the TCustomTrayIcon class
*
*  PARAMETERS:     TheOwner  - The owner of the component (this may be nil)
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TCustomTrayIcon.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  FIcon := TIcon.Create;

  FShowIcon := True;
end;

{*******************************************************************
*  TCustomTrayIcon.Destroy ()
*
*  DESCRIPTION:    Destroys an object derived from the TCustomTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TCustomTrayIcon.Destroy;
begin
  { Avoids an unremoved icon on the tray }
  Hide;

  FIcon.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TCustomTrayIcon.Hide: Boolean;
begin
  if not FVisible then Exit;

  FVisible := False;

//  InternalUpdate;

  Result := InternalHide(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TCustomTrayIcon.Show: Boolean;
begin
  if FVisible then Exit;

  FVisible := True;

  InternalUpdate;

  Result := InternalShow(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.SetVisible ()
*
*  DESCRIPTION:    Setter method of the Visible property
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
procedure TCustomTrayIcon.SetVisible(Value: Boolean);
begin
  if Value then Show
  else Hide;
end;

{*******************************************************************
*  TCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCustomTrayIcon.InternalUpdate;
begin
  // Nothing to be done
end;

{*******************************************************************
*  TCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TCustomTrayIcon.GetPosition: TPoint;
begin
  Result := Point(0, 0);
end;

{*******************************************************************
*  TCustomTrayIcon.GetCanvas ()
*
*  DESCRIPTION:    Getter method of the Canvas property
*
*  PARAMETERS:     None
*
*  RETURNS:        The canvas of the underlaying Widgetset component
*
*******************************************************************}
function TCustomTrayIcon.GetCanvas: TCanvas;
begin
  Result := nil;
end;

{*******************************************************************
*  TCustomTrayIcon.InternalHide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TCustomTrayIcon.InternalHide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  tnid: TNotifyIconData;
begin
  // Fill TNotifyIconData
  FillChar(tnid, SizeOf(tnid), 0);
  tnid.cbSize := SizeOf(TNotifyIconData);
  tnid.Wnd := ATrayIcon.Handle;
  tnid.uID := uIDTrayIcon;

  // Remove the icon
  Result := Shell_NotifyIconA(NIM_DELETE, @tnid);

  // Destroys the helper Windows
  PostMessage(ATrayIcon.Handle, WM_CLOSE, 0, 0);
  PostMessage(ATrayIcon.Handle, WM_DESTROY, 0, 0);

  Application.ProcessMessages;
end;

{*******************************************************************
*  TCustomTrayIcon.InternalShow ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TCustomTrayIcon.InternalShow(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  tnid: TNotifyIconData;
  buffer: PChar;
  Window: Windows.TWndClassEx;
begin
  vwsTrayIcon := ATrayIcon;

  ZeroMemory(@Window, SizeOf(TWndClassEx));
  Window.cbSize := SizeOf(TWndClassEx);
  Window.style := CS_OWNDC;
  Window.lpfnWndProc := @TrayWndProc;
  Window.cbClsExtra := 0;
  Window.cbWndExtra := 0;
  Window.hInstance := hInstance;
//  Window.hIcon := Icon.Handle;
  Window.hCursor := LoadCursor(0, IDC_ARROW);
  Window.hbrBackground := HBRUSH(GetStockObject(NULL_BRUSH));
  Window.lpszMenuName := nil;
  Window.lpszClassName := szClassName;
//  Window.hIconSm := hSmallIcon;

  Windows.RegisterClassEx(Window);

  ATrayIcon.Handle := CreateWindowEx(
        0,            //* Ensure that there will be no button in the bar */
        szClassName,        //* Name of the registered class */
        szAppTitle,         //* Title of the window */
        0,                  //* Style of the window */
        0,                  //* x-position (at beginning) */
        0,                  //* y-position (at beginning) */
        CW_USEDEFAULT,      //* window width */
        CW_USEDEFAULT,      //* window height */
        0,                  //* handle to parent or owner window */
        0,                  //* handle to menu */
        hInstance,          //* handle to application instance */
        nil);               //* pointer to window-creation data */

  // Fill TNotifyIconData
  FillChar(tnid, SizeOf(tnid), 0);
  tnid.cbSize := SizeOf(TNotifyIconData);
  tnid.Wnd := ATrayIcon.Handle;
  tnid.uID := uIDTrayIcon;
  tnid.uFlags := NIF_MESSAGE or NIF_ICON;
  if (ATrayIcon.Hint <> '') then tnid.uFlags := tnid.uFlags or NIF_TIP;
  tnid.uCallbackMessage := WM_USER + uIDTrayIcon;
  tnid.hIcon := ATrayIcon.Icon.Handle;
  buffer := PChar(ATrayIcon.Hint);
  StrCopy(@tnid.szTip, buffer);

  // Create Taskbar icon
  Result := Shell_NotifyIconA(NIM_ADD, @tnid);
end;

end.
